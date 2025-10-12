/*
 * websocket/server.rs
 *
 * Copyright (C) 2025 Lotas Inc. All rights reserved.
 *
 */

use std::collections::HashMap;
use std::sync::Arc;
use std::sync::Mutex as StdMutex;

use crossbeam::channel::Receiver;
use crossbeam::channel::Sender;
use futures_util::SinkExt;
use futures_util::StreamExt;
use futures_util::stream::SplitSink;
use tokio::net::TcpListener;
use tokio::net::TcpStream;
use tokio::sync::Mutex as TokioMutex;
use tokio_tungstenite::accept_async;
use tokio_tungstenite::tungstenite::Message as WsMessage;
use tokio_tungstenite::WebSocketStream;

use crate::comm::event::CommManagerEvent;
use crate::error::Error;
use crate::language::control_handler::ControlHandler;
use crate::language::shell_handler::ShellHandler;
use crate::session::Session;
use crate::socket::iopub::IOPubMessage;
use crate::socket::stdin::StdInRequest;
use crate::wire::input_reply::InputReply;

pub type WsConnection = WebSocketStream<TcpStream>;
pub type WsWriter = SplitSink<WsConnection, WsMessage>;
pub type ClientId = usize;

pub struct WebSocketServer {
    port: u16,
    clients: Arc<TokioMutex<Vec<Arc<TokioMutex<WsWriter>>>>>,
}

impl WebSocketServer {
    pub fn new(port: u16) -> Self {
        Self {
            port,
            clients: Arc::new(TokioMutex::new(Vec::new())),
        }
    }

    pub async fn start(
        self,
        session: Session,
        shell_handler: Box<dyn ShellHandler>,
        control_handler: Arc<StdMutex<dyn ControlHandler>>,
        iopub_tx: Sender<IOPubMessage>,
        mut iopub_rx: Receiver<IOPubMessage>,
        comm_manager_tx: Sender<CommManagerEvent>,
        stdin_request_rx: Receiver<StdInRequest>,
        stdin_reply_tx: Sender<crate::Result<InputReply>>,
    ) -> Result<(), Error> {
        let addr = format!("127.0.0.1:{}", self.port);
        let listener = TcpListener::bind(&addr)
            .await
            .map_err(|e| Error::Anyhow(anyhow::anyhow!("Failed to bind to {}: {}", addr, e)))?;

        log::info!("WebSocket server listening on {}", addr);

        let clients = self.clients.clone();
        let shell_handler_arc = Arc::new(TokioMutex::new(shell_handler));

        // IOPub broadcaster - runs in blocking thread and broadcasts to all clients
        let iopub_clients = clients.clone();
        let iopub_session = session.clone();
        std::thread::spawn(move || {
            log::debug!("IOPub broadcaster thread started");
            let rt = tokio::runtime::Runtime::new().unwrap();
            while let Ok(msg) = iopub_rx.recv() {
                if let Some(json) = crate::websocket::iopub::convert_iopub_to_json(&msg, &iopub_session) {
                    let clients_clone = iopub_clients.clone();
                    let json_clone = json.clone();
                    rt.block_on(async move {
                        let mut clients_guard = clients_clone.lock().await;
                        let mut disconnected = Vec::new();
                        
                        for (i, client_ws) in clients_guard.iter().enumerate() {
                            let mut ws = client_ws.lock().await;
                            if let Err(e) = ws.send(WsMessage::Text(json_clone.clone())).await {
                                log::error!("Failed to send IOPub message to client {}: {}", i, e);
                                disconnected.push(i);
                            }
                        }
                        
                        // Remove disconnected clients (in reverse order to preserve indices)
                        for &i in disconnected.iter().rev() {
                            clients_guard.remove(i);
                            log::debug!("Removed disconnected client {}", i);
                        }
                    });
                }
            }
            log::debug!("IOPub broadcaster thread ended");
        });

        // StdIn broadcaster - runs in blocking thread and broadcasts input requests to all clients
        let stdin_clients = clients.clone();
        let stdin_session = session.clone();
        std::thread::spawn(move || {
            let rt = tokio::runtime::Runtime::new().unwrap();
            while let Ok(request) = stdin_request_rx.recv() {
                if let Some(json) = crate::websocket::stdin::convert_stdin_request_to_json(&request, &stdin_session) {
                    let clients_clone = stdin_clients.clone();
                    let json_clone = json.clone();
                    rt.block_on(async move {
                        let clients_guard = clients_clone.lock().await;
                        let mut disconnected = Vec::new();
                        
                        for (i, client_ws) in clients_guard.iter().enumerate() {
                            let mut ws = client_ws.lock().await;
                            if let Err(e) = ws.send(WsMessage::Text(json_clone.clone())).await {
                                log::error!("Failed to send stdin message to client {}: {}", i, e);
                                disconnected.push(i);
                            }
                        }
                        
                        // Remove disconnected clients (in reverse order to preserve indices)
                        for &i in disconnected.iter().rev() {
                            let mut clients_guard = clients_clone.lock().await;
                            clients_guard.remove(i);
                        }
                    });
                }
            }
        });

        while let Ok((stream, addr)) = listener.accept().await {
            log::info!("New WebSocket connection from {}", addr);

            match accept_async(stream).await {
                Ok(ws_stream) => {
                    // Split WebSocket into read and write halves
                    let (write_half, read_half) = ws_stream.split();
                    
                    // Write half goes to clients list for IOPub broadcasting
                    let write_arc = Arc::new(TokioMutex::new(write_half));
                    clients.lock().await.push(write_arc.clone());

                    let session_clone = session.clone();
                    let shell_handler_clone = shell_handler_arc.clone();
                    let control_handler_clone = control_handler.clone();
                    let iopub_tx_clone = iopub_tx.clone();
                    let comm_manager_tx_clone = comm_manager_tx.clone();
                    let stdin_reply_tx_clone = stdin_reply_tx.clone();
                    let clients_clone = clients.clone();
                    let write_arc_for_removal = write_arc.clone();

                    tokio::spawn(async move {
                        if let Err(e) = crate::websocket::session::handle_session(
                            read_half,
                            write_arc,
                            session_clone,
                            shell_handler_clone,
                            control_handler_clone,
                            iopub_tx_clone,
                            comm_manager_tx_clone,
                            stdin_reply_tx_clone,
                        )
                        .await
                        {
                            log::error!("WebSocket session error: {}", e);
                        }

                        // Remove client on disconnect
                        let mut clients_guard = clients_clone.lock().await;
                        if let Some(pos) = clients_guard.iter().position(|c| Arc::ptr_eq(c, &write_arc_for_removal)) {
                            clients_guard.remove(pos);
                            log::info!("Client removed from clients list");
                        }
                    });
                },
                Err(e) => {
                    log::error!("Error during WebSocket handshake: {}", e);
                },
            }
        }
        
        Ok(())
    }
}
