/*
 * websocket/iopub.rs
 *
 * Copyright (C) 2025 Lotas Inc. All rights reserved.
 *
 */

use std::collections::HashMap;
use std::sync::Arc;

use crossbeam::channel::Receiver;
use tokio::sync::Mutex as TokioMutex;

use crate::session::Session;
use crate::socket::iopub::IOPubMessage;
use crate::wire::header::JupyterHeader;
use crate::wire::jupyter_message::JupyterMessage;
use crate::wire::wire_message::WireMessage;

use super::server::ClientId;

pub fn convert_iopub_to_json(msg: &IOPubMessage, session: &Session) -> Option<String> {
    match msg {
        IOPubMessage::Status(parent, _channel, status) => {
            let jupyter_msg = JupyterMessage {
                header: JupyterHeader {
                    msg_id: uuid::Uuid::new_v4().to_string(),
                    msg_type: "status".to_string(),
                    session: parent.session.clone(),
                    username: parent.username.clone(),
                    date: chrono::Utc::now().to_rfc3339(),
                    version: parent.version.clone(),
                },
                parent_header: Some(parent.clone()),
                content: status.clone(),
            };
            WireMessage::try_from(&jupyter_msg)
                .ok()
                .and_then(|wm| serde_json::to_string(&wm).ok())
        },
        IOPubMessage::ExecuteResult(result) => {
            let jupyter_msg = JupyterMessage::create(result.clone(), None, session);
            WireMessage::try_from(&jupyter_msg)
                .ok()
                .and_then(|wm| serde_json::to_string(&wm).ok())
        },
        IOPubMessage::ExecuteError(error) => {
            let jupyter_msg = JupyterMessage::create(error.clone(), None, session);
            WireMessage::try_from(&jupyter_msg)
                .ok()
                .and_then(|wm| serde_json::to_string(&wm).ok())
        },
        IOPubMessage::ExecuteInput(input) => {
            let jupyter_msg = JupyterMessage::create(input.clone(), None, session);
            WireMessage::try_from(&jupyter_msg)
                .ok()
                .and_then(|wm| serde_json::to_string(&wm).ok())
        },
        IOPubMessage::Stream(stream) => {
            let jupyter_msg = JupyterMessage::create(stream.clone(), None, session);
            WireMessage::try_from(&jupyter_msg)
                .ok()
                .and_then(|wm| serde_json::to_string(&wm).ok())
        },
        IOPubMessage::CommOpen(comm_open) => {
            let jupyter_msg = JupyterMessage::create(comm_open.clone(), None, session);
            WireMessage::try_from(&jupyter_msg)
                .ok()
                .and_then(|wm| serde_json::to_string(&wm).ok())
        },
        IOPubMessage::CommMsgReply(parent, comm_msg) => {
            let jupyter_msg = JupyterMessage::create(comm_msg.clone(), Some(parent.clone()), session);
            WireMessage::try_from(&jupyter_msg)
                .ok()
                .and_then(|wm| serde_json::to_string(&wm).ok())
        },
        IOPubMessage::CommMsgEvent(comm_msg) => {
            let jupyter_msg = JupyterMessage::create(comm_msg.clone(), None, session);
            WireMessage::try_from(&jupyter_msg)
                .ok()
                .and_then(|wm| serde_json::to_string(&wm).ok())
        },
        IOPubMessage::CommClose(comm_close) => {
            let jupyter_msg = JupyterMessage::create(comm_close.clone(), None, session);
            WireMessage::try_from(&jupyter_msg)
                .ok()
                .and_then(|wm| serde_json::to_string(&wm).ok())
        },
        IOPubMessage::DisplayData(parent, display_data) => {
            let jupyter_msg = JupyterMessage::create(display_data.clone(), parent.clone(), session);
            WireMessage::try_from(&jupyter_msg)
                .ok()
                .and_then(|wm| serde_json::to_string(&wm).ok())
        },
        IOPubMessage::UpdateDisplayData(parent, update_display_data) => {
            let jupyter_msg = JupyterMessage::create(update_display_data.clone(), parent.clone(), session);
            WireMessage::try_from(&jupyter_msg)
                .ok()
                .and_then(|wm| serde_json::to_string(&wm).ok())
        },
        IOPubMessage::Wait(wait) => {
            wait.wait_tx.send(()).ok();
            None
        },
    }
}

pub async fn iopub_broadcaster(
    iopub_rx: Receiver<IOPubMessage>,
    clients: Arc<TokioMutex<HashMap<ClientId, tokio::sync::mpsc::UnboundedSender<String>>>>,
    session: Session,
) {
    log::info!("📡 IOPub broadcaster started");
    let mut msg_count = 0;
    loop {
        log::trace!("📡 IOPub: Waiting for message #{}", msg_count);
        match iopub_rx.recv() {
            Ok(msg) => {
                log::info!("📡 IOPub: Received message #{}: {:?}", msg_count, std::mem::discriminant(&msg));
                msg_count += 1;
                let json_opt: Option<String> = match &msg {
                    IOPubMessage::Status(parent, _channel, status) => {
                        let jupyter_msg = JupyterMessage {
                            header: JupyterHeader {
                                msg_id: uuid::Uuid::new_v4().to_string(),
                                msg_type: "status".to_string(),
                                session: parent.session.clone(),
                                username: parent.username.clone(),
                                date: chrono::Utc::now().to_rfc3339(),
                                version: parent.version.clone(),
                            },
                            parent_header: Some(parent.clone()),
                            content: status.clone(),
                        };
                        WireMessage::try_from(&jupyter_msg)
                            .ok()
                            .and_then(|wm| serde_json::to_string(&wm).ok())
                    },
                    IOPubMessage::ExecuteResult(result) => {
                        let jupyter_msg = JupyterMessage::create(result.clone(), None, &session);
                        WireMessage::try_from(&jupyter_msg)
                            .ok()
                            .and_then(|wm| serde_json::to_string(&wm).ok())
                    },
                    IOPubMessage::ExecuteError(error) => {
                        let jupyter_msg = JupyterMessage::create(error.clone(), None, &session);
                        WireMessage::try_from(&jupyter_msg)
                            .ok()
                            .and_then(|wm| serde_json::to_string(&wm).ok())
                    },
                    IOPubMessage::ExecuteInput(input) => {
                        let jupyter_msg = JupyterMessage::create(input.clone(), None, &session);
                        WireMessage::try_from(&jupyter_msg)
                            .ok()
                            .and_then(|wm| serde_json::to_string(&wm).ok())
                    },
                    IOPubMessage::Stream(stream) => {
                        let jupyter_msg = JupyterMessage::create(stream.clone(), None, &session);
                        WireMessage::try_from(&jupyter_msg)
                            .ok()
                            .and_then(|wm| serde_json::to_string(&wm).ok())
                    },
                    IOPubMessage::CommOpen(comm_open) => {
                        let jupyter_msg = JupyterMessage::create(comm_open.clone(), None, &session);
                        WireMessage::try_from(&jupyter_msg)
                            .ok()
                            .and_then(|wm| serde_json::to_string(&wm).ok())
                    },
                    IOPubMessage::CommMsgReply(parent, comm_msg) => {
                        let jupyter_msg = JupyterMessage::create(comm_msg.clone(), Some(parent.clone()), &session);
                        WireMessage::try_from(&jupyter_msg)
                            .ok()
                            .and_then(|wm| serde_json::to_string(&wm).ok())
                    },
                    IOPubMessage::CommMsgEvent(comm_msg) => {
                        let jupyter_msg = JupyterMessage::create(comm_msg.clone(), None, &session);
                        let result = WireMessage::try_from(&jupyter_msg)
                            .ok()
                            .and_then(|wm| serde_json::to_string(&wm).ok());
                        result
                    },
                    IOPubMessage::CommClose(comm_close) => {
                        let jupyter_msg = JupyterMessage::create(comm_close.clone(), None, &session);
                        WireMessage::try_from(&jupyter_msg)
                            .ok()
                            .and_then(|wm| serde_json::to_string(&wm).ok())
                    },
                    IOPubMessage::DisplayData(parent, display_data) => {
                        let jupyter_msg = JupyterMessage::create(display_data.clone(), parent.clone(), &session);
                        WireMessage::try_from(&jupyter_msg)
                            .ok()
                            .and_then(|wm| serde_json::to_string(&wm).ok())
                    },
                    IOPubMessage::UpdateDisplayData(parent, update_display_data) => {
                        let jupyter_msg = JupyterMessage::create(update_display_data.clone(), parent.clone(), &session);
                        WireMessage::try_from(&jupyter_msg)
                            .ok()
                            .and_then(|wm| serde_json::to_string(&wm).ok())
                    },
                    IOPubMessage::Wait(wait) => {
                        wait.wait_tx.send(()).ok();
                        None
                    },
                };

                if let Some(json) = json_opt {
                    log::info!("📡 [ARK IOPUB] Broadcasting {} bytes to clients", json.len());
                    log::info!("📡 [ARK IOPUB] Full IOPub message: {}", json);

                    let clients_guard = clients.lock().await;
                    let client_count = clients_guard.len();
                    log::info!("📡 [ARK IOPUB] Have {} connected clients", client_count);
                    
                    let mut disconnected = Vec::new();

                    for (client_id, client_tx) in clients_guard.iter() {
                        log::info!("📡 [ARK IOPUB] Sending to client {}", client_id);
                        if let Err(e) = client_tx.send(json.clone()) {
                            log::error!("📡 [ARK IOPUB] Failed to send to client {}: {}", client_id, e);
                            disconnected.push(*client_id);
                        } else {
                            log::info!("✅ [ARK IOPUB] Successfully queued message for client {}", client_id);
                        }
                    }

                    drop(clients_guard);

                    if !disconnected.is_empty() {
                        let mut clients_guard = clients.lock().await;
                        for client_id in disconnected {
                            clients_guard.remove(&client_id);
                            log::info!("Removed disconnected client {}", client_id);
                        }
                    }
                } else {
                    log::trace!("📡 IOPub: Message had no JSON representation");
                }
            },
            Err(e) => {
                log::error!("📡 IOPub: Failed to receive message: {:?}", e);
                break;
            },
        }
    }
    log::error!("📡 IOPub broadcaster ended");
}
