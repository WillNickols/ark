/*
 * kernel.rs
 *
 * Copyright (C) 2022 Posit Software, PBC. All rights reserved.
 *
 */

use std::sync::Arc;
use std::sync::Mutex;

use crossbeam::channel::Receiver;
use crossbeam::channel::Sender;
use stdext::spawn;

use crate::comm::comm_manager::CommManager;
use crate::comm::event::CommManagerEvent;
use crate::error::Error;
use crate::language::control_handler::ControlHandler;
use crate::language::server_handler::ServerHandler;
use crate::language::shell_handler::ShellHandler;
use crate::session::Session;
use crate::socket::iopub::IOPubMessage;
use crate::socket::stdin::StdInRequest;
use crate::stream_capture::StreamCapture;
use crate::wire::input_reply::InputReply;

/// Possible behaviors for the stream capture thread. When set to `Capture`,
/// the stream capture thread will capture all output to stdout and stderr.
/// When set to `None`, no stream output is captured.
#[derive(PartialEq)]
pub enum StreamBehavior {
    Capture,
    None,
}

/// Starts the output capture thread.
fn output_capture_thread(iopub_tx: Sender<IOPubMessage>) -> Result<(), Error> {
    let output_capture = StreamCapture::new(iopub_tx);
    output_capture.listen();
    Ok(())
}

/// Connects the Kernel to the frontend using WebSocket transport
pub fn connect_websocket(
    name: &str,
    port: u16,
    shell_handler: Box<dyn ShellHandler>,
    control_handler: Arc<Mutex<dyn ControlHandler>>,
    _lsp_handler: Option<Arc<Mutex<dyn ServerHandler>>>,
    _dap_handler: Option<Arc<Mutex<dyn ServerHandler>>>,
    stream_behavior: StreamBehavior,
    iopub_tx: Sender<IOPubMessage>,
    iopub_rx: Receiver<IOPubMessage>,
    comm_manager_tx: Sender<CommManagerEvent>,
    comm_manager_rx: Receiver<CommManagerEvent>,
    stdin_request_rx: Receiver<StdInRequest>,
    stdin_reply_tx: Sender<crate::Result<InputReply>>,
) -> Result<(), Error> {
    log::info!("Starting kernel with WebSocket transport on port {}", port);

    // CRITICAL FIX: Unblock SIGINT to allow both thread creation AND interrupts
    #[cfg(unix)]
    {
        use nix::sys::signal::*;
        let mut sigset = SigSet::empty();
        sigset.add(SIGINT);
        sigprocmask(SigmaskHow::SIG_UNBLOCK, Some(&sigset), None)
            .map_err(|e| Error::Anyhow(anyhow::anyhow!("Failed to unblock signals: {}", e)))?;
    }

    let session = Session::create("")?;

    CommManager::start(iopub_tx.clone(), comm_manager_rx);

    if stream_behavior == StreamBehavior::Capture {
        let iopub_tx_clone = iopub_tx.clone();
        spawn!(format!("{name}-output-capture"), move || {
            output_capture_thread(iopub_tx_clone)
        });
    }

    let rt = tokio::runtime::Builder::new_current_thread()
        .enable_all()
        .build()
        .map_err(|e| Error::Anyhow(anyhow::anyhow!("Failed to create Tokio runtime: {}", e)))?;

    spawn!(format!("{name}-websocket-server"), move || {
        rt.block_on(async move {
            let server = crate::websocket::server::WebSocketServer::new(port);

            if let Err(e) = server
                .start(
                    session,
                    shell_handler,
                    control_handler,
                    iopub_tx,
                    iopub_rx,
                    comm_manager_tx,
                    stdin_request_rx,
                    stdin_reply_tx,
                )
                .await
            {
                log::error!("WebSocket server error: {:?}", e);
            }
        })
    });

    std::thread::sleep(std::time::Duration::from_millis(100));

    Ok(())
}
