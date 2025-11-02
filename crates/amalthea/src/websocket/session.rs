use std::collections::HashMap;
use std::sync::Arc;
use std::sync::Mutex as StdMutex;

use anyhow;
use crossbeam::channel::Receiver;
use crossbeam::channel::Sender;
use futures_util::SinkExt;
use futures_util::StreamExt;
use futures_util::stream::SplitStream;
use tokio::net::TcpStream;
use tokio::sync::Mutex as TokioMutex;
use tokio_tungstenite::tungstenite::Message as WsMessage;
use tokio_tungstenite::WebSocketStream;

use crate::comm::comm_channel::Comm;
use crate::comm::comm_channel::CommMsg;
use crate::comm::event::CommManagerEvent;
use crate::error::Error;
use crate::language::control_handler::ControlHandler;
use crate::language::shell_handler::ShellHandler;
use crate::session::Session;
use crate::socket::comm::CommInitiator;
use crate::socket::comm::CommSocket;
use crate::socket::iopub::IOPubMessage;
use crate::wire::comm_close::CommClose;
use crate::wire::comm_msg::CommWireMsg;
use crate::wire::comm_open::CommOpen;
use crate::wire::jupyter_message::JupyterMessage;
use crate::wire::jupyter_message::ProtocolMessage;
use crate::wire::kernel_info_full_reply;
use crate::wire::originator::Originator;
use crate::wire::status::ExecutionState;
use crate::wire::status::KernelStatus;
use crate::wire::wire_message::WireMessage;
use crate::wire::input_reply::InputReply;

use super::server::ClientId;

pub type WsConnection = WebSocketStream<TcpStream>;
pub type WsReader = SplitStream<WsConnection>;
pub type WsWriter = super::server::WsWriter;

pub async fn handle_session(
    mut read_half: WsReader,
    write_half: Arc<TokioMutex<WsWriter>>,
    session: Session,
    shell_handler: Arc<TokioMutex<Box<dyn ShellHandler>>>,
    control_handler: Arc<StdMutex<dyn ControlHandler>>,
    iopub_tx: Sender<IOPubMessage>,
    backend_comms: super::server::BackendComms,
    stdin_reply_tx: Sender<crate::Result<InputReply>>,
) -> Result<(), Error> {
    log::info!("WebSocket client connected");

    // Track frontend-initiated comm channels for routing messages
    let open_comms: Arc<TokioMutex<HashMap<String, CommSocket>>> = Arc::new(TokioMutex::new(HashMap::new()));
    
    // Track pending RPC requests so relay threads can match responses and send with parent headers
    let pending_rpcs: Arc<TokioMutex<HashMap<String, (JupyterMessage<CommWireMsg>, Arc<TokioMutex<WsWriter>>)>>> = Arc::new(TokioMutex::new(HashMap::new()));
    
    // Track which backend comms already have relay threads spawned (to avoid duplicates)
    let backend_relays_spawned: Arc<TokioMutex<std::collections::HashSet<String>>> = Arc::new(TokioMutex::new(std::collections::HashSet::new()));

    // Read loop - no mutex needed, we own the read half
    while let Some(msg_result) = read_half.next().await {
        match msg_result {
            Ok(WsMessage::Text(text)) => {
                if let Err(e) = handle_message(&text, &session, &shell_handler, &control_handler, &iopub_tx, &backend_comms, &stdin_reply_tx, &write_half, &open_comms, &pending_rpcs, &backend_relays_spawned).await {
                    log::error!("Error handling message: {}", e);
                }
            },
            Ok(WsMessage::Close(_)) => {
                log::info!("Client closed connection");
                break;
            },
            Err(e) => {
                log::error!("WebSocket error: {}", e);
                break;
            },
            _ => {
                // Ignore other message types (Binary, Ping, Pong, Frame)
            }
        }
    }

    log::info!("WebSocket client disconnected");
    Ok(())
}

async fn handle_message(
    text: &str,
    session: &Session,
    shell_handler: &Arc<TokioMutex<Box<dyn ShellHandler>>>,
    control_handler: &Arc<StdMutex<dyn ControlHandler>>,
    iopub_tx: &Sender<IOPubMessage>,
    backend_comms: &super::server::BackendComms,
    stdin_reply_tx: &Sender<crate::Result<InputReply>>,
    write_half: &Arc<TokioMutex<WsWriter>>,
    open_comms: &Arc<TokioMutex<HashMap<String, CommSocket>>>,
    pending_rpcs: &Arc<TokioMutex<HashMap<String, (JupyterMessage<CommWireMsg>, Arc<TokioMutex<WsWriter>>)>>>,
    backend_relays_spawned: &Arc<TokioMutex<std::collections::HashSet<String>>>,
)-> Result<(), Error> {
    let mut wire_msg: WireMessage = serde_json::from_str(text)
        .map_err(|e| Error::Anyhow(anyhow::anyhow!("Failed to parse message: {}", e)))?;
    let msg_type = wire_msg.header.msg_type.as_str();

    // Some frontends send interrupt_request with null content; normalize to empty object
    if msg_type == "interrupt_request" && wire_msg.content.is_null() {
        wire_msg.content = serde_json::json!({});
    }
    match msg_type {
        "kernel_info_request" => {
            let req: JupyterMessage<crate::wire::kernel_info_request::KernelInfoRequest> =
                JupyterMessage::try_from(&wire_msg)?;
            
            send_status(&req, ExecutionState::Busy, iopub_tx)?;
            
            let reply = shell_handler.lock().await.handle_info_request(&req.content).await?;
            let reply = kernel_info_full_reply::KernelInfoReply::from(reply);
            
            send_reply(&req, reply, session, write_half).await?;
            
            send_status(&req, ExecutionState::Idle, iopub_tx)?;
        },
        "execute_request" => {
            let req: JupyterMessage<crate::wire::execute_request::ExecuteRequest> =
                JupyterMessage::try_from(&wire_msg)?;
            
            send_status(&req, ExecutionState::Busy, iopub_tx)?;
            
            // Spawn execution into background task so WebSocket loop can continue reading messages (like interrupt_request)
            let originator = Originator::from(&req);
            let shell_handler_clone = shell_handler.clone();
            let req_content = req.content.clone();
            let session_clone = session.clone();
            let write_clone = write_half.clone();
            let iopub_tx_clone = iopub_tx.clone();
            
            tokio::spawn(async move {
                let result = shell_handler_clone.lock().await.handle_execute_request(originator, &req_content).await;
                
                let reply_result = match result {
                    Ok(reply) => {
                        send_reply(&req, reply, &session_clone, &write_clone).await
                    },
                    Err(e) => {
                        // For ShellErrorExecuteReply errors, send error reply
                        match e {
                            Error::ShellErrorExecuteReply(exception, count) => {
                                use crate::wire::execute_reply_exception::ExecuteReplyException;
                                use crate::wire::jupyter_message::Status;
                                let error_reply = ExecuteReplyException {
                                    status: Status::Error,
                                    execution_count: count,
                                    exception: exception,
                                };
                                send_reply(&req, error_reply, &session_clone, &write_clone).await
                            },
                            other => {
                                log::error!("Execute request error: {:?}", other);
                                return;
                            }
                        }
                    }
                };
                
                if let Err(e) = reply_result {
                    log::error!("Failed to send execute reply: {:?}", e);
                }
                
                if let Err(e) = send_status(&req, ExecutionState::Idle, &iopub_tx_clone) {
                    log::error!("Failed to send idle status: {:?}", e);
                }
            });
        },
        "complete_request" => {
            let req: JupyterMessage<crate::wire::complete_request::CompleteRequest> =
                JupyterMessage::try_from(&wire_msg)?;
            
            // Spawn in background to avoid blocking the receive loop when R is waiting for stdin
            let shell_handler_clone = shell_handler.clone();
            let session_clone = session.clone();
            let write_half_clone = write_half.clone();
            tokio::spawn(async move {
                match shell_handler_clone.lock().await.handle_complete_request(&req.content).await {
                    Ok(reply) => {
                        if let Err(e) = send_reply(&req, reply, &session_clone, &write_half_clone).await {
                            log::error!("complete_request: failed to send reply: {}", e);
                        }
                    },
                    Err(e) => {
                        log::error!("complete_request: handler error: {}", e);
                    }
                }
            });
        },
        "inspect_request" => {
            let req: JupyterMessage<crate::wire::inspect_request::InspectRequest> =
                JupyterMessage::try_from(&wire_msg)?;
            
            // Spawn in background to avoid blocking the receive loop when R is waiting for stdin
            let shell_handler_clone = shell_handler.clone();
            let session_clone = session.clone();
            let write_half_clone = write_half.clone();
            tokio::spawn(async move {
                match shell_handler_clone.lock().await.handle_inspect_request(&req.content).await {
                    Ok(reply) => {
                        if let Err(e) = send_reply(&req, reply, &session_clone, &write_half_clone).await {
                            log::error!("inspect_request: failed to send reply: {}", e);
                        }
                    },
                    Err(e) => {
                        log::error!("inspect_request: handler error: {}", e);
                    }
                }
            });
        },
        "is_complete_request" => {
            let req: JupyterMessage<crate::wire::is_complete_request::IsCompleteRequest> =
                JupyterMessage::try_from(&wire_msg)?;
            
            // Spawn in background to avoid blocking when R is waiting for stdin
            let shell_handler_clone = shell_handler.clone();
            let session_clone = session.clone();
            let write_half_clone = write_half.clone();
            tokio::spawn(async move {
                match shell_handler_clone.lock().await.handle_is_complete_request(&req.content).await {
                    Ok(reply) => {
                        if let Err(e) = send_reply(&req, reply, &session_clone, &write_half_clone).await {
                            log::error!("is_complete_request: failed to send reply: {}", e);
                        }
                    },
                    Err(e) => {
                        log::error!("is_complete_request: handler error: {}", e);
                    }
                }
            });
        },
        "history_request" => {
            // History is not currently implemented; return empty history
            // For now, just log a warning and don't send a reply
            // TODO: Implement history tracking and proper history_reply
            log::warn!("history_request received but not fully implemented (returning empty)");
        },
        "comm_open" => {
            let req: JupyterMessage<CommOpen> = JupyterMessage::try_from(&wire_msg)?;
            let comm = req.content.target_name.parse::<Comm>()
                .unwrap_or_else(|_| Comm::Other(req.content.target_name.clone()));
            let comm_socket = CommSocket::new(CommInitiator::FrontEnd, req.content.comm_id.clone(), req.content.target_name.clone());
            let opened = shell_handler.lock().await.handle_comm_open(comm, comm_socket.clone()).await?;
            if opened {
                // Store the comm socket for routing future messages
                open_comms.lock().await.insert(req.content.comm_id.clone(), comm_socket.clone());
                
                let comm_id = req.content.comm_id.clone();
                let outgoing_rx = comm_socket.outgoing_rx.clone();
                let iopub_tx_clone = iopub_tx.clone();
                let pending_rpcs_clone = pending_rpcs.clone();
                let session_clone = session.clone();
                
                std::thread::spawn(move || {
                    loop {
                        match outgoing_rx.recv() {
                            Ok(msg) => {
                                match msg {
                                    CommMsg::Data(data) => {
                                        let comm_msg = CommWireMsg {
                                            comm_id: comm_id.clone(),
                                            data,
                                        };
                                        if let Err(_) = iopub_tx_clone.send(IOPubMessage::CommMsgEvent(comm_msg)) {
                                            break;
                                        }
                                    },
                                    CommMsg::Rpc(msg_id, data) => {
                                        // Check if this is a response to a pending RPC request
                                        let rt = tokio::runtime::Runtime::new().unwrap();
                                        let pending = rt.block_on(async {
                                            pending_rpcs_clone.lock().await.remove(&msg_id)
                                        });
                                        
                                        if let Some((req, write)) = pending {
                                            // Send RPC response with proper parent header
                                            let reply_msg = CommWireMsg {
                                                comm_id: comm_id.clone(),
                                                data,
                                            };
                                            rt.block_on(async {
                                                let _ = send_reply(&req, reply_msg, &session_clone, &write).await;
                                            });
                                        } else {
                                            // No pending request, just broadcast (shouldn't happen for UI comm)
                                            let comm_msg = CommWireMsg {
                                                comm_id: comm_id.clone(),
                                                data,
                                            };
                                            if let Err(_) = iopub_tx_clone.send(IOPubMessage::CommMsgEvent(comm_msg)) {
                                                break;
                                            }
                                        }
                                    },
                                    CommMsg::Close => {
                                        let comm_close = CommClose {
                                            comm_id: comm_id.clone(),
                                        };
                                        let _ = iopub_tx_clone.send(IOPubMessage::CommClose(comm_close));
                                        break;
                                    }
                                }
                            },
                            Err(_) => {
                                break;
                            }
                        }
                    }
                });
            }
        },
        "comm_msg" => {
            let req: JupyterMessage<CommWireMsg> = JupyterMessage::try_from(&wire_msg)?;
            
            // Check if this method should be handled by the generic shell handler
            // These methods are implemented in shell.rs handle_comm_message and should
            // not be routed to specialized comm handlers even if the comm is registered
            let method = req.content.data.get("method").and_then(|m| m.as_str()).unwrap_or("");
            let shell_methods = [
                "set_working_directory",
                "set_console_width",
                "list_packages",
                "install_package",
                "uninstall_package",
                "check_missing_packages"
            ];
            let use_shell_handler = shell_methods.contains(&method);
            
            // Check if this is a registered comm with its own handler
            // First check frontend-initiated comms, then backend-initiated comms
            let comms = open_comms.lock().await;
            let has_frontend_handler = comms.contains_key(&req.content.comm_id);
            
            if has_frontend_handler && !use_shell_handler {
                // Add this request to pending_rpcs so the relay thread can match the response
                pending_rpcs.lock().await.insert(req.header.msg_id.clone(), (req.clone(), write_half.clone()));
                
                // Route message to the comm's handler via its incoming channel
                let comm_socket = comms.get(&req.content.comm_id).unwrap();
                let msg = CommMsg::Rpc(req.header.msg_id.clone(), req.content.data.clone());
                let send_result = comm_socket.incoming_tx.send(msg);
                drop(comms); // Release the lock
                
                if let Err(e) = send_result {
                    // Remove from pending since we won't get a response
                    pending_rpcs.lock().await.remove(&req.header.msg_id);
                    
                    let error_data = serde_json::json!({
                        "jsonrpc": "2.0",
                        "id": req.content.data.get("id"),
                        "error": {
                            "code": -32603,
                            "message": format!("Comm handler not available: {}", e)
                        }
                    });
                    let reply_msg = crate::wire::comm_msg::CommWireMsg {
                        comm_id: req.content.comm_id.clone(),
                        data: error_data,
                    };
                    send_reply(&req, reply_msg, session, write_half).await?;
                }
            } else {
                drop(comms); // Release frontend comms lock
                
                // Check backend-initiated comms
                let backend_comms_guard = backend_comms.lock().await;
                let has_backend_handler = backend_comms_guard.contains_key(&req.content.comm_id);
                
                if has_backend_handler && !use_shell_handler {
                    let comm_socket = backend_comms_guard.get(&req.content.comm_id).unwrap().clone();
                    let comm_id = req.content.comm_id.clone();
                    drop(backend_comms_guard); // Release the lock early
                    
                    // Spawn relay thread for this backend comm if not already spawned
                    // This ensures RPC responses use send_reply with proper parent_header
                    let mut relays = backend_relays_spawned.lock().await;
                    if !relays.contains(&comm_id) {
                        relays.insert(comm_id.clone());
                        drop(relays);
                        
                        let outgoing_rx = comm_socket.outgoing_rx.clone();
                        let iopub_tx_clone = iopub_tx.clone();
                        let pending_rpcs_clone = pending_rpcs.clone();
                        let session_clone = session.clone();
                        
                        std::thread::spawn(move || {
                            loop {
                                match outgoing_rx.recv() {
                                    Ok(msg) => {
                                        match msg {
                                            CommMsg::Data(data) => {
                                                let comm_msg = CommWireMsg {
                                                    comm_id: comm_id.clone(),
                                                    data,
                                                };
                                                if let Err(_) = iopub_tx_clone.send(IOPubMessage::CommMsgEvent(comm_msg)) {
                                                    break;
                                                }
                                            },
                                            CommMsg::Rpc(msg_id, data) => {
                                                // Check if this is a response to a pending RPC request
                                                let rt = tokio::runtime::Runtime::new().unwrap();
                                                let pending = rt.block_on(async {
                                                    pending_rpcs_clone.lock().await.remove(&msg_id)
                                                });
                                                
                                                if let Some((req, write)) = pending {
                                                    // Send RPC response with proper parent header
                                                    let reply_msg = CommWireMsg {
                                                        comm_id: comm_id.clone(),
                                                        data,
                                                    };
                                                    rt.block_on(async {
                                                        let _ = send_reply(&req, reply_msg, &session_clone, &write).await;
                                                    });
                                                } else {
                                                    // No pending request, just broadcast
                                                    let comm_msg = CommWireMsg {
                                                        comm_id: comm_id.clone(),
                                                        data,
                                                    };
                                                    if let Err(_) = iopub_tx_clone.send(IOPubMessage::CommMsgEvent(comm_msg)) {
                                                        break;
                                                    }
                                                }
                                            },
                                            CommMsg::Close => {
                                                let comm_close = CommClose {
                                                    comm_id: comm_id.clone(),
                                                };
                                                let _ = iopub_tx_clone.send(IOPubMessage::CommClose(comm_close));
                                                break;
                                            }
                                        }
                                    },
                                    Err(_) => {
                                        break;
                                    }
                                }
                            }
                        });
                    } else {
                        drop(relays);
                    }
                    
                    // Add this request to pending_rpcs so the relay thread can match the response
                    pending_rpcs.lock().await.insert(req.header.msg_id.clone(), (req.clone(), write_half.clone()));
                    
                    let msg = CommMsg::Rpc(req.header.msg_id.clone(), req.content.data.clone());
                    let send_result = comm_socket.incoming_tx.send(msg);
                    
                    if let Err(e) = send_result {
                        // Remove from pending since we won't get a response
                        pending_rpcs.lock().await.remove(&req.header.msg_id);
                        
                        let error_data = serde_json::json!({
                            "jsonrpc": "2.0",
                            "id": req.content.data.get("id"),
                            "error": {
                                "code": -32603,
                                "message": format!("Backend comm handler not available: {}", e)
                            }
                        });
                        let reply_msg = crate::wire::comm_msg::CommWireMsg {
                            comm_id: req.content.comm_id.clone(),
                            data: error_data,
                        };
                        send_reply(&req, reply_msg, session, write_half).await?;
                    }
                } else {
                    drop(backend_comms_guard); // Release the lock before calling shell handler
                    
                    // No registered handler, use the shell's generic handler
                    let result = shell_handler.lock().await.handle_comm_message(&req.content.comm_id, &req.content.data).await;
                    
                    match result {
                        Ok(reply_data) => {
                            let reply_msg = crate::wire::comm_msg::CommWireMsg {
                                comm_id: req.content.comm_id.clone(),
                                data: reply_data,
                            };
                            send_reply(&req, reply_msg, session, write_half).await?;
                        },
                        Err(e) => {
                            log::error!("[WEBSOCKET SESSION] Error in shell handler: {}", e);
                            let error_data = serde_json::json!({
                                "jsonrpc": "2.0",
                                "id": req.content.data.get("id"),
                                "error": {
                                    "code": -32603,
                                    "message": format!("Internal error: {}", e)
                                }
                            });
                            let reply_msg = crate::wire::comm_msg::CommWireMsg {
                                comm_id: req.content.comm_id.clone(),
                                data: error_data,
                            };
                            send_reply(&req, reply_msg, session, write_half).await?;
                        }
                    }
                }
            }
        },
        "comm_close" => {
            let req: JupyterMessage<CommClose> = JupyterMessage::try_from(&wire_msg)?;
            
            // Send close message to the comm and remove it from tracking
            let mut comms = open_comms.lock().await;
            if let Some(comm_socket) = comms.remove(&req.content.comm_id) {
                log::trace!("Closing comm {}", req.content.comm_id);
                let _ = comm_socket.incoming_tx.send(CommMsg::Close);
            }
        },
        "interrupt_request" => {
            let req: JupyterMessage<crate::wire::interrupt_request::InterruptRequest> =
                JupyterMessage::try_from(&wire_msg)?;
            let handler_clone = control_handler.clone();
            let result = tokio::task::spawn_blocking(move || {
                let handler = handler_clone.lock().unwrap();
                futures::executor::block_on(handler.handle_interrupt_request())
            }).await;
            match result {
                Ok(Ok(reply)) => {
                    send_reply(&req, reply, session, write_half).await?;
                },
                Ok(Err(e)) => {
                    log::error!("interrupt_request handler returned error: {:?}", e);
                },
                Err(e) => {
                    log::error!("interrupt_request spawn_blocking join error: {:?}", e);
                }
            }
        },
        "shutdown_request" => {
            let req: JupyterMessage<crate::wire::shutdown_request::ShutdownRequest> =
                JupyterMessage::try_from(&wire_msg)?;
            let handler_clone = control_handler.clone();
            let content_clone = req.content.clone();
            let reply = tokio::task::spawn_blocking(move || {
                let handler = handler_clone.lock().unwrap();
                futures::executor::block_on(handler.handle_shutdown_request(&content_clone))
            }).await.ok();
            
            if let Some(Ok(reply)) = reply {
                send_reply(&req, reply, session, write_half).await?;
            }
        },
        "input_reply" => {
            let reply: JupyterMessage<crate::wire::input_reply::InputReply> =
                JupyterMessage::try_from(&wire_msg)?;
            
            // Forward the input reply back to R through the stdin_reply_tx channel
            if let Err(e) = stdin_reply_tx.send(Ok(reply.content)) {
                log::error!("Failed to forward input_reply to R: {}", e);
            }
        },
        _ => {
            log::warn!("Unhandled message type: {}", msg_type);
        }
    }

    Ok(())
}

async fn send_reply<Req, Rep>(
    req: &JupyterMessage<Req>,
    reply_content: Rep,
    session: &Session,
    write_half: &Arc<TokioMutex<WsWriter>>,
) -> Result<(), Error>
where
    Req: ProtocolMessage,
    Rep: ProtocolMessage,
{
    let reply_msg = req.create_reply(reply_content, session);
    let wire_reply = WireMessage::try_from(&reply_msg)?;
    let json = serde_json::to_string(&wire_reply)
        .map_err(|e| Error::Anyhow(anyhow::anyhow!("Failed to serialize reply: {}", e)))?;
    
    let mut write_guard = write_half.lock().await;
    write_guard.send(WsMessage::Text(json)).await
        .map_err(|e| Error::Anyhow(anyhow::anyhow!("Failed to send reply: {}", e)))?;
    
    Ok(())
}

fn send_status<Req>(
    req: &JupyterMessage<Req>,
    state: ExecutionState,
    iopub_tx: &Sender<IOPubMessage>,
) -> Result<(), Error>
where
    Req: ProtocolMessage,
{
    let status = KernelStatus { execution_state: state };
    let msg = IOPubMessage::Status(
        req.header.clone(),
        crate::socket::iopub::IOPubContextChannel::Shell,
        status
    );
    iopub_tx.send(msg)
        .map_err(|e| Error::Anyhow(anyhow::anyhow!("Failed to send status: {}", e)))?;
    Ok(())
}
