//
// shell.rs
//
// Copyright (C) 2022-2024 Posit Software, PBC. All rights reserved.
//
//

use amalthea::comm::comm_channel::Comm;
use amalthea::comm::event::CommManagerEvent;
use amalthea::language::shell_handler::ShellHandler;
use amalthea::socket::comm::CommSocket;
use amalthea::socket::stdin::StdInRequest;
use amalthea::wire::complete_reply::CompleteReply;
use amalthea::wire::complete_request::CompleteRequest;
use amalthea::wire::execute_reply::ExecuteReply;
use amalthea::wire::execute_request::ExecuteRequest;
use amalthea::wire::inspect_reply::InspectReply;
use amalthea::wire::inspect_request::InspectRequest;
use amalthea::wire::is_complete_reply::IsComplete;
use amalthea::wire::is_complete_reply::IsCompleteReply;
use amalthea::wire::is_complete_request::IsCompleteRequest;
use amalthea::wire::jupyter_message::Status;
use amalthea::wire::kernel_info_reply::KernelInfoReply;
use amalthea::wire::kernel_info_request::KernelInfoRequest;
use amalthea::wire::language_info::LanguageInfo;
use amalthea::wire::language_info::LanguageInfoPositron;
use amalthea::wire::originator::Originator;
use async_trait::async_trait;
use bus::BusReader;
use crossbeam::channel::unbounded;
use crossbeam::channel::Sender;
use harp::exec::RFunction;
use harp::exec::RFunctionExt;
use harp::line_ending::convert_line_endings;
use harp::line_ending::LineEnding;
use harp::object::RObject;
use harp::ParseResult;
use log::*;
use serde_json::json;
use tokio::sync::mpsc::UnboundedSender as AsyncUnboundedSender;

use crate::help::r_help::RHelp;
use crate::help_proxy;
use crate::interface::KernelInfo;
use crate::interface::RMain;
use crate::plots::graphics_device::GraphicsDeviceNotification;
use crate::r_task;
use crate::request::KernelRequest;
use crate::request::RRequest;
use crate::ui::UiComm;

pub struct Shell {
    comm_manager_tx: Sender<CommManagerEvent>,
    r_request_tx: Sender<RRequest>,
    stdin_request_tx: Sender<StdInRequest>,
    kernel_request_tx: Sender<KernelRequest>,
    kernel_init_rx: BusReader<KernelInfo>,
    kernel_info: Option<KernelInfo>,
    graphics_device_tx: AsyncUnboundedSender<GraphicsDeviceNotification>,
}

#[derive(Debug)]
pub enum REvent {
    Prompt,
}

impl Shell {
    /// Creates a new instance of the shell message handler.
    pub(crate) fn new(
        comm_manager_tx: Sender<CommManagerEvent>,
        r_request_tx: Sender<RRequest>,
        stdin_request_tx: Sender<StdInRequest>,
        kernel_init_rx: BusReader<KernelInfo>,
        kernel_request_tx: Sender<KernelRequest>,
        graphics_device_tx: AsyncUnboundedSender<GraphicsDeviceNotification>,
    ) -> Self {
        Self {
            comm_manager_tx,
            r_request_tx,
            stdin_request_tx,
            kernel_request_tx,
            kernel_init_rx,
            kernel_info: None,
            graphics_device_tx,
        }
    }

    fn r_handle_is_complete_request(
        &self,
        req: &IsCompleteRequest,
    ) -> amalthea::Result<IsCompleteReply> {
        match harp::parse_status(&harp::ParseInput::Text(req.code.as_str())) {
            Ok(ParseResult::Complete(_)) => Ok(IsCompleteReply {
                status: IsComplete::Complete,
                indent: String::from(""),
            }),
            Ok(ParseResult::Incomplete) => Ok(IsCompleteReply {
                status: IsComplete::Incomplete,
                indent: String::from("+"),
            }),
            Err(_) | Ok(ParseResult::SyntaxError { .. }) => Ok(IsCompleteReply {
                status: IsComplete::Invalid,
                indent: String::from(""),
            }),
        }
    }

    fn r_handle_complete_request(
        &self,
        req: &CompleteRequest,
    ) -> amalthea::Result<CompleteReply> {
        use harp::exec::RFunction;
        use harp::exec::RFunctionExt;
        
        let code = &req.code;
        let cursor_pos = req.cursor_pos as i32;
        
        unsafe {
            let result = RFunction::new("", ".ps.completions.getJupyterCompletions")
                .add(code.as_str())
                .add(cursor_pos)
                .call();
            
            match result {
                Ok(value) => {
                    let matches: Vec<String> = RFunction::new("base", "[[")
                        .add(value.sexp)
                        .add("matches")
                        .call()
                        .and_then(|x| x.try_into())
                        .unwrap_or_default();
                    
                    let cursor_start: i32 = RFunction::new("base", "[[")
                        .add(value.sexp)
                        .add("cursor_start")
                        .call()
                        .and_then(|x| x.try_into())
                        .unwrap_or(cursor_pos);
                    
                    let cursor_end: i32 = RFunction::new("base", "[[")
                        .add(value.sexp)
                        .add("cursor_end")
                        .call()
                        .and_then(|x| x.try_into())
                        .unwrap_or(cursor_pos);
                    
                    Ok(CompleteReply {
                        matches,
                        status: Status::Ok,
                        cursor_start: cursor_start as u32,
                        cursor_end: cursor_end as u32,
                        metadata: json!({}),
                    })
                }
                Err(_) => {
                    Ok(CompleteReply {
                        matches: vec![],
                        status: Status::Ok,
                        cursor_start: cursor_pos as u32,
                        cursor_end: cursor_pos as u32,
                        metadata: json!({}),
                    })
                }
            }
        }
    }
}

#[async_trait]
impl ShellHandler for Shell {
    async fn handle_info_request(
        &mut self,
        _req: &KernelInfoRequest,
    ) -> amalthea::Result<KernelInfoReply> {
        // Wait here for kernel initialization if it hasn't completed. This is
        // necessary for two reasons:
        //
        // 1. The kernel info response must include the startup banner, which is
        //    not emitted until R is done starting up.
        // 2. Jupyter frontends typically wait for the kernel info response to
        //    be sent before they signal that the kernel as ready for use, so
        //    blocking here ensures that it doesn't try to execute code before R is
        //    ready.
        if self.kernel_info.is_none() {
            trace!("Got kernel info request; waiting for R to complete initialization");
            self.kernel_info = Some(self.kernel_init_rx.recv().unwrap());
            trace!("R completed initialization, replying to kernel info request");
        } else {
            trace!("Got kernel info request; R has already started, replying to kernel info request with existing kernel information")
        }
        let kernel_info = self.kernel_info.as_ref().unwrap();

        let info = LanguageInfo {
            name: String::from("R"),
            version: kernel_info.version.clone(),
            file_extension: String::from(".R"),
            mimetype: String::from("text/r"),
            pygments_lexer: None,
            codemirror_mode: None,
            nbconvert_exporter: None,
            positron: Some(LanguageInfoPositron {
                input_prompt: kernel_info.input_prompt.clone(),
                continuation_prompt: kernel_info.continuation_prompt.clone(),
            }),
        };
        Ok(KernelInfoReply {
            status: Status::Ok,
            banner: kernel_info.banner.clone(),
            debugger: false,
            help_links: Vec::new(),
            language_info: info,
        })
    }

    async fn handle_complete_request(
        &self,
        req: &CompleteRequest,
    ) -> amalthea::Result<CompleteReply> {
        r_task(|| self.r_handle_complete_request(req))
    }

    /// Handle a request to test code for completion.
    async fn handle_is_complete_request(
        &self,
        req: &IsCompleteRequest,
    ) -> amalthea::Result<IsCompleteReply> {
        r_task(|| self.r_handle_is_complete_request(req))
    }

    /// Handles an ExecuteRequest by sending the code to the R execution thread
    /// for processing.
    async fn handle_execute_request(
        &mut self,
        originator: Originator,
        req: &ExecuteRequest,
    ) -> amalthea::Result<ExecuteReply> {
        let (response_tx, response_rx) = unbounded::<amalthea::Result<ExecuteReply>>();
        let mut req_clone = req.clone();
        req_clone.code = convert_line_endings(&req_clone.code, LineEnding::Posix);
        
        if let Err(err) = self.r_request_tx.send(RRequest::ExecuteCode(
            req_clone.clone(),
            originator,
            response_tx,
        )) {
            warn!(
                "Could not deliver execution request to execution thread: {}",
                err
            )
        }

        // Use spawn_blocking to avoid blocking the async runtime
        // This allows interrupt requests to be processed while R is executing
        let result = tokio::task::spawn_blocking(move || {
            response_rx.recv().unwrap()
        }).await.unwrap();

        result
    }

    /// Handles an introspection request
    async fn handle_inspect_request(&self, req: &InspectRequest) -> amalthea::Result<InspectReply> {
        let data = match req.code.as_str() {
            "err" => {
                json!({"text/plain": "This generates an error!"})
            },
            "teapot" => {
                json!({"text/plain": "This is clearly a teapot."})
            },
            _ => serde_json::Value::Null,
        };
        Ok(InspectReply {
            status: Status::Ok,
            found: data != serde_json::Value::Null,
            data,
            metadata: json!({}),
        })
    }

    /// Handle a request to open a new comm channel
    ///
    /// Note that there might be multiple requests during a single session if
    /// the UI has been disconnected and reconnected.
    async fn handle_comm_open(&self, target: Comm, comm: CommSocket) -> amalthea::Result<bool> {
        match target {
            Comm::Variables => {
                log::info!("[COMM OPEN] Received comm open for Variables");
                Ok(true)
            },
            Comm::Ui => {
                log::info!("[COMM OPEN] Received comm open for Ui");
                handle_comm_open_ui(
                    comm,
                    self.stdin_request_tx.clone(),
                    self.kernel_request_tx.clone(),
                    self.graphics_device_tx.clone(),
                )
            },
            Comm::Help => {
                let result = handle_comm_open_help(comm);
                result
            },
            Comm::Environment => {
                log::info!("[COMM OPEN] Received comm open for Environment");
                Ok(true)
            },
            _ => {
                log::info!("[COMM OPEN] Received comm open for unknown target");
                Ok(false)
            }
        }
    }
    
    async fn handle_comm_message(&self, _comm_id: &str, data: &serde_json::Value) -> amalthea::Result<serde_json::Value> {
        let method = data.get("method").and_then(|m| m.as_str()).unwrap_or("");
        let id = data.get("id").cloned();
        let params = data.get("params");
        
        let result = match method {
            "list_packages" => {
                let packages = r_task(|| -> anyhow::Result<Vec<serde_json::Value>> {
                    let result = RFunction::from("installed.packages").call()?;
                    
                    let df = RFunction::from("as.data.frame")
                        .param("x", result)
                        .param("stringsAsFactors", false)
                        .call()?;
                    
                    let nrows: i32 = RFunction::from("nrow")
                        .param("x", df.clone())
                        .call()
                        .and_then(|x| x.try_into())?;
                    
                    let mut packages = Vec::new();
                    for i in 1..=nrows {
                        let name: String = RFunction::from("[")
                            .param("x", df.clone())
                            .param("i", i)
                            .param("j", "Package")
                            .call()
                            .and_then(|x| x.try_into())?;
                        
                        let version: String = RFunction::from("[")
                            .param("x", df.clone())
                            .param("i", i)
                            .param("j", "Version")
                            .call()
                            .and_then(|x| x.try_into())?;
                        
                        packages.push(serde_json::json!({
                            "name": name,
                            "version": version
                        }));
                    }
                    
                    Ok(packages)
                }).map_err(|e| amalthea::Error::Anyhow(anyhow::anyhow!("Failed to list packages: {}", e)))?;
                
                serde_json::Value::Array(packages)
            },
            
            "install_package" => {
                let package_name = params
                    .and_then(|p| p.get("package_name"))
                    .and_then(|n| n.as_str())
                    .ok_or_else(|| amalthea::Error::Anyhow(anyhow::anyhow!("Missing package_name parameter")))?;
                
                let install_result = r_task(|| -> anyhow::Result<()> {
                    RFunction::from("install.packages")
                        .param("pkgs", package_name)
                        .call()?;
                    Ok(())
                });
                
                match install_result {
                    Ok(_) => serde_json::json!({
                        "success": true,
                        "error": serde_json::Value::Null
                    }),
                    Err(e) => serde_json::json!({
                        "success": false,
                        "error": format!("Failed to install package {}: {}", package_name, e)
                    })
                }
            },
            
            "uninstall_package" => {
                let package_name = params
                    .and_then(|p| p.get("package_name"))
                    .and_then(|n| n.as_str())
                    .ok_or_else(|| amalthea::Error::Anyhow(anyhow::anyhow!("Missing package_name parameter")))?;
                
                let uninstall_result = r_task(|| -> anyhow::Result<()> {
                    RFunction::from("remove.packages")
                        .param("pkgs", package_name)
                        .call()?;
                    Ok(())
                });
                
                match uninstall_result {
                    Ok(_) => serde_json::json!({
                        "success": true,
                        "error": serde_json::Value::Null
                    }),
                    Err(e) => serde_json::json!({
                        "success": false,
                        "error": format!("Failed to uninstall package {}: {}", package_name, e)
                    })
                }
            },
            
            "set_working_directory" => {
                let directory = params
                    .and_then(|p| {
                        // Try array access first
                        if let Some(arr) = p.as_array() {
                            arr.get(0).and_then(|v| v.as_str())
                        } else {
                            // Try object access
                            p.get("directory").and_then(|v| v.as_str())
                                .or_else(|| p.get("0").and_then(|v| v.as_str()))
                                .or_else(|| p.as_str())
                        }
                    })
                    .ok_or_else(|| amalthea::Error::Anyhow(anyhow::anyhow!("Missing directory parameter")))?;
                
                r_task(|| -> anyhow::Result<serde_json::Value> {
                    // Call the R function that sets directory and notifies frontend
                    let result = RFunction::from(".ps.ui.setWorkingDirectory")
                        .add(directory)
                        .call()?;
                    Ok(serde_json::Value::try_from(result)?)
                }).map_err(|e| amalthea::Error::Anyhow(anyhow::anyhow!("Failed to set working directory: {}", e)))?
            },
            
            "show_help_topic" => {
                let topic = params
                    .and_then(|p| p.get("topic"))
                    .and_then(|t| t.as_str())
                    .ok_or_else(|| amalthea::Error::Anyhow(anyhow::anyhow!("Missing topic parameter")))?;
                
                let found = r_task(|| -> anyhow::Result<bool> {
                    let result: harp::Result<bool> = RFunction::from(".ps.rpc.showHelpTopic")
                        .param("topic", topic)
                        .call()
                        .and_then(|x| x.try_into());
                    
                    match result {
                        Ok(found) => Ok(found),
                        Err(_) => Ok(false)
                    }
                }).map_err(|e| amalthea::Error::Anyhow(anyhow::anyhow!("Failed to show help topic: {}", e)))?;
                
                serde_json::Value::Bool(found)
            },
            
            "search_help_topics" => {
                let query = params
                    .and_then(|p| p.get("query"))
                    .and_then(|q| q.as_str())
                    .unwrap_or("");
                
                let topics = r_task(|| -> anyhow::Result<Vec<String>> {
                    let result: harp::Result<Vec<String>> = RFunction::from(".ps.rpc.searchHelpTopics")
                        .param("query", query)
                        .call()
                        .and_then(|x| x.try_into());
                    
                    match result {
                        Ok(topics) => Ok(topics),
                        Err(e) => Err(anyhow::anyhow!("Failed to search help topics: {}", e))
                    }
                }).map_err(|e| amalthea::Error::Anyhow(anyhow::anyhow!("Failed to search help topics: {}", e)))?;
                
                serde_json::Value::Array(topics.into_iter().map(|t| serde_json::Value::String(t)).collect())
            },
            
            "parse_functions" => {
                let code = params
                    .and_then(|p| p.get("code"))
                    .and_then(|c| c.as_str())
                    .ok_or_else(|| amalthea::Error::Anyhow(anyhow::anyhow!("Missing code parameter")))?;
                
                let language = params
                    .and_then(|p| p.get("language"))
                    .and_then(|l| l.as_str())
                    .unwrap_or("r");
                
                let result = r_task(|| -> anyhow::Result<serde_json::Value> {
                    let result: harp::Result<RObject> = RFunction::from(".ps.rpc.parse_functions")
                        .param("code", code)
                        .param("language", language)
                        .call();
                    
                    match result {
                        Ok(obj) => {
                            // The R function returns a list with functions, success, error
                            let functions: Vec<String> = RFunction::from("[[")
                                .param("x", obj.clone())
                                .param("i", "functions")
                                .call()
                                .and_then(|x| x.try_into())
                                .unwrap_or_else(|_| vec![]);
                            
                            let success: bool = RFunction::from("[[")
                                .param("x", obj.clone())
                                .param("i", "success")
                                .call()
                                .and_then(|x| x.try_into())
                                .unwrap_or(false);
                            
                            let error: Option<String> = RFunction::from("[[")
                                .param("x", obj)
                                .param("i", "error")
                                .call()
                                .and_then(|x| x.try_into())
                                .ok();
                            
                            Ok(serde_json::json!({
                                "functions": functions,
                                "success": success,
                                "error": error
                            }))
                        },
                        Err(e) => Ok(serde_json::json!({
                            "functions": Vec::<String>::new(),
                            "success": false,
                            "error": format!("{}", e)
                        }))
                    }
                }).map_err(|e| amalthea::Error::Anyhow(anyhow::anyhow!("Failed to parse functions: {}", e)))?;
                
                result
            },
            
            _ => {
                return Err(amalthea::Error::Anyhow(anyhow::anyhow!("Unknown method: {}", method)));
            }
        };
        
        Ok(serde_json::json!({
            "jsonrpc": "2.0",
            "id": id,
            "result": result
        }))
    }
}


fn handle_comm_open_ui(
    comm: CommSocket,
    stdin_request_tx: Sender<StdInRequest>,
    kernel_request_tx: Sender<KernelRequest>,
    graphics_device_tx: AsyncUnboundedSender<GraphicsDeviceNotification>,
) -> amalthea::Result<bool> {
    // Create a frontend to wrap the comm channel we were just given. This starts
    // a thread that proxies messages to the frontend.
    let ui_comm_tx = UiComm::start(comm, stdin_request_tx, graphics_device_tx);

    // Send the frontend event channel to the execution thread so it can emit
    // events to the frontend.
    if let Err(err) = kernel_request_tx.send(KernelRequest::EstablishUiCommChannel(ui_comm_tx)) {
        log::error!("Could not deliver UI comm channel to execution thread: {err:?}");
    };

    Ok(true)
}

fn handle_comm_open_help(comm: CommSocket) -> amalthea::Result<bool> {
    use stdext::unwrap;
    
    r_task(|| {
        // Ensure the R help server is started, and get its port
        let r_port = unwrap!(RHelp::r_start_or_reconnect_to_help_server(), Err(err) => {
            log::error!("Could not start R help server: {err:?}");
            return Ok(false);
        });

        // Ensure our proxy help server is started, and get its port
        let proxy_port = unwrap!(help_proxy::start(r_port), Err(err) => {
            log::error!("Could not start R help proxy server: {err:?}");
            return Ok(false);
        });

        // Start the R Help handler that routes help requests
        let help_event_tx = unwrap!(RHelp::start(comm, r_port, proxy_port), Err(err) => {
            log::error!("Could not start R Help handler: {err:?}");
            return Ok(false);
        });

        // Send the help event channel to the main R thread so it can
        // emit help events, to be delivered over the help comm.
        RMain::with_mut(|main| main.set_help_fields(help_event_tx, r_port));

        Ok(true)
    })
}


