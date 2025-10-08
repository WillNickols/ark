//
// start.rs
//
// Copyright (C) 2023-2024 Posit Software, PBC. All rights reserved.
//
//

use std::sync::Arc;
use std::sync::Mutex;

use amalthea::comm::event::CommManagerEvent;
use amalthea::connection_file::ConnectionFile;
use amalthea::kernel;
use amalthea::registration_file::RegistrationFile;
use amalthea::socket::iopub::IOPubMessage;
use amalthea::socket::stdin::StdInRequest;
use bus::Bus;
use crossbeam::channel::bounded;
use crossbeam::channel::unbounded;

use crate::control::Control;
use crate::dap;
use crate::interface::SessionMode;
use crate::lsp;
use crate::plots::graphics_device::GraphicsDeviceNotification;
use crate::repos::DefaultRepos;
use crate::request::KernelRequest;
use crate::request::RRequest;
use crate::shell::Shell;

/// Exported for unit tests. (DEPRECATED - ZMQ only, use start_kernel_websocket)
#[allow(dead_code)]
pub fn start_kernel(
    _connection_file: ConnectionFile,
    _registration_file: Option<RegistrationFile>,
    _r_args: Vec<String>,
    _startup_file: Option<String>,
    _session_mode: SessionMode,
    _capture_streams: bool,
    _default_repos: DefaultRepos,
) {
    panic!("ZMQ-based start_kernel is no longer supported. Use start_kernel_websocket instead.");
}

pub fn start_kernel_websocket(
    port: u16,
    r_args: Vec<String>,
    startup_file: Option<String>,
    session_mode: SessionMode,
    capture_streams: bool,
    default_repos: DefaultRepos,
) {
    let (iopub_tx, iopub_rx) = bounded::<IOPubMessage>(10);
    let (comm_manager_tx, comm_manager_rx) = bounded::<CommManagerEvent>(10);
    let mut kernel_init_tx = Bus::new(1);
    let (r_request_tx, r_request_rx) = bounded::<RRequest>(1);
    let (kernel_request_tx, kernel_request_rx) = bounded::<KernelRequest>(1);

    let lsp = Arc::new(Mutex::new(lsp::handler::Lsp::new(kernel_init_tx.add_rx())));
    let dap = dap::Dap::new_shared(r_request_tx.clone());
    let (stdin_request_tx, stdin_request_rx) = bounded::<StdInRequest>(1);
    let (graphics_device_tx, graphics_device_rx) =
        tokio::sync::mpsc::unbounded_channel::<GraphicsDeviceNotification>();

    let kernel_init_rx = kernel_init_tx.add_rx();
    let shell = Box::new(Shell::new(
        comm_manager_tx.clone(),
        r_request_tx.clone(),
        stdin_request_tx.clone(),
        kernel_init_rx,
        kernel_request_tx,
        graphics_device_tx,
    ));

    let control = Arc::new(Mutex::new(Control::new(r_request_tx.clone())));

    let stream_behavior = match capture_streams {
        true => amalthea::kernel::StreamBehavior::Capture,
        false => amalthea::kernel::StreamBehavior::None,
    };

    let (stdin_reply_tx, stdin_reply_rx) = unbounded();

    let res = kernel::connect_websocket(
        "ark",
        port,
        shell,
        control,
        Some(lsp),
        Some(dap.clone()),
        stream_behavior,
        iopub_tx.clone(),
        iopub_rx,
        comm_manager_tx.clone(),
        comm_manager_rx,
        stdin_request_rx,
        stdin_reply_tx,
    );
    if let Err(err) = res {
        panic!("Couldn't start WebSocket server: {err:?}");
    }

    if let Err(err) = crate::sys::parent_monitor::start_parent_monitoring(r_request_tx.clone()) {
        log::error!("Failed to start parent process monitoring: {err}");
    }

    // Run R on main thread (it's designed to block - that's OK)
    // The WebSocket server runs in background threads
    crate::interface::RMain::start(
        r_args,
        startup_file,
        comm_manager_tx,
        r_request_rx,
        stdin_request_tx,
        stdin_reply_rx,
        iopub_tx,
        kernel_init_tx,
        kernel_request_rx,
        dap,
        session_mode,
        default_repos,
        graphics_device_rx,
    );
}
