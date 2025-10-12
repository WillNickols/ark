/*
 * websocket/stdin.rs
 *
 * Copyright (C) 2025 Lotas Inc. All rights reserved.
 *
 */

use serde_json::json;

use crate::session::Session;
use crate::socket::stdin::StdInRequest;
use crate::wire::header::JupyterHeader;
use crate::wire::input_request::InputRequest;
use crate::wire::input_request::ShellInputRequest;

/// Convert a StdInRequest to a Jupyter input_request JSON message
pub fn convert_stdin_request_to_json(request: &StdInRequest, session: &Session) -> Option<String> {
    match request {
        StdInRequest::Input(shell_request) => {
            let json = create_input_request_json(&shell_request.request, session, &shell_request.originator.header);
            Some(json)
        },
        StdInRequest::Comm(_) => {
            // Comm-based stdin requests are not yet supported over WebSocket
            log::warn!("Comm-based stdin requests not yet supported over WebSocket");
            None
        },
    }
}

fn create_input_request_json(request: &InputRequest, session: &Session, parent_header: &JupyterHeader) -> String {
    let header = JupyterHeader::create(
        String::from("input_request"),
        session.session_id.clone(),
        session.username.clone(),
    );

    let message = json!({
        "header": {
            "msg_id": header.msg_id,
            "session": header.session,
            "username": header.username,
            "date": header.date,
            "msg_type": header.msg_type,
            "version": header.version,
        },
        "parent_header": {
            "msg_id": parent_header.msg_id,
            "session": parent_header.session,
            "username": parent_header.username,
            "date": parent_header.date,
            "msg_type": parent_header.msg_type,
            "version": parent_header.version,
        },
        "metadata": {},
        "content": {
            "prompt": request.prompt,
            "password": request.password,
        }
    });

    serde_json::to_string(&message).unwrap_or_else(|e| {
        log::error!("Failed to serialize input_request: {}", e);
        String::new()
    })
}


