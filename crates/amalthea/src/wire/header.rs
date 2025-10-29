/*
 * header.rs
 *
 * Copyright (C) 2022 Posit Software, PBC. All rights reserved.
 *
 */

use serde::Deserialize;
use serde::Serialize;
use uuid::Uuid;
use time::{format_description::well_known::Rfc3339, OffsetDateTime};

/// Represents the header of a Jupyter message
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct JupyterHeader {
    /// The message identifier; must be unique per message
    pub msg_id: String,

    /// Session ID; must be unique per session
    pub session: String,

    /// Username; must be unique per user
    pub username: String,

    /// Date/time when message was created (ISO 8601)
    pub date: String,

    /// Message type
    pub msg_type: String,

    /// Message protocol version
    pub version: String,
}

impl JupyterHeader {
    /// Creates a new Jupyter message header
    pub fn create(msg_type: String, session: String, username: String) -> Self {
        Self {
            msg_id: Uuid::new_v4().to_string(),
            session,
            username,
            msg_type,
            date: Self::now_timestamp(),
            version: String::from("5.3"),
        }
    }

    pub(crate) fn now_timestamp() -> String {
        OffsetDateTime::now_utc()
            .format(&Rfc3339)
            .unwrap_or_else(|_| "<unknown>".to_string())
    }
}
