/*
 * originator.rs
 *
 * Copyright (C) 2023 Posit Software, PBC. All rights reserved.
 *
 */

use crate::wire::header::JupyterHeader;
use crate::wire::jupyter_message::JupyterMessage;

#[derive(Debug, Clone)]
pub struct Originator {
    pub header: JupyterHeader,
}

impl<T> From<&JupyterMessage<T>> for Originator {
    fn from(msg: &JupyterMessage<T>) -> Originator {
        Originator {
            header: msg.header.clone(),
        }
    }
}
