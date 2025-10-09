/*
 * iopub.rs
 *
 * Copyright (C) 2022 Posit Software, PBC. All rights reserved.
 *
 */

use crossbeam::channel::Sender;

use crate::wire::comm_close::CommClose;
use crate::wire::comm_msg::CommWireMsg;
use crate::wire::comm_open::CommOpen;
use crate::wire::display_data::DisplayData;
use crate::wire::execute_error::ExecuteError;
use crate::wire::execute_input::ExecuteInput;
use crate::wire::execute_result::ExecuteResult;
use crate::wire::header::JupyterHeader;
use crate::wire::status::KernelStatus;
use crate::wire::stream::StreamOutput;
use crate::wire::update_display_data::UpdateDisplayData;

/// Enumeration of possible channels that an IOPub message can be associated
/// with.
pub enum IOPubContextChannel {
    Shell,
    Control,
}

/// Enumeration of all messages that can be delivered from the IOPub channel.
/// These messages generally are created on other threads and then sent
/// to the IOPub broadcaster for delivery to all connected clients.
pub enum IOPubMessage {
    Status(JupyterHeader, IOPubContextChannel, KernelStatus),
    ExecuteResult(ExecuteResult),
    ExecuteError(ExecuteError),
    ExecuteInput(ExecuteInput),
    Stream(StreamOutput),
    CommOpen(CommOpen),
    CommMsgReply(JupyterHeader, CommWireMsg),
    CommMsgEvent(CommWireMsg),
    CommClose(CommClose),
    DisplayData(Option<JupyterHeader>, DisplayData),
    UpdateDisplayData(Option<JupyterHeader>, UpdateDisplayData),
    Wait(Wait),
}

/// A special IOPub message used to block the sender until the IOPub queue has
/// forwarded all messages before this one on to the frontend.
pub struct Wait {
    pub wait_tx: Sender<()>,
}
