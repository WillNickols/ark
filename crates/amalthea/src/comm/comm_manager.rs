/*
 * comm_manager.rs
 *
 * Stub - comms are now sent directly through WebSocket sessions
 */

use crossbeam::channel::{Receiver, Sender};
use stdext::spawn;

use crate::comm::event::CommManagerEvent;
use crate::socket::iopub::IOPubMessage;

pub struct CommManager;

impl CommManager {
    /// Stub - drains the channel but does nothing
    /// Actual comm handling is done directly in WebSocket sessions
    pub fn start(_iopub_tx: Sender<IOPubMessage>, comm_event_rx: Receiver<CommManagerEvent>) {
        spawn!("comm-manager-stub", move || {
            while let Ok(_event) = comm_event_rx.recv() {
                // Events are now handled directly in WebSocket session
            }
        });
    }
}
