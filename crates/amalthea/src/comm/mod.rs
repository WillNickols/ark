/*
 * mod.rs
 *
 * Comm types for internal message passing within kernel
 * External communication now goes through WebSockets
 */

pub mod base_comm;
pub mod comm_channel;
pub mod comm_manager;
pub mod connections_comm;
pub mod data_explorer_comm;
pub mod environment_comm;
pub mod event;
pub mod help_comm;
pub mod plot_comm;
pub mod server_comm;
pub mod ui_comm;
pub mod variables_comm;
