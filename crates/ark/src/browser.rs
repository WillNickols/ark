//
// browser.rs
//
// Copyright (C) 2023-2024 Posit Software, PBC. All rights reserved.
//
//

use harp::object::RObject;
use harp::utils::r_normalize_path;
use libr::Rf_ScalarLogical;
use libr::SEXP;

use crate::help::message::HelpEvent;
use crate::help::message::ShowHelpUrlParams;
use crate::interface::RMain;
use crate::ui::events::send_open_with_system_event;
use crate::ui::events::send_show_url_event;

#[harp::register]
pub unsafe extern "C-unwind" fn ps_browse_url(url: SEXP) -> anyhow::Result<SEXP> {
    ps_browse_url_impl(url).or_else(|err| {
        log::error!("Failed to browse url due to: {err}");
        Ok(Rf_ScalarLogical(0))
    })
}

fn is_help_url(url: &str) -> bool {
    RMain::with(|main| main.is_help_url(url))
}

fn handle_help_url(url: String) -> anyhow::Result<()> {
    log::info!("[RUST BROWSER] handle_help_url: Creating ShowHelpUrl event for: {}", url);
    RMain::with(|main| {
        let event = HelpEvent::ShowHelpUrl(ShowHelpUrlParams { url: url.clone() });
        log::info!("[RUST BROWSER] handle_help_url: Calling main.send_help_event");
        let result = main.send_help_event(event);
        match &result {
            Ok(_) => log::info!("[RUST BROWSER] handle_help_url: Successfully sent help event"),
            Err(e) => log::error!("[RUST BROWSER] handle_help_url: Failed to send help event: {}", e),
        }
        result
    })
}

unsafe fn ps_browse_url_impl(url: SEXP) -> anyhow::Result<SEXP> {
    // Extract URL string for analysis
    let url_string = RObject::view(url).to::<String>()?;
    let _span = tracing::trace_span!("browseURL", url = %url_string).entered();
    
    log::info!("[RUST BROWSER] ps_browse_url called with URL: {}", url_string);

    // Handle help server requests.
    if is_help_url(&url_string) {
        log::info!("[RUST BROWSER] Detected help URL, calling handle_help_url");
        handle_help_url(url_string)?;
        log::info!("[RUST BROWSER] handle_help_url completed successfully");
        return Ok(Rf_ScalarLogical(1));
    }

    // Handle web URLs
    if is_web_url(&url_string) {
        log::info!("[RUST BROWSER] Handling web URL");
        send_show_url_event(&url_string)?;
        return Ok(Rf_ScalarLogical(1));
    }

    // This is probably a file path? Send to the front end and ask for system
    // default opener.
    log::info!("[RUST BROWSER] Treating as file path and asking system to open");
    let path = r_normalize_path(url.into())?;
    send_open_with_system_event(&path)?;
    Ok(Rf_ScalarLogical(1))
}

fn is_web_url(url: &str) -> bool {
    url.starts_with("http://") || url.starts_with("https://")
}
