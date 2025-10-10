//
// events.rs
//
// Copyright (C) 2024 by Posit Software, PBC
//
//

use amalthea::comm::ui_comm::OpenEditorParams;
use amalthea::comm::ui_comm::OpenWithSystemParams;
use amalthea::comm::ui_comm::OpenWorkspaceParams;
use amalthea::comm::ui_comm::Position;
use amalthea::comm::ui_comm::Range;
use amalthea::comm::ui_comm::SetEditorSelectionsParams;
use amalthea::comm::ui_comm::ShowMessageParams;
use amalthea::comm::ui_comm::ShowUrlParams;
use amalthea::comm::ui_comm::UiFrontendEvent;
use harp::object::RObject;
use libr::R_NilValue;
use libr::SEXP;

use crate::interface::RMain;

#[harp::register]
pub unsafe extern "C-unwind" fn ps_ui_show_message(message: SEXP) -> anyhow::Result<SEXP> {
    let params = ShowMessageParams {
        message: RObject::view(message).try_into()?,
    };

    let event = UiFrontendEvent::ShowMessage(params);

    let main = RMain::get();
    let ui_comm_tx = main
        .get_ui_comm_tx()
        .ok_or_else(|| ui_comm_not_connected("ui_show_message"))?;
    ui_comm_tx.send_event(event);

    Ok(R_NilValue)
}

#[harp::register]
pub unsafe extern "C-unwind" fn ps_ui_set_working_directory(path: SEXP) -> anyhow::Result<SEXP> {
    use harp::exec::RFunction;
    use harp::exec::RFunctionExt;
    use amalthea::comm::ui_comm::WorkingDirectoryParams;
    
    let path_str: String = RObject::view(path).try_into()?;
    
    // Actually set the working directory in R
    let result = RFunction::from("setwd")
        .param("dir", path_str.clone())
        .call()?;
    
    // Notify the frontend via UI comm event
    let main = RMain::get();
    if let Some(ui_comm_tx) = main.get_ui_comm_tx() {
        // Use the same aliasing logic as refresh_working_directory
        let mut new_working_directory = std::path::PathBuf::from(&path_str);
        
        // Attempt to alias the directory if it's within the home directory
        if let Some(home_dir) = home::home_dir() {
            if let Ok(stripped_dir) = new_working_directory.strip_prefix(home_dir) {
                let mut new_path = std::path::PathBuf::from("~");
                new_path.push(stripped_dir);
                new_working_directory = new_path;
            }
        }
        
        let event = UiFrontendEvent::WorkingDirectory(WorkingDirectoryParams {
            directory: new_working_directory.to_string_lossy().to_string(),
        });
        ui_comm_tx.send_event(event);
    }
    
    Ok(result.sexp)
}

#[harp::register]
pub unsafe extern "C-unwind" fn ps_ui_open_workspace(
    path: SEXP,
    new_window: SEXP,
) -> anyhow::Result<SEXP> {
    let params = OpenWorkspaceParams {
        path: RObject::view(path).try_into()?,
        new_window: RObject::view(new_window).try_into()?,
    };

    let event = UiFrontendEvent::OpenWorkspace(params);

    let main = RMain::get();
    let ui_comm_tx = main
        .get_ui_comm_tx()
        .ok_or_else(|| ui_comm_not_connected("ui_open_workspace"))?;
    ui_comm_tx.send_event(event);

    Ok(R_NilValue)
}

#[harp::register]
pub unsafe extern "C-unwind" fn ps_ui_navigate_to_file(
    file: SEXP,
    line: SEXP,
    column: SEXP,
) -> anyhow::Result<SEXP> {
    let params = OpenEditorParams {
        file: RObject::view(file).try_into()?,
        line: RObject::view(line).try_into()?,
        column: RObject::view(column).try_into()?,
    };

    let event = UiFrontendEvent::OpenEditor(params);

    let main = RMain::get();
    let ui_comm_tx = main
        .get_ui_comm_tx()
        .ok_or_else(|| ui_comm_not_connected("ui_navigate_to_file"))?;
    ui_comm_tx.send_event(event);

    Ok(R_NilValue)
}

#[harp::register]
pub unsafe extern "C-unwind" fn ps_ui_set_selection_ranges(ranges: SEXP) -> anyhow::Result<SEXP> {
    let selections = ps_ui_robj_as_ranges(ranges)?;
    let params = SetEditorSelectionsParams { selections };

    let event = UiFrontendEvent::SetEditorSelections(params);

    let main = RMain::get();
    let ui_comm_tx = main
        .get_ui_comm_tx()
        .ok_or_else(|| ui_comm_not_connected("ui_set_selection_ranges"))?;
    ui_comm_tx.send_event(event);

    Ok(R_NilValue)
}

pub fn send_show_url_event(url: &str) -> anyhow::Result<()> {
    let params = ShowUrlParams {
        url: url.to_string(),
    };
    let event = UiFrontendEvent::ShowUrl(params);

    let main = RMain::get();
    let ui_comm_tx = main
        .get_ui_comm_tx()
        .ok_or_else(|| ui_comm_not_connected("show_url"))?;
    ui_comm_tx.send_event(event);

    Ok(())
}

#[harp::register]
pub unsafe extern "C-unwind" fn ps_ui_show_url(url: SEXP) -> anyhow::Result<SEXP> {
    let url_string = RObject::view(url).to::<String>()?;
    send_show_url_event(&url_string)?;
    Ok(R_NilValue)
}

pub fn send_open_with_system_event(path: &str) -> anyhow::Result<()> {
    let params = OpenWithSystemParams {
        path: path.to_string(),
    };
    let event = UiFrontendEvent::OpenWithSystem(params);

    let main = RMain::get();
    let ui_comm_tx = main
        .get_ui_comm_tx()
        .ok_or_else(|| ui_comm_not_connected("open_with_system"))?;
    ui_comm_tx.send_event(event);

    Ok(())
}

pub fn ps_ui_robj_as_ranges(ranges: SEXP) -> anyhow::Result<Vec<Range>> {
    let ranges_as_r_objects: Vec<RObject> = RObject::view(ranges).try_into()?;
    let ranges_as_result: Result<Vec<Vec<i32>>, _> = ranges_as_r_objects
        .iter()
        .map(|x| Vec::<i32>::try_from(x.clone()))
        .collect();
    let ranges_as_vec_of_vecs = ranges_as_result?;
    let selections: Vec<Range> = ranges_as_vec_of_vecs
        .iter()
        .map(|chunk| Range {
            start: Position {
                character: chunk[1] as i64,
                line: chunk[0] as i64,
            },
            end: Position {
                character: chunk[3] as i64,
                line: chunk[2] as i64,
            },
        })
        .collect();
    Ok(selections)
}

fn ui_comm_not_connected(name: &str) -> anyhow::Error {
    anyhow::anyhow!("UI comm not connected, can't run `{name}`.")
}
