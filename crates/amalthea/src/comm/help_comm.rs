// @generated

/*---------------------------------------------------------------------------------------------
 *  Copyright (C) 2024-2025 Posit Software, PBC. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

//
// AUTO-GENERATED from help.json; do not edit.
//

use serde::Deserialize;
use serde::Serialize;

/// Possible values for Kind in ShowHelp
#[derive(Clone, Debug, Serialize, Deserialize, PartialEq, strum_macros::Display)]
pub enum ShowHelpKind {
	#[serde(rename = "html")]
	#[strum(to_string = "html")]
	Html,

	#[serde(rename = "markdown")]
	#[strum(to_string = "markdown")]
	Markdown,

	#[serde(rename = "url")]
	#[strum(to_string = "url")]
	Url
}

/// Parameters for the ShowHelpTopic method.
#[derive(Clone, Debug, Serialize, Deserialize, PartialEq)]
pub struct ShowHelpTopicParams {
	/// The help topic to show
	pub topic: String,
}

/// Parameters for the ShowHelp method.
#[derive(Clone, Debug, Serialize, Deserialize, PartialEq)]
pub struct ShowHelpParams {
	/// The help content to show
	pub content: String,

	/// The type of content to show
	pub kind: ShowHelpKind,

	/// Whether to focus the Help pane when the content is displayed.
	pub focus: bool,
}

/// Parameters for the ParseRFunctions method.
#[derive(Clone, Debug, Serialize, Deserialize, PartialEq)]
pub struct ParseRFunctionsParams {
	/// The R code to parse for function calls
	pub code: String,
}

/// Result for the ParseRFunctions method.
#[derive(Clone, Debug, Serialize, Deserialize, PartialEq)]
pub struct ParseRFunctionsResult {
	/// List of function names found in the code
	pub functions: Vec<String>,

	/// Whether the parsing was successful
	pub success: bool,

	/// Error message if parsing failed
	pub error: Option<String>,
}

/**
 * Backend RPC request types for the help comm
 */
#[derive(Clone, Debug, Serialize, Deserialize, PartialEq)]
#[serde(tag = "method", content = "params")]
pub enum HelpBackendRequest {
	/// Look for and, if found, show a help topic.
	///
	/// Requests that the help backend look for a help topic and, if found,
	/// show it. If the topic is found, it will be shown via a Show Help
	/// notification. If the topic is not found, no notification will be
	/// delivered.
	#[serde(rename = "show_help_topic")]
	ShowHelpTopic(ShowHelpTopicParams),

	/// Parse R code to extract function calls.
	#[serde(rename = "parse_r_functions")]
	ParseRFunctions(ParseRFunctionsParams),

}

/**
 * Backend RPC Reply types for the help comm
 */
#[derive(Clone, Debug, Serialize, Deserialize, PartialEq)]
#[serde(tag = "method", content = "result")]
pub enum HelpBackendReply {
	/// Whether the topic was found and shown. Topics are shown via a Show
	/// Help notification.
	ShowHelpTopicReply(bool),

	/// Result of parsing R code for function calls.
	ParseRFunctionsReply(ParseRFunctionsResult),

}

/**
 * Frontend RPC request types for the help comm
 */
#[derive(Clone, Debug, Serialize, Deserialize, PartialEq)]
#[serde(tag = "method", content = "params")]
pub enum HelpFrontendRequest {
}

/**
 * Frontend RPC Reply types for the help comm
 */
#[derive(Clone, Debug, Serialize, Deserialize, PartialEq)]
#[serde(tag = "method", content = "result")]
pub enum HelpFrontendReply {
}

/**
 * Frontend events for the help comm
 */
#[derive(Clone, Debug, Serialize, Deserialize, PartialEq)]
#[serde(tag = "method", content = "params")]
pub enum HelpFrontendEvent {
	#[serde(rename = "show_help")]
	ShowHelp(ShowHelpParams),

}

