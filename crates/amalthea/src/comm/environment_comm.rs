// @generated

/*---------------------------------------------------------------------------------------------
 *  Copyright (C) 2025 Lotas Inc. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

//
// AUTO-GENERATED from environment.json; do not edit.
//

use serde::Deserialize;
use serde::Serialize;

/// Package information structure
#[derive(Clone, Debug, Serialize, Deserialize, PartialEq)]
pub struct PackageInfo {
	/// Package name
	pub name: String,

	/// Package version
	pub version: String,

	/// Package description
	pub description: Option<String>,

	/// Package installation location
	pub location: Option<String>,

	/// Whether package is currently loaded (R only)
	pub is_loaded: Option<bool>,

	/// Package priority (R only)
	pub priority: Option<String>,

	/// Whether package is editable install (Python only)
	pub editable: Option<bool>,
}

/// Installation result structure
#[derive(Clone, Debug, Serialize, Deserialize, PartialEq)]
pub struct InstallResult {
	/// Whether installation was successful
	pub success: bool,

	/// Error message if installation failed
	pub error: Option<String>,
}

/// Uninstallation result structure
#[derive(Clone, Debug, Serialize, Deserialize, PartialEq)]
pub struct UninstallResult {
	/// Whether uninstallation was successful
	pub success: bool,

	/// Error message if uninstallation failed
	pub error: Option<String>,
}

/// Possible values for PackageType in ListPackages
#[derive(Clone, Debug, Serialize, Deserialize, PartialEq, strum_macros::Display)]
pub enum ListPackagesPackageType {
	#[serde(rename = "r")]
	#[strum(to_string = "r")]
	R,

	#[serde(rename = "python")]
	#[strum(to_string = "python")]
	Python,
}

/// Possible values for PackageType in InstallPackage
#[derive(Clone, Debug, Serialize, Deserialize, PartialEq, strum_macros::Display)]
pub enum InstallPackagePackageType {
	#[serde(rename = "r")]
	#[strum(to_string = "r")]
	R,

	#[serde(rename = "python")]
	#[strum(to_string = "python")]
	Python,
}

/// Possible values for PackageType in UninstallPackage
#[derive(Clone, Debug, Serialize, Deserialize, PartialEq, strum_macros::Display)]
pub enum UninstallPackagePackageType {
	#[serde(rename = "r")]
	#[strum(to_string = "r")]
	R,

	#[serde(rename = "python")]
	#[strum(to_string = "python")]
	Python,
}

/// Possible values for PackageType in PackagesChanged
#[derive(Clone, Debug, Serialize, Deserialize, PartialEq, strum_macros::Display)]
pub enum PackagesChangedPackageType {
	#[serde(rename = "r")]
	#[strum(to_string = "r")]
	R,

	#[serde(rename = "python")]
	#[strum(to_string = "python")]
	Python,
}

/// Parameters for the ListPackages method.
#[derive(Clone, Debug, Serialize, Deserialize, PartialEq)]
pub struct ListPackagesParams {
	/// Type of packages to list
	pub package_type: ListPackagesPackageType,
}

/// Parameters for the InstallPackage method.
#[derive(Clone, Debug, Serialize, Deserialize, PartialEq)]
pub struct InstallPackageParams {
	/// Name of package to install
	pub package_name: String,

	/// Type of package manager to use
	pub package_type: InstallPackagePackageType,
}

/// Parameters for the UninstallPackage method.
#[derive(Clone, Debug, Serialize, Deserialize, PartialEq)]
pub struct UninstallPackageParams {
	/// Name of package to uninstall
	pub package_name: String,

	/// Type of package manager to use
	pub package_type: UninstallPackagePackageType,
}

/// Parameters for the PackagesChanged event.
#[derive(Clone, Debug, Serialize, Deserialize, PartialEq)]
pub struct PackagesChangedParams {
	/// Type of packages that changed
	pub package_type: PackagesChangedPackageType,
}

/**
 * Backend RPC request types for the environment comm
 */
#[derive(Clone, Debug, Serialize, Deserialize, PartialEq)]
#[serde(tag = "method", content = "params")]
pub enum EnvironmentBackendRequest {
	/// List installed packages for the current runtime
	#[serde(rename = "list_packages")]
	ListPackages(ListPackagesParams),

	/// Install a package
	#[serde(rename = "install_package")]
	InstallPackage(InstallPackageParams),

	/// Uninstall a package
	#[serde(rename = "uninstall_package")]
	UninstallPackage(UninstallPackageParams),
}

/**
 * Backend RPC Reply types for the environment comm
 */
#[derive(Clone, Debug, Serialize, Deserialize, PartialEq)]
#[serde(tag = "method", content = "result")]
pub enum EnvironmentBackendReply {
	/// Returns array of installed packages with their versions and metadata
	ListPackagesReply(Vec<PackageInfo>),

	/// Result of package installation
	InstallPackageReply(InstallResult),

	/// Result of package uninstallation
	UninstallPackageReply(UninstallResult),
}

/**
 * Frontend RPC request types for the environment comm
 */
#[derive(Clone, Debug, Serialize, Deserialize, PartialEq)]
#[serde(tag = "method", content = "params")]
pub enum EnvironmentFrontendRequest {
}

/**
 * Frontend RPC Reply types for the environment comm
 */
#[derive(Clone, Debug, Serialize, Deserialize, PartialEq)]
#[serde(tag = "method", content = "result")]
pub enum EnvironmentFrontendReply {
}

/**
 * Frontend events for the environment comm
 */
#[derive(Clone, Debug, Serialize, Deserialize, PartialEq)]
#[serde(tag = "method", content = "params")]
pub enum EnvironmentFrontendEvent {
	/// Notify when packages have changed
	#[serde(rename = "packages_changed")]
	PackagesChanged(PackagesChangedParams),
}

