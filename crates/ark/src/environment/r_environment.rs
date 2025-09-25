//
// r_environment.rs
//
// Copyright (C) 2025 by Lotas Inc.
//
//

use amalthea::comm::comm_channel::CommMsg;
use amalthea::comm::environment_comm::EnvironmentBackendReply;
use amalthea::comm::environment_comm::EnvironmentBackendRequest;
use amalthea::comm::environment_comm::InstallResult;
use amalthea::comm::environment_comm::ListPackagesPackageType;
use amalthea::comm::environment_comm::PackageInfo;
use amalthea::comm::environment_comm::UninstallResult;
use amalthea::socket::comm::CommSocket;
use anyhow::anyhow;
use crossbeam::select;
use harp::exec::RFunction;
use harp::exec::RFunctionExt;
use harp::RObject;
use log::info;
use log::trace;
use log::warn;
use stdext::spawn;

use crate::r_task;

/**
 * The R Environment handler provides the server side of the Environment panel.
 */
pub struct REnvironment {
    comm: CommSocket,
}

impl REnvironment {
    /**
     * Start the environment handler.
     *
     * - `comm`: The socket for communicating with the frontend.
     */
    pub fn start(comm: CommSocket) -> anyhow::Result<()> {
        // Start the environment thread and wait for requests from the frontend
        spawn!("ark-environment", move || {
            let environment = Self { comm };
            environment.execution_thread();
        });

        Ok(())
    }

    /**
     * The main environment execution thread; receives messages from the frontend
     * and processes them.
     */
    fn execution_thread(&self) {
        loop {
            // Wait for a message from the frontend
            select! {
                recv(&self.comm.incoming_rx) -> msg => {
                    match msg {
                        Ok(msg) => {
                            if !self.handle_comm_message(msg) {
                                info!("Environment comm {} closing by request from frontend.", self.comm.comm_id);
                                break;
                            }
                        },
                        Err(err) => {
                            // The connection with the frontend has been closed; let
                            // the thread exit.
                            warn!("Error receiving message from frontend: {:?}", err);
                            break;
                        },
                    }
                },
            }
        }
        trace!("Environment comm {} closed.", self.comm.comm_id);
    }

    /**
     * Handles a comm message from the frontend.
     *
     * Returns true if the thread should continue, false if it should exit.
     */
    fn handle_comm_message(&self, message: CommMsg) -> bool {
        if let CommMsg::Close = message {
            // The frontend has closed the connection; let the
            // thread exit.
            return false;
        }

        if self
            .comm
            .handle_request(message, |req| self.handle_rpc(req))
        {
            return true;
        }

        true
    }

    fn handle_rpc(&self, message: EnvironmentBackendRequest) -> anyhow::Result<EnvironmentBackendReply> {
        // Match on the type of data received.
        match message {
            EnvironmentBackendRequest::ListPackages(params) => {
                match self.list_packages(params.package_type) {
                    Ok(packages) => Ok(EnvironmentBackendReply::ListPackagesReply(packages)),
                    Err(err) => {
                        log::error!("Failed to list packages: {:?}", err);
                        Ok(EnvironmentBackendReply::ListPackagesReply(vec![]))
                    }
                }
            },
            EnvironmentBackendRequest::InstallPackage(params) => {
                match self.install_package(params.package_name, params.package_type, params.environment_type) {
                    Ok(result) => Ok(EnvironmentBackendReply::InstallPackageReply(result)),
                    Err(err) => {
                        log::error!("Failed to install package: {:?}", err);
                        Ok(EnvironmentBackendReply::InstallPackageReply(InstallResult {
                            success: false,
                            error: Some(format!("Failed to install package: {}", err)),
                        }))
                    }
                }
            },
            EnvironmentBackendRequest::UninstallPackage(params) => {
                match self.uninstall_package(params.package_name, params.package_type, params.environment_type) {
                    Ok(result) => Ok(EnvironmentBackendReply::UninstallPackageReply(result)),
                    Err(err) => {
                        log::error!("Failed to uninstall package: {:?}", err);
                        Ok(EnvironmentBackendReply::UninstallPackageReply(UninstallResult {
                            success: false,
                            error: Some(format!("Failed to uninstall package: {}", err)),
                        }))
                    }
                }
            },
        }
    }

    /// List installed packages
    #[tracing::instrument(level = "trace", skip(self))]
    fn list_packages(&self, package_type: ListPackagesPackageType) -> anyhow::Result<Vec<PackageInfo>> {
        match package_type {
            ListPackagesPackageType::R => self.list_r_packages(),
            ListPackagesPackageType::Python => self.list_python_packages(),
        }
    }

    /// List installed R packages
    fn list_r_packages(&self) -> anyhow::Result<Vec<PackageInfo>> {
        // Process the R result inside r_task to avoid thread safety issues
        let packages = r_task(|| -> anyhow::Result<Vec<PackageInfo>> {
            let result = RFunction::from("installed.packages")
                .call()?;
            
            // Parse the result inside the task to avoid sending RObject between threads
            self.parse_installed_packages_matrix(result)
        })?;

        Ok(packages)
    }

    /// List installed Python packages (when running in a Python runtime)
    fn list_python_packages(&self) -> anyhow::Result<Vec<PackageInfo>> {
        // This is the R runtime, so we can't directly list Python packages
        // In a real implementation, this would need to interface with Python
        Err(anyhow!("Python package listing not supported in R runtime"))
    }

    /// Parse the R installed.packages() matrix into our PackageInfo format
    fn parse_installed_packages_matrix(&self, matrix: RObject) -> anyhow::Result<Vec<PackageInfo>> {
        let mut packages = Vec::new();

        // Convert the matrix to a data frame for easier access
        let df = RFunction::from("as.data.frame")
            .param("x", matrix)
            .param("stringsAsFactors", false)
            .call()?;

        // Get the number of rows
        let nrows: i32 = RFunction::from("nrow")
            .param("x", df.clone())
            .call()
            .and_then(|x| x.try_into())?;

        for i in 1..=nrows {
            // Extract package info from each row
            let name: String = RFunction::from("[")
                .param("x", df.clone())
                .param("i", i)
                .param("j", "Package")
                .call()
                .and_then(|x| x.try_into())?;

            let version: String = RFunction::from("[")
                .param("x", df.clone())
                .param("i", i)
                .param("j", "Version")
                .call()
                .and_then(|x| x.try_into())?;

            let priority: Option<String> = RFunction::from("[")
                .param("x", df.clone())
                .param("i", i)
                .param("j", "Priority")
                .call()
                .and_then(|x| x.try_into())
                .ok();

            // Check if package is loaded
            let is_loaded = self.is_package_loaded(&name).unwrap_or(false);

            packages.push(PackageInfo {
                name,
                version,
                description: None, // Could extract from Description column if needed
                location: None,
                is_loaded: Some(is_loaded),
                priority,
                editable: None,
            });
        }

        Ok(packages)
    }

    /// Check if an R package is currently loaded
    fn is_package_loaded(&self, package_name: &str) -> anyhow::Result<bool> {
        let loaded_packages: Vec<String> = RFunction::from("loadedNamespaces")
            .call()
            .and_then(|x| x.try_into())?;

        Ok(loaded_packages.contains(&package_name.to_string()))
    }

    /// Install a package
    #[tracing::instrument(level = "trace", skip(self))]
    fn install_package(&self, package_name: String, package_type: amalthea::comm::environment_comm::InstallPackagePackageType, environment_type: Option<String>) -> anyhow::Result<InstallResult> {
        match package_type {
            amalthea::comm::environment_comm::InstallPackagePackageType::R => {
                // R doesn't have the same environment complexity as Python, so we ignore environment_type for now
                // In the future, this could be used to determine different R package repositories or installation methods
                self.install_r_package(package_name, environment_type)
            },
            amalthea::comm::environment_comm::InstallPackagePackageType::Python => {
                Err(anyhow!("Python package installation not supported in R runtime"))
            },
        }
    }

    /// Install an R package
    fn install_r_package(&self, package_name: String, _environment_type: Option<String>) -> anyhow::Result<InstallResult> {
        let result = r_task(|| -> anyhow::Result<()> {
            RFunction::from("install.packages")
                .param("pkgs", package_name.clone())
                .call()?;
            Ok(())
        });

        match result {
            Ok(_) => {
                Ok(InstallResult {
                    success: true,
                    error: None,
                })
            },
            Err(err) => {
                Ok(InstallResult {
                    success: false,
                    error: Some(format!("Failed to install R package {}: {}", package_name, err)),
                })
            }
        }
    }

    /// Uninstall a package
    #[tracing::instrument(level = "trace", skip(self))]
    fn uninstall_package(&self, package_name: String, package_type: amalthea::comm::environment_comm::UninstallPackagePackageType, environment_type: Option<String>) -> anyhow::Result<UninstallResult> {
        match package_type {
            amalthea::comm::environment_comm::UninstallPackagePackageType::R => {
                // R doesn't have the same environment complexity as Python, so we ignore environment_type for now
                self.uninstall_r_package(package_name, environment_type)
            },
            amalthea::comm::environment_comm::UninstallPackagePackageType::Python => {
                Err(anyhow!("Python package uninstallation not supported in R runtime"))
            },
        }
    }

    /// Uninstall an R package
    fn uninstall_r_package(&self, package_name: String, _environment_type: Option<String>) -> anyhow::Result<UninstallResult> {
        let result = r_task(|| -> anyhow::Result<()> {
            let _result = RFunction::from("remove.packages")
                .param("pkgs", package_name.clone())
                .call()?;
            Ok(())
        });

        match result {
            Ok(_) => {
                Ok(UninstallResult {
                    success: true,
                    error: None,
                })
            },
            Err(err) => {
                Ok(UninstallResult {
                    success: false,
                    error: Some(format!("Failed to uninstall R package {}: {}", package_name, err)),
                })
            }
        }
    }

}
