#
# package.R
#
# Copyright (C) 2023-2025 Posit Software, PBC. All rights reserved.
#
#

# Checks if a package is installed without loading it.
# Could be slow on network drives.
#' @export
.ps.is_installed <- function(pkg, minimum_version = NULL) {
    installed <- system.file(package = pkg) != ""

    if (installed && !is.null(minimum_version)) {
        installed <- utils::packageVersion(pkg) >= minimum_version
    }

    installed
}

#' @export
.ps.rpc.is_installed <- .ps.is_installed

# Returns a list containing:
#   * the version string if the package is installed and NULL otherwise
#   * a logical indicating if package is installed at or above the minimum version
#  This may seem weird, but it's impractical for positron-r to do version
#  comparisons.
#' @export
.ps.rpc.packageVersion <- function(pkg, minimumVersion = NULL) {
    installed <- system.file(package = pkg) != ""

    if (installed) {
        version <- utils::packageVersion(pkg)
        list(
            version = as.character(version),
            compatible = is.null(minimumVersion) || version >= minimumVersion
        )
    } else {
        list(
            version = NULL,
            compatible = FALSE
        )
    }
}

#' @export
.ps.rpc.install_packages <- function(packages) {
    for (pkg in packages) {
        if (.ps.rpc.isPackageAttached(pkg)) {
            stop("Should not install a package if it's already attached.")
        }
    }
    utils::install.packages(unlist(packages))
    TRUE
}

#' @export
.ps.rpc.isPackageAttached <- function(pkg) {
    if (!is_string(pkg)) {
        stop("`pkg` must be a string.")
    }

    pkg %in% .packages()
}

#' @export
.ps.rpc.get_attached_packages <- function(...) {
    .packages()
}

#' Check which packages are used but not installed
#' @param content The file content to parse
#' @param file_path The file path (used to detect file type)
#' @export
.ps.check_missing_packages <- function(content, file_path = "<unknown>") {
    # Detect file type from path
    file_type <- .ps.detect_file_type(file_path)
    
    # Extract packages based on file type
    if (file_type == "rmd") {
        packages <- .ps.extract_packages_from_rmd(content)
    } else if (file_type == "qmd") {
        packages <- .ps.extract_packages_from_qmd(content)
    } else if (file_type == "rnw") {
        packages <- .ps.extract_packages_from_rnw(content)
    } else {
        packages <- .ps.extract_packages_from_r_script(content)
    }
    
    # Check which aren't installed
    missing <- character(0)
    for (pkg in unique(packages)) {
        if (!.ps.is_installed(pkg)) {
            missing <- c(missing, pkg)
        }
    }
    
    missing
}

#' Detect file type from file path
.ps.detect_file_type <- function(file_path) {
    if (is.null(file_path) || file_path == "" || file_path == "<unknown>") {
        return("r")
    }
    
    if (grepl("\\.Rmd$|\\.rmd$", file_path, ignore.case = TRUE)) {
        return("rmd")
    } else if (grepl("\\.qmd$", file_path, ignore.case = TRUE)) {
        return("qmd")
    } else if (grepl("\\.Rnw$|\\.rnw$", file_path, ignore.case = TRUE)) {
        return("rnw")
    } else {
        return("r")
    }
}

#' Extract package names from an R expression
.ps.extract_packages_from_expr <- function(expr) {
    if (!is.call(expr)) return(character(0))
    
    packages <- character(0)
    fn_name <- tryCatch(as.character(expr[[1]])[1], error = function(e) "")
    
    # Check for library() or require()
    if (fn_name %in% c("library", "require") && length(expr) >= 2) {
        # Extract the package name - it might be quoted or unquoted
        pkg <- tryCatch({
            pkg_expr <- expr[[2]]
            if (is.character(pkg_expr)) {
                pkg_expr
            } else {
                as.character(pkg_expr)
            }
        }, error = function(e) NULL)
        if (!is.null(pkg) && length(pkg) > 0 && nchar(pkg[1]) > 0) {
            packages <- c(packages, pkg[1])
        }
    }
    
    # Check for package::function or package:::function
    # Two cases:
    # 1. pkg::func (just the namespace access) - expr is the :: call itself
    # 2. pkg::func(args) (function call) - expr[[1]] is the :: call
    
    # Case 1: expr itself is a :: or ::: call (e.g., just `pkg::func`)
    # In this case, fn_name will be "::" or ":::" 
    # BUT we need to make sure expr[[1]] is NOT a call (otherwise it's case 2)
    if (fn_name %in% c("::", ":::") && !is.call(expr[[1]]) && length(expr) >= 2) {
        pkg <- tryCatch(as.character(expr[[2]])[1], error = function(e) NULL)
        if (!is.null(pkg) && nchar(pkg) > 0) {
            packages <- c(packages, pkg)
        }
    }
    
    # Case 2: expr[[1]] (the function being called) is itself a :: call
    # This handles cases like fakepackage::pivot_longer(df)
    if (is.call(expr[[1]])) {
        fn_call <- expr[[1]]
        fn_call_name <- tryCatch(as.character(fn_call[[1]])[1], error = function(e) "")
        if (fn_call_name %in% c("::", ":::") && length(fn_call) >= 2) {
            pkg <- tryCatch(as.character(fn_call[[2]])[1], error = function(e) NULL)
            if (!is.null(pkg) && nchar(pkg) > 0) {
                packages <- c(packages, pkg)
            }
        }
    }
    
    # Check for requireNamespace()
    if (fn_name == "requireNamespace" && length(expr) >= 2) {
        pkg <- tryCatch({
            pkg_expr <- expr[[2]]
            if (is.character(pkg_expr)) {
                pkg_expr
            } else {
                as.character(pkg_expr)
            }
        }, error = function(e) NULL)
        if (!is.null(pkg) && length(pkg) > 0 && nchar(pkg[1]) > 0) {
            packages <- c(packages, pkg[1])
        }
    }
    
    # Recurse into sub-expressions to find more package references
    # Start from index 2 to skip expr[[1]] (the function), only check arguments
    if (length(expr) > 1) {
        for (i in 2:length(expr)) {
            if (is.call(expr[[i]])) {
                packages <- c(packages, .ps.extract_packages_from_expr(expr[[i]]))
            }
        }
    }
    
    packages
}

#' Extract packages from R script content
.ps.extract_packages_from_r_script <- function(content) {
    # Parse the R code
    expr <- tryCatch(parse(text = content), error = function(e) {
        NULL
    })
    if (is.null(expr)) return(character(0))
    
    packages <- character(0)
    
    # Walk the AST to find package references
    for (e in expr) {
        packages <- c(packages, .ps.extract_packages_from_expr(e))
    }
    
    # Filter out base R packages
    packages <- .ps.filter_base_packages(packages)
    
    unique(sort(packages))
}

#' Extract packages from R Markdown content
.ps.extract_packages_from_rmd <- function(content) {
    lines <- strsplit(content, "\n")[[1]]
    
    # Extract R code chunks
    r_code <- .ps.extract_r_chunks(lines, "^\\s*```+\\s*\\{[rR]\\b.*\\}\\s*$", "^\\s*```+\\s*$")
    
    # Parse YAML frontmatter for runtime/server specifications
    yaml_packages <- .ps.extract_packages_from_yaml(lines)
    
    # Parse R code
    code_packages <- .ps.extract_packages_from_r_script(r_code)
    
    # R Markdown always requires rmarkdown
    all_packages <- unique(c("rmarkdown", yaml_packages, code_packages))
    
    # Filter out base R packages
    all_packages <- .ps.filter_base_packages(all_packages)
    
    unique(sort(all_packages))
}

#' Extract packages from Quarto Markdown content
.ps.extract_packages_from_qmd <- function(content) {
    lines <- strsplit(content, "\n")[[1]]
    
    # Extract R code chunks (Quarto uses same syntax as RMD)
    r_code <- .ps.extract_r_chunks(lines, "^\\s*```+\\s*\\{[rR]\\b.*\\}\\s*$", "^\\s*```+\\s*$")
    
    # Parse YAML frontmatter
    yaml_packages <- .ps.extract_packages_from_yaml(lines)
    
    # If there's no R code, return empty (don't require rmarkdown for non-R Quarto docs)
    if (nchar(trimws(r_code)) == 0) {
        return(character(0))
    }
    
    # Parse R code
    code_packages <- .ps.extract_packages_from_r_script(r_code)
    
    # Quarto with R code requires rmarkdown
    all_packages <- unique(c("rmarkdown", yaml_packages, code_packages))
    
    # Filter out base R packages
    all_packages <- .ps.filter_base_packages(all_packages)
    
    unique(sort(all_packages))
}

#' Extract packages from Sweave/Rnw content
.ps.extract_packages_from_rnw <- function(content) {
    lines <- strsplit(content, "\n")[[1]]
    
    # Extract R code chunks (Sweave format)
    r_code <- .ps.extract_r_chunks(lines, "^\\s*<<.*>>=.*$", "^\\s*@\\s*(%+.*|)$")
    
    # Parse R code
    packages <- .ps.extract_packages_from_r_script(r_code)
    
    # Filter out base R packages
    packages <- .ps.filter_base_packages(packages)
    
    unique(sort(packages))
}

#' Extract R code chunks from lines
.ps.extract_r_chunks <- function(lines, chunk_start_pattern, chunk_end_pattern) {
    in_r_chunk <- FALSE
    r_code <- character(0)
    
    for (line in lines) {
        if (!in_r_chunk && grepl(chunk_start_pattern, line, perl = TRUE)) {
            in_r_chunk <- TRUE
        } else if (in_r_chunk && grepl(chunk_end_pattern, line, perl = TRUE)) {
            in_r_chunk <- FALSE
        } else if (in_r_chunk) {
            r_code <- c(r_code, line)
        }
    }
    
    paste(r_code, collapse = "\n")
}

#' Extract package requirements from YAML frontmatter
.ps.extract_packages_from_yaml <- function(lines) {
    # Find YAML frontmatter (between --- lines at start)
    if (length(lines) == 0 || !grepl("^---\\s*$", lines[1])) {
        return(character(0))
    }
    
    # Find end of YAML
    yaml_end <- 0
    for (i in 2:length(lines)) {
        if (grepl("^---\\s*$", lines[i]) || grepl("^\\.\\.\\.$", lines[i])) {
            yaml_end <- i
            break
        }
    }
    
    if (yaml_end == 0) {
        return(character(0))
    }
    
    yaml_lines <- lines[2:(yaml_end - 1)]
    packages <- character(0)
    
    # Check for runtime: shiny or server: shiny
    for (line in yaml_lines) {
        if (grepl("^\\s*runtime:\\s*shiny", line, ignore.case = TRUE) ||
            grepl("^\\s*runtime:\\s*shinyrmd", line, ignore.case = TRUE) ||
            grepl("^\\s*runtime:\\s*shiny_prerendered", line, ignore.case = TRUE) ||
            grepl("^\\s*server:\\s*shiny", line, ignore.case = TRUE)) {
            packages <- c(packages, "shiny")
        }
    }
    
    packages
}

#' Filter out base R packages that are always available
.ps.filter_base_packages <- function(packages) {
    base_packages <- c("base", "stats", "utils", "graphics", "grDevices", "methods", "datasets", "tools")
    setdiff(packages, base_packages)
}
