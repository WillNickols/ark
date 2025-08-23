#
# llm_tools.R
#
# Copyright (C) 2025 Posit Software, PBC. All rights reserved.
#
#

#' Get the help topics for a package
#'
#' This function retrieves the help topics for a specified package in R.
#' It returns a data frame with the topic ID, title, and aliases for each help
#' topic in the package.
#'
#' Adapted from btw::btw_tool_docs_package_help_topics
#'
#' @param package_name Name of the package to get help topics for
#' @return A list of help topics for the package, each with a topic ID,
#'   title, and aliases.
#'
#' @export
.ps.rpc.list_package_help_topics <- function(package_name) {
    # Check if the package is installed
    if (!requireNamespace(package_name, quietly = TRUE)) {
        return(paste("Package", package_name, "is not installed."))
    }

    # Search for help topics in the package
    help_db <- utils::help.search(
        "",
        package = package_name,
        fields = c("alias", "title"),
        ignore.case = TRUE
    )
    res <- help_db$matches

    # Did we get any matches?
    if (nrow(res) == 0) {
        return(paste("No help topics found for package", package_name, "."))
    }

    res_split <- split(res, res$Name)
    res_list <- lapply(res_split, function(group) {
        list(
            topic_id = group$Name[1],
            title = group$Entry[group$Field == "Title"][1],
            aliases = paste(
                group$Entry[group$Field == "alias"],
                collapse = ", "
            )
        )
    })
    names(res_list) <- NULL
    res_list
}

#' Get the version of installed packages
#'
#' This function retrieves the versions of specified packages in R.
#'
#' It returns a named list where the names are the package names and the values
#' are the corresponding package versions, or "Not installed" if the package is
#' not found.
#'
#' @export
.ps.rpc.get_package_versions <- function(package_names, ...) {
    lapply(set_names(package_names), function(pkg) {
        if (is_on_disk(pkg)) {
            as.character(utils::packageVersion(pkg))
        } else {
            "Not installed"
        }
    })
}

#' Get available vignettes for a package
#'
#' This function retrieves the vignettes available for a specified package in R.
#' It returns a list of vignettes, each with a title and topic.
#'
#' Adapted from btw::btw_tool_docs_available_vignettes.
#'
#' @param package_name Name of the package to get vignettes for
#' @return A list of vignettes for the package, each with a title and topic.
#'
#' @export
.ps.rpc.list_available_vignettes <- function(package_name) {
    # Check if the package is installed
    if (!requireNamespace(package_name, quietly = TRUE)) {
        return(paste("Package", package_name, "is not installed."))
    }

    # Get vignettes for the package
    vignettes <- tools::getVignetteInfo(package = package_name)
    if (length(vignettes) == 0) {
        return(paste("Package", package_name, "has no vignettes."))
    }

    # Convert the matrix to a list of lists
    vignette_list <- lapply(seq_len(nrow(vignettes)), function(i) {
        list(
            title = vignettes[i, "Title"],
            topic = vignettes[i, "Topic"]
        )
    })
    vignette_list
}

#' Get a specific vignette for a package
#'
#' This function retrieves a specific vignette available for a specified package in R.
#' It returns the vignette content as a Markdown character string.
#'
#' Adapted from btw::btw_tool_docs_vignette.
#'
#' @param package_name Name of the package to get vignettes for
#' @return A list of vignettes for the package, each with a title and topic.
#'
#' @export
.ps.rpc.get_package_vignette <- function(package_name, vignette) {
    vignettes <- as.data.frame(tools::getVignetteInfo(package = package_name))
    if (nrow(vignettes) == 0) {
        return(paste("Package", package_name, "has no vignettes."))
    }
    vignette_info <- vignettes[vignettes$Topic == vignette, , drop = FALSE]
    if (nrow(vignette_info) == 0) {
        return(
            paste(
                "No vignette",
                vignette,
                "for package",
                package_name,
                "found."
            )
        )
    }

    # Use Pandoc (bundled with Positron) to convert rendered vignette (PDF or
    # HTML) to Markdown
    output_file <- tempfile(fileext = ".md")
    tryCatch(
        {
            pandoc_convert(
                input = file.path(vignette_info$Dir, "doc", vignette_info$PDF),
                to = "markdown",
                output = output_file,
                verbose = FALSE
            )
            # read the converted Markdown file
            vignette_md <- readLines(output_file, warn = FALSE)

            # remove the first line which is the title
            vignette_md <- vignette_md[-1]
            vignette_md <- paste(vignette_md, collapse = "\n")
            vignette_md
        },
        error = function(e) {
            paste("Error converting vignette:", e$message)
        }
    )
}


#' Get a specific help page
#'
#' This function retrieves a specific help page available for a specified package in R.
#' It returns the help page content as a Markdown character string.
#'
#' Adapted from btw::btw_tool_docs_help_page.
#'
#' @param topic The topic to get help for
#' @param package_name The name of the package to get help for. If empty,
#' searches all installed packages.
#' @return A list of help pages for the package, each with a title and topic.
#'
#' @export
.ps.rpc.get_help_page <- function(topic, package_name = "") {
    if (identical(package_name, "")) {
        package_name <- NULL
    }

    if (!is.null(package_name)) {
        if (!requireNamespace(package_name, quietly = TRUE)) {
            return(paste("Package", package_name, "is not installed."))
        }
    }

    # Temporarily disable menu graphics
    old.menu.graphics <- getOption("menu.graphics", default = TRUE)
    options(menu.graphics = FALSE)
    on.exit(options(menu.graphics = old.menu.graphics), add = TRUE)

    # Read the help page
    help_page <- utils::help(
        package = (package_name),
        topic = (topic),
        help_type = "text",
        try.all.packages = (is.null(package_name))
    )

    if (!length(help_page)) {
        return(
            paste0(
                "No help page found for topic ",
                topic,
                if (!is.null(package_name)) {
                    paste(" in package", package_name)
                } else {
                    " in all installed packages"
                },
                "."
            )
        )
    }

    # Resolve the help page to a specific topic and package
    resolved <- help_package_topic(help_page)

    if (length(resolved$resolved) > 1) {
        calls <- sprintf(
            '{"topic":"%s", "package_name":"%s"}',
            resolved$resolved,
            resolved$package
        )
        calls <- stats::setNames(calls, "*")
        return(
            paste(
                "Topic",
                topic,
                "matched",
                length(resolved$resolved),
                "different topics. Choose one or submit individual tool calls for each topic.",
            )
        )
    }

    # Convert the help page to Markdown using Pandoc
    md_file <- tempfile(fileext = ".md")
    format_help_page_markdown(
        help_page,
        output = md_file,
        options = c("--shift-heading-level-by=1")
    )
    md <- readLines(md_file, warn = FALSE)

    # Remove up to the first empty line
    first_empty <- match(TRUE, !nzchar(md), nomatch = 1) - 1
    if (first_empty > 0) {
        md <- md[-seq_len(first_empty)]
    }

    # Add a heading for the help page
    heading <- sprintf(
        "## `help(package = \"%s\", \"%s\")`",
        resolved$package,
        topic
    )

    # Return the help page as a list
    list(
        help_text = paste0(md, collapse = "\n"),
        topic = basename(resolved$topic),
        package = resolved$package
    )
}

#' Suggest R help topics based on a query string
#' 
#' This function searches through all available R help topics and returns
#' matches for the given query. It's designed to be called via ARK's
#' callMethod interface as 'suggest_topics'.
#'
#' @param query Search query string to filter topics
#' @return Character vector of matching help topics
#' @export
.ps.rpc.suggest_topics <- function(query = "") {
    tryCatch({
        # Use RStudio's approach: read help aliases directly from packages
        # This finds ALL function names including base R functions like rnorm
        
        pkgpaths <- base::path.package(quiet = TRUE)
        
        # Read topics from all package help aliases
        all_topics <- character(0)
        
        for (pkgpath in pkgpaths) {
            tryCatch({
                # Try aliases.rds first (preferred)
                aliases_path <- base::file.path(pkgpath, "help/aliases.rds")
                index_path <- base::file.path(pkgpath, "help/AnIndex")
                
                topics <- if (base::file.exists(aliases_path)) {
                    base::names(base::readRDS(aliases_path))
                } else if (base::file.exists(index_path)) {
                    data <- utils::read.table(index_path, sep = "\t")
                    data[, 1]
                } else {
                    character(0)
                }
                
                if (length(topics) > 0) {
                    all_topics <- c(all_topics, topics)
                }
                
            }, error = function(e) {
                # Skip packages that fail to read
            })
        }
        
        # Remove duplicates
        all_topics <- unique(all_topics)
        
        if (length(all_topics) == 0) {
            return(character(0))
        }
        
        # Handle empty query case
        if (is.null(query) || identical(query, "") || !nzchar(query)) {
            # Return common topics
            common_topics <- c("help", "library", "data", "plot", "summary", "str", "head", "tail", "mean", "median")
            available_common <- common_topics[common_topics %in% all_topics]
            return(utils::head(c(available_common, all_topics), 50))
        }
        
        # Apply RStudio-style filtering and scoring
        query_lower <- base::tolower(query)
        all_topics_lower <- base::tolower(all_topics)
        
        # First filter: only keep subsequence matches (like RStudio does)
        subsequence_matches <- character(0)
        for (i in seq_along(all_topics)) {
            topic_lower <- all_topics_lower[i]
            if (.rs.isSubsequence(topic_lower, query_lower)) {
                subsequence_matches <- c(subsequence_matches, all_topics[i])
            }
        }
        
        if (length(subsequence_matches) == 0) {
            return(character(0))
        }
        
        # Score matches (RStudio-style scoring)
        scores <- numeric(length(subsequence_matches))
        for (i in seq_along(subsequence_matches)) {
            topic_lower <- base::tolower(subsequence_matches[i])
            scores[i] <- .rs.scoreMatch(topic_lower, query_lower)
        }
        
        # Order by score (lower is better in RStudio scoring), then by length
        ordered_indices <- base::order(scores, base::nchar(subsequence_matches))
        ordered_topics <- subsequence_matches[ordered_indices]
        
        # Apply first character filter like RStudio does
        if (base::nchar(query) > 0) {
            first_char <- base::substring(query, 1L, 1L)
            # Allow leading dots (for functions like .libPaths)
            pattern <- base::sprintf("^[.]*[%s]", first_char)
            ordered_topics <- base::grep(pattern, ordered_topics, value = TRUE, ignore.case = TRUE)
        }
        
        # Return top matches
        result <- utils::head(unique(ordered_topics), 50)
        return(result)
        
    }, error = function(e) {
        return(character(0))
    })
}

# Helper function: check if needle is a subsequence of haystack
.rs.isSubsequence <- function(haystack, needle) {
    if (base::nchar(needle) == 0) return(TRUE)
    if (base::nchar(haystack) == 0) return(FALSE)
    
    haystack_chars <- base::strsplit(haystack, "")[[1]]
    needle_chars <- base::strsplit(needle, "")[[1]]
    
    haystack_idx <- 1
    for (needle_char in needle_chars) {
        found <- FALSE
        while (haystack_idx <= length(haystack_chars)) {
            if (haystack_chars[haystack_idx] == needle_char) {
                haystack_idx <- haystack_idx + 1
                found <- TRUE
                break
            }
            haystack_idx <- haystack_idx + 1
        }
        if (!found) return(FALSE)
    }
    return(TRUE)
}

# Helper function: score a match (lower scores are better)
.rs.scoreMatch <- function(suggestion, query) {
    # Perfect match gets score 0
    if (suggestion == query) return(0)
    
    # Find subsequence match positions
    match_positions <- .rs.findSubsequencePositions(suggestion, query)
    if (length(match_positions) == 0) return(999999)  # No match
    
    total_penalty <- 0
    
    # Score based on match positions
    for (i in seq_along(match_positions)) {
        pos <- match_positions[i]
        penalty <- pos  # Position penalty (later positions cost more)
        
        # Reduce penalty for matches after delimiters
        if (pos > 1) {
            prev_char <- base::substring(suggestion, pos - 1, pos - 1)
            if (prev_char %in% c("_", "-", ".")) {
                penalty <- i  # Much lower penalty
            }
        }
        
        # Reward exact case matches
        suggestion_char <- base::substring(suggestion, pos, pos)
        query_char <- base::substring(query, i, i)
        if (suggestion_char == query_char) {
            penalty <- penalty - 1
        }
        
        total_penalty <- total_penalty + penalty
    }
    
    # Penalty for unmatched characters in query
    unmatched_chars <- base::nchar(query) - length(match_positions)
    total_penalty <- total_penalty + (unmatched_chars * base::nchar(query))
    
    return(total_penalty)
}

# Helper function: find positions of subsequence matches
.rs.findSubsequencePositions <- function(haystack, needle) {
    if (base::nchar(needle) == 0) return(integer(0))
    if (base::nchar(haystack) == 0) return(integer(0))
    
    haystack_chars <- base::strsplit(haystack, "")[[1]]
    needle_chars <- base::strsplit(needle, "")[[1]]
    
    positions <- integer(0)
    haystack_idx <- 1
    
    for (needle_char in needle_chars) {
        found_pos <- 0
        while (haystack_idx <= length(haystack_chars)) {
            if (haystack_chars[haystack_idx] == needle_char) {
                found_pos <- haystack_idx
                haystack_idx <- haystack_idx + 1
                break
            }
            haystack_idx <- haystack_idx + 1
        }
        if (found_pos == 0) return(integer(0))  # No match found
        positions <- c(positions, found_pos)
    }
    
    return(positions)
}

#' Extract function calls from R code
#' 
#' Parses R code and extracts all function calls using R's built-in parse() and getParseData().
#' This provides the same functionality as Rao's extract_r_functions for Positron's function parsing.
#' 
#' @param r_code Character string containing R code to parse
#' @return List with 'functions' (character vector of function names) and 'success' (logical)
#' @export
.ps.rpc.extract_r_functions <- function(r_code) {
    base::tryCatch({
        if (base::is.null(r_code) || base::is.na(r_code) || base::nchar(base::trimws(r_code)) == 0) {
            return(base::list(functions = base::character(0), detailed = base::list(), success = TRUE))
        }
        
        # Clean the R code (remove backticks with optional language specifiers)
        trimmed_code <- base::gsub("^```[a-zA-Z]*\\s*\\n?", "", r_code, perl = TRUE)
        trimmed_code <- base::gsub("\\n?```\\s*$", "", trimmed_code, perl = TRUE)
        trimmed_code <- base::gsub("```\\n", "", trimmed_code, perl = TRUE)
        trimmed_code <- base::trimws(trimmed_code)
        
        if (base::nchar(trimmed_code) == 0) {
            return(base::list(functions = base::character(0), detailed = base::list(), success = TRUE))
        }
        
        # Parse the R code
        expr <- base::parse(text = trimmed_code, keep.source = TRUE)
        if (base::length(expr) == 0) {
            return(base::list(functions = base::character(0), detailed = base::list(), success = TRUE))
        }
        
        # Get parse data - explicitly use utils namespace
        parse_data <- utils::getParseData(expr)
        if (base::is.null(parse_data) || base::nrow(parse_data) == 0) {
            return(base::list(functions = base::character(0), detailed = base::list(), success = TRUE))
        }
        
        # Extract function calls and assignment operators
        function_tokens <- base::c("SYMBOL_FUNCTION_CALL", "SPECIAL", "LEFT_ASSIGN", "RIGHT_ASSIGN", "EQ_ASSIGN")
        functions <- parse_data[parse_data$token %in% function_tokens, ]
        
        # Get unique function names
        function_names <- functions$text
        function_names <- function_names[!base::is.na(function_names) & base::nchar(function_names) > 0]
        function_names <- base::unique(function_names)
        function_names <- base::sort(function_names)
        
        # Create detailed information for each function call
        detailed <- base::lapply(base::seq_len(base::nrow(functions)), function(i) {
            base::list(
                name = functions$text[i],
                type = "function",
                line = functions$line1[i],
                column = functions$col1[i]
            )
        })
        
        return(base::list(
            functions = function_names,
            detailed = detailed,
            success = TRUE
        ))
        
    }, error = function(e) {
        return(base::list(
            functions = base::character(0),
            detailed = base::list(),
            success = FALSE,
            error = base::as.character(e)
        ))
    })
}
