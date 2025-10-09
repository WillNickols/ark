#
# help.R
#
# Copyright (C) 2023-2025 Posit Software, PBC. All rights reserved.
#
#

options(help_type = "html")

# A wrapper around `help()` that works for our specific use cases:
# - Picks up devtools `help()` if the shim is on the search path.
# - Expects that `topic` and `package` don't require NSE and are just strings or `NULL`.
# - Works around a pkgload NSE bug that has been fixed, but many people won't have
#   (https://github.com/r-lib/pkgload/pull/267).
# - Hardcodes a request for HTML help results.
# - Searches all installed packages, not just loaded ones.
help <- function(topic, package = NULL) {
    if ("devtools_shims" %in% search()) {
        help <- as.environment("devtools_shims")[["help"]]
    } else {
        help <- utils::help
    }

    # Since `topic` and `package` are strings (or `NULL`), we wrap them in `()` to tell the
    # special NSE semantics of `help()` to evaluate them rather than deparse them.
    if (is.null(package)) {
        # Use an explicit `NULL` to ensure this always works with dev help
        # https://github.com/r-lib/pkgload/pull/267
        # Use try.all.packages = TRUE to search all installed packages, not just loaded ones
        help(topic = (topic), package = NULL, help_type = "html", try.all.packages = TRUE)
    } else {
        help(topic = (topic), package = (package), help_type = "html")
    }
}

#' Start R's dynamic HTTP help server; returns the chosen port (invisibly)
#'
#' If the help server is already started, the port already in use is returned
#' (due to using `start = NA`).
#' @export
.ps.help.startOrReconnectToHelpServer <- function() {
    suppressMessages(tools::startDynamicHelp(start = NA))
}

# Show help on a topic. Returns a logical value indicating whether help was
# found.
#' @export
.ps.help.showHelpTopic <- function(topic) {
    info <- split_topic(topic)
    topic <- info$topic
    package <- info$package

    # Try to find help on the topic.
    results <- help(topic, package)

    # If we found results of any kind, show them.
    # If we are running ark tests, don't show the results as this requires
    # `ps_browse_url()` which needs a full `RMain` instance.
    if (length(results) > 0 && !in_ark_tests()) {
        # Force selection of first result to avoid console menu
        if (length(results) > 1) {
            results <- results[1]
        }
        
        # If package wasn't explicitly specified, extract it from the help result path
        # and load the package namespace before displaying help (like Rao does)
        if (is.null(package)) {
            help_path <- as.character(results)[1]
            package <- basename(dirname(dirname(help_path)))
        }
        
        # Load the package namespace so help can be displayed properly
        if (!is.null(package) && nzchar(package)) {
            requireNamespace(package, quietly = TRUE)
            
            # Call help() again to get fresh results with the package now loaded
            # This ensures print() will display the help properly instead of showing
            # "not in any loaded package" message
            results <- help(topic, package)
        }
        
        print(results)
    }

    # Return whether we found any help.
    length(results) > 0
}

# Resolve the package specifier, if there is one
split_topic <- function(topic) {
    # Try `:::` first, as `::` will match both
    components <- strsplit(topic, ":::")[[1L]]
    if (length(components) > 1L) {
        package <- components[[1L]]
        topic <- components[[2L]]
        return(list(topic = topic, package = package))
    }

    components <- strsplit(topic, "::")[[1L]]
    if (length(components) > 1L) {
        package <- components[[1L]]
        topic <- components[[2L]]
        return(list(topic = topic, package = package))
    }

    list(topic = topic, package = NULL)
}

# Expose the show help topic function as an RPC.
#' @export
.ps.rpc.showHelpTopic <- .ps.help.showHelpTopic

# Show a vignette. Returns a logical value indicating whether the vignette
# was found.
#' @export
.ps.rpc.showVignetteTopic <- function(topic) {
    # Resolve the package specifier.
    package <- NULL
    components <- strsplit(topic, "::")[[1L]]
    if (length(components) > 1L) {
        package <- components[[1L]]
        topic <- components[[2L]]
    }

    # Try to find the vignette; suppress warnings so we don't pollute the
    # console.
    results <- suppressWarnings(utils::vignette(topic, package = package))

    # If we found a vignette, show it.
    if ("vignette" %in% class(results)) {
        print(results)
        TRUE
    } else {
        FALSE
    }
}

#' @export
.ps.help.getHtmlHelpContents <- function(topic, package = NULL) {
    # If a package name is encoded into 'topic', split that here.
    if (grepl(":{2,3}", topic)) {
        parts <- strsplit(topic, ":{2,3}")[[1L]]
        package <- parts[[1L]]
        topic <- parts[[2L]]
    }

    # Get the help file associated with this topic.
    helpFiles <- help(topic, package)

    if (inherits(helpFiles, "dev_topic")) {
        getHtmlHelpContentsDev(helpFiles)
    } else {
        getHtmlHelpContentsInstalled(helpFiles, package)
    }
}

getHtmlHelpContentsInstalled <- function(helpFiles, package) {
    if (length(helpFiles) == 0) {
        return(NULL)
    }

    # If there are multiple hits for the same topic, right now we just choose the first
    # (which I believe goes with the most recently loaded package)
    helpFile <- helpFiles[[1L]]

    rd <- utils:::.getHelpFile(helpFile)

    # Set 'package' now if it was unknown.
    if (is.null(package)) {
        package <- getPackageNameFromHelpPath(helpFile)
    }

    # If still unknown, set to `""` for `Rd2HTML()`
    if (is.null(package)) {
        package <- ""
    }

    # Convert to html.
    htmlFile <- tempfile(fileext = ".html")
    on.exit(unlink(htmlFile), add = TRUE)
    tools::Rd2HTML(rd, out = htmlFile, package = package)
    contents <- readLines(htmlFile, warn = FALSE)
    paste(contents, collapse = "\n")
}

getPackageNameFromHelpPath <- function(path) {
    # Help paths are always of the form:
    # <libpath>/<package>/help/<topic>
    #
    # `<libpath>/<package>/help` should be a real path on the file system,
    # and points to the folder where the Rd database is stored. Note that
    # the full path of `<libpath>/<package>/help/<topic>` actually does NOT
    # exist on the file system, it's just a convention!
    #
    # `utils:::.getHelpFile()` utilizes the fact that `<package>` is always
    # relative to `<topic>` in a specific way, so we should be safe to do the
    # same.
    path_help <- dirname(path)
    path_package <- dirname(path_help)

    if (!file.exists(path_package)) {
        # Guard against nonexistent packages
        return(NULL)
    }

    package <- basename(path_package)

    package
}

getHtmlHelpContentsDev <- function(x) {
    tryCatch(
        getHtmlHelpContentsDevImpl(x),
        error = function(e) NULL
    )
}

# pkgload specific dev help when looking up help for an internal function
# while working on a package
getHtmlHelpContentsDevImpl <- function(x) {
    if (!"pkgload" %in% loadedNamespaces()) {
        # Refuse if we somehow get a dev topic but pkgload isn't loaded
        return(NULL)
    }

    directory <- positron_tempdir("help")
    path <- file.path(directory, "dev-contents.html")

    package_root <- find_package_root(x$path)
    macros <- load_macros(package_root)

    # - `no_links = TRUE` because we don't click links while looking at docs on hover
    # - `dynamic = FALSE` because we want the HTML to be static for the hover provider, it isn't connected to a help server
    tools::Rd2HTML(
        x$path,
        out = path,
        package = x$pkg,
        stages = x$stage,
        no_links = TRUE,
        dynamic = FALSE,
        macros = macros
    )

    contents <- readLines(path, warn = FALSE)
    paste(contents, collapse = "\n")
}

#' @export
.ps.help.previewRd <- function(rd_file) {
    # `/preview` causes this to be handled by preview_rd() in ark's help proxy.
    url <- sprintf("/preview?file=%s", rd_file)
    port <- tools:::httpdPort()
    url <- tools:::dynamicHelpURL(url, port)
    .ps.Call("ps_browse_url", as.character(url))
}

# @param rd_file Path to an `.Rd` file.
# @returns The result of converting that `.Rd` to HTML and concatenating to a
#   string.
#' @export
.ps.Rd2HTML <- function(rd_file, package = "") {
    package_root <- find_package_root(rd_file)
    if (!is.null(package_root) && !nzchar(package)) {
        package_desc <- package_info(package_root)
        package <- package_desc$Package %||% ""
    }

    path <- tempfile(fileext = ".html")
    on.exit(unlink(path), add = TRUE)

    # Write HTML to file (with support for links and dynamic requests)
    macros <- load_macros(package_root)
    tools::Rd2HTML(
        rd_file,
        out = path,
        package = package,
        macros = macros,
        dynamic = TRUE
    )

    # Make tweaks to the returned HTML
    lines <- readLines(path, warn = FALSE)

    lines <- sub(
        "R Documentation</td></tr></table>",
        "(preview) R Documentation</td></tr></table>",
        lines
    )

    # This condition is meant to detect the scenario where we are clearly working
    # inside the source of a specific in-development package, which will almost
    # always be true.
    if (!is.null(package_root) && nzchar(package)) {
        # `/dev-figure` causes this to be handled by preview_img() in ark's help proxy.
        lines <- gsub(
            'img src="figures/([^"]*)"',
            sprintf(
                'img src="dev-figure?file=%s/man/figures/\\1"',
                package_root
            ),
            lines
        )

        # Rewrite links to other help topics.
        lines <- vapply(lines, rewrite_help_links, "", package, package_root)
    }

    paste0(lines, collapse = "\n")
}

load_macros <- function(package_root) {
    if (
        is.null(package_root) ||
            !file.exists(file.path(package_root, "DESCRIPTION"))
    ) {
        path <- file.path(R.home("share"), "Rd/macros/system.Rd")
        tools::loadRdMacros(path)
    } else {
        tools::loadPkgRdMacros(package_root)
    }
}

# @param path The path to a file believed to be inside an R source package.
#   Currently only used when we expect a path like
#   {package-root}/man/some_topic.Rd. But you could imagine doing a more general
#   recursive walk upwards, if we ever need that.
# @returns Normalized path to package root or NULL if we don't seem to be in a
#   package.
find_package_root <- function(path) {
    maybe_package_root <- dirname(dirname(path))

    if (file.exists(file.path(maybe_package_root, "DESCRIPTION"))) {
        normalizePath(maybe_package_root)
    } else {
        NULL
    }
}

# @param path Normalized path to package root.
# @returns A list containing the metadata in DESCRIPTION.
package_info <- function(path) {
    stopifnot(is_string(path))

    desc <- file.path(path, "DESCRIPTION")
    if (!file.exists(desc)) {
        stop("No DESCRIPTION found")
    }

    desc_mat <- read.dcf(desc)
    as.list(desc_mat[1, ])
}

# @param line A single line of HTML in a rendered help topic.
# @param package The name of the in-development package.
# @param package_root The normalized path to the source of the in-development
#   package.
# @returns The input `line`, with its help links rewritten.
rewrite_help_links <- function(line, package, package_root) {
    # inspired by an official example for regmatches()
    # gregexec() returns overlapping ranges: the first match is the full match,
    #   then the sub-matches follow (pkg and topic, for us)
    # when we use the `regmatches<-` assignment form, we want to keep only the
    #   coordinates for the first, full match
    keep_first <- function(x) {
        if (!anyNA(x) && all(x > 0)) {
            ml <- attr(x, 'match.length')
            it <- attr(x, 'index.type')
            ub <- attr(x, 'useBytes')
            if (is.matrix(x)) {
                x <- x[1, , drop = FALSE]
            } else {
                x <- x[1]
            }
            if (is.matrix(ml)) {
                attr(x, 'match.length') <- ml[1, , drop = FALSE]
            } else {
                attr(x, 'match.length') <- ml[1]
            }
            attr(x, 'index.type') <- it
            attr(x, 'useBytes') <- ub
        }
        x
    }

    # Accomplishes two goals:
    # 1. Determine if topic belongs to the in-development package.
    # 2. If so, account for the scenario where our nominal `topic` is actually an
    #    alias and we need the actual topic name.
    dev_topic_find <- function(topic) {
        # It should be exceedingly rare, but if we somehow get here in the absence
        # of pkgload, all bets are off re: rewriting links.
        if (!.ps.is_installed("pkgload")) {
            return(NA_character_)
        }

        tf <- pkgload::dev_topic_find(topic, dev_packages = package)
        if (is.null(tf)) {
            NA_character_
        } else {
            tf$path
        }
    }

    # Incoming links look like this:
    # href="../../PACKAGE/help/TOPIC"
    #
    # For a topic known to be in this OTHER_PACKAGE, we rewrite as:
    # href="/library/OTHER_PACKAGE/help/TOPIC"
    #
    # For a topic in this in-development PACKAGE, we rewrite like:
    # href="/preview?file=/normalized/path/to/source/of/PACKAGE/man/TOPIC.Rd"
    #
    # When we know the topic is not in this in-development PACKAGE, but we don't
    # know which other package it belongs to, we rewrite as:
    # href="/library/PACKAGE/help/TOPIC"
    # This is obviously wrong, but this is how we delegate the problem of
    # resolving the package back to the R help server.

    # concrete incoming examples:
    #                        dev  a href="../../devhelp/help/blarg">
    #   installed, known package  a href="../../rlang/help/abort">
    # installed, unknown package  a href="../../devhelp/help/match">
    pattern <- 'a href="../../(?<pkg>[^/]*)/help/(?<topic>[^/]*)">'

    x <- gregexec(pattern, line, perl = TRUE)
    rm <- regmatches(line, x)[[1]]

    if (length(rm) == 0) {
        return(line)
    }

    match_data <- as.data.frame(t(rm))

    maybe_dev_rd_path <- vapply(match_data$topic, dev_topic_find, "")

    replacement <- ifelse(
        is.na(maybe_dev_rd_path),
        sprintf(
            'a href="/library/%s/help/%s">',
            match_data$pkg,
            match_data$topic
        ),
        sprintf('a href="/preview?file=%s">', maybe_dev_rd_path)
    )
    regmatches(line, lapply(x, keep_first)) <- list(as.matrix(replacement))

    # concrete outgoing examples:
    #                        dev  a href="/preview?file=/Users/jenny/rrr/devhelp/man/blarg.Rd">
    #   installed, known package  a href="/library/rlang/help/abort">
    # installed, unknown package  a href="/library/devhelp/help/match">

    line
}

#' Convert a help page to an Rd object.
#'
#' Ported from btw:::help_to_rd()
help_to_rd <- function(help_page) {
    if (inherits(help_page, "dev_topic")) {
        rd_path <- help_page$path
        return(tools::parse_Rd(rd_path))
    }

    help_path <- as.character(help_page)
    rd_name <- basename(help_path)
    rd_package <- basename(dirname(dirname(help_path)))
    tools::Rd_db(rd_package)[[paste0(rd_name, ".Rd")]]
}


#' Format a help page as Markdown.
#'
#' Ported from btw:::format_help_page_markdown()
format_help_page_markdown <- function(
    help_page,
    ...,
    to = "markdown_strict+pipe_tables+backtick_code_blocks"
) {
    rd_obj <- help_to_rd(help_page)
    tmp_rd_file <- tempfile(fileext = ".html")

    tools::Rd2HTML(rd_obj, out = tmp_rd_file)

    pandoc_convert(
        tmp_rd_file,
        to = to,
        ...
    )
}

#' Get the help topic and package from a help page.
#'
#' Ported from btw:::help_package_topic()
help_package_topic <- function(help_page) {
    if (inherits(help_page, "dev_topic")) {
        return(list(
            topic = help_page$topic,
            resolved = help_page$path,
            package = help_page$pkg
        ))
    }

    # help() mainly returns a path to the un-aliased help topic
    # help("promise"): .../library/promises/help/promise
    # help("mutate_if", "dplyr"): .../library/dplyr/help/mutate_all
    topic <- attr(help_page, "topic", exact = TRUE)

    help_path <- as.character(help_page)

    # In the case where there are multiple matches, sort them so that the
    # raised error is deterministic
    package <- basename(dirname(dirname(help_path)))
    sort_indices <- rank(package, ties.method = "first")

    list(
        topic = rep(topic, length(help_path)),
        resolved = basename(help_path)[sort_indices],
        package = if (length(package)) package[sort_indices]
    )
}

# Initialize topics cache environment (exact port from Rao)
if (!exists("topicsEnv", envir = .GlobalEnv)) {
    assign("topicsEnv", new.env(parent = emptyenv()), envir = .GlobalEnv)
}

# Initialize complete topics cache and last package list
if (!exists("completeTopicsCache", envir = .GlobalEnv)) {
    assign("completeTopicsCache", character(0), envir = .GlobalEnv)
}
if (!exists("lastPackageList", envir = .GlobalEnv)) {
    assign("lastPackageList", character(0), envir = .GlobalEnv)
}

#' Escape special regex characters for safe pattern matching
#' Exact port of rao/src/cpp/session/modules/SessionCodeTools.R:escapeForRegex
.ps.escapeForRegex <- function(regex) {
    gsub("([\\-\\[\\]\\{\\}\\(\\)\\*\\+\\?\\.\\,\\\\\\^\\$\\|\\#\\s])", "\\\\\\1", regex, perl = TRUE)
}

#' Check if one string is a subsequence of another
#' Exact port of rao/src/cpp/core/StringUtils.cpp:isSubsequence algorithm in R
.ps.isSubsequence <- function(haystack, needle) {
    if (length(needle) == 0 || nchar(needle) == 0) return(rep(TRUE, length(haystack)))
    if (length(haystack) == 0) return(logical(0))
    
    # Vectorized implementation
    sapply(haystack, function(h) {
        if (nchar(h) == 0) return(FALSE)
        if (nchar(needle) > nchar(h)) return(FALSE)
        
        h_chars <- utf8ToInt(h)
        n_chars <- utf8ToInt(needle)
        
        h_idx <- 1L
        n_idx <- 1L
        h_len <- length(h_chars)
        n_len <- length(n_chars)
        
        while (h_idx <= h_len && n_idx <= n_len) {
            if (h_chars[h_idx] == n_chars[n_idx]) {
                n_idx <- n_idx + 1L
                if (n_idx > n_len) return(TRUE)
            }
            h_idx <- h_idx + 1L
        }
        FALSE
    }, USE.NAMES = FALSE)
}

#' Find subsequence match indices in a string
#' Exact port of rao/src/cpp/core/StringUtils.cpp:subsequenceIndices
.ps.subsequenceIndices <- function(haystack, needle) {
    if (nchar(needle) == 0) return(integer(0))
    if (nchar(haystack) == 0) return(integer(0))
    
    h_chars <- utf8ToInt(haystack)
    n_chars <- utf8ToInt(needle)
    
    result <- integer(length(n_chars))
    h_idx <- 1L
    
    for (i in seq_along(n_chars)) {
        found <- FALSE
        while (h_idx <= length(h_chars)) {
            if (h_chars[h_idx] == n_chars[i]) {
                result[i] <- h_idx - 1L  # 0-based indexing like C++
                h_idx <- h_idx + 1L
                found <- TRUE
                break
            }
            h_idx <- h_idx + 1L
        }
        if (!found) return(integer(0))
    }
    result
}

#' Score a single match for ranking
#' Exact port of rao/src/cpp/session/modules/SessionCodeSearch.cpp:scoreMatch
.ps.scoreMatch <- function(suggestion, query, is_file = FALSE) {
    # No penalty for perfect matches
    if (suggestion == query) return(0L)
    
    matches <- .ps.subsequenceIndices(suggestion, query)
    if (length(matches) == 0) return(.Machine$integer.max)
    
    total_penalty <- 0L
    s_chars <- utf8ToInt(suggestion)
    q_chars <- utf8ToInt(query)
    
    # Loop over the matches and assign a score
    for (j in seq_along(matches)) {
        match_pos <- matches[j] + 1L  # Convert to 1-based for R
        penalty <- match_pos
        
        # Less penalty if character follows special delimiter
        if (match_pos > 1L) {
            prev_char <- intToUtf8(s_chars[match_pos - 1L])
            if (prev_char %in% c("_", "-") || (!is_file && prev_char == ".")) {
                penalty <- j
            }
        }
        
        # Less penalty for perfect case match (reward case-sensitive match)
        if (match_pos <= length(s_chars) && j <= length(q_chars)) {
            if (s_chars[match_pos] == q_chars[j]) {
                penalty <- penalty - 1L
            }
        }
        
        total_penalty <- total_penalty + penalty
    }
    
    # Penalize files
    if (is_file) {
        total_penalty <- total_penalty + 1L
    }
    
    # Penalize unmatched characters
    unmatched_penalty <- (length(q_chars) - length(matches)) * length(q_chars)
    total_penalty <- total_penalty + as.integer(unmatched_penalty)
    
    total_penalty
}

#' Score multiple strings against a query
#' Exact port of rao/src/cpp/session/modules/SessionCodeSearch.cpp:rs_scoreMatches
.ps.scoreMatches <- function(suggestions, query) {
    if (length(suggestions) == 0) return(integer(0))
    sapply(suggestions, function(s) .ps.scoreMatch(s, query, FALSE), USE.NAMES = FALSE)
}

#' Search help topics by query string
#' Get current list of ALL installed packages for cache invalidation
.ps.getCurrentPackageList <- function() {
    # Get all library paths
    lib_paths <- .libPaths()
    
    # For each library, get all installed package directories
    all_pkgpaths <- unlist(lapply(lib_paths, function(lib) {
        # List all directories in this library
        pkgs <- list.dirs(lib, full.names = TRUE, recursive = FALSE)
        # Filter to only valid package directories (must have DESCRIPTION file)
        pkgs[file.exists(file.path(pkgs, "DESCRIPTION"))]
    }), use.names = FALSE)
    
    # Sort for consistent comparison
    sort(all_pkgpaths)
}

#' Discover all help topics with proper cache invalidation
#' Only rebuilds cache when package list changes (like Rao)
.ps.discoverAllHelpTopics <- function() {
    # Get current package list
    current_package_list <- .ps.getCurrentPackageList()
    
    # Check if package list has changed since last discovery
    cached_flat <- get("completeTopicsCache", envir = .GlobalEnv)
    last_package_list <- get("lastPackageList", envir = .GlobalEnv)
    
    # If we have cached topics and package list unchanged, use cached results
    if (length(cached_flat) > 0 && 
        identical(last_package_list, current_package_list)) {
        return(cached_flat)
    }
    
    # Package list changed - need to rebuild cache
    topics_env <- get("topicsEnv", envir = .GlobalEnv)
    rm(list = ls(envir = topics_env), envir = topics_env)
    
    # Read topics from packages (with per-package caching)
    topics <- lapply(current_package_list, function(pkgpath) tryCatch({
        aliases <- file.path(pkgpath, "help/aliases.rds")
        index <- file.path(pkgpath, "help/AnIndex")
        
        value <- if (file.exists(aliases)) {
            names(readRDS(aliases))
        } else if (file.exists(index)) {
            data <- read.table(index, sep = "\t")
            data[, 1]
        } else {
            NULL
        }
        
        # Cache the topics for this package
        if (!is.null(value)) {
            assign(pkgpath, value, envir = topics_env)
        }
        value
        
    }, error = function(e) NULL))
    
    # Build flat list and cache it
    flat <- unlist(topics, use.names = FALSE)
    assign("completeTopicsCache", flat, envir = .GlobalEnv)
    assign("lastPackageList", current_package_list, envir = .GlobalEnv)
    
    flat
}

#' @export
.ps.rpc.searchHelpTopics <- function(query = "") {
    # Get all topics (uses aggressive caching for performance)
    flat <- .ps.discoverAllHelpTopics()
    
    # Handle empty query
    if (!nzchar(query)) {
        return(flat[1:min(length(flat), 50)])  # Return first 50 like Python
    }
    
    # Order matches by subsequence match score (exact Rao algorithm)
    scores <- .ps.scoreMatches(tolower(flat), tolower(query))
    ordered <- flat[order(scores, nchar(flat))]
    matches <- unique(ordered[.ps.isSubsequence(tolower(ordered), tolower(query))])
    
    # Force first character to match, but allow typos after.
    # Also keep matches with one or more leading '.', so that e.g.
    # the prefix 'libpaths' can match '.libPaths'
    if (nzchar(query)) {
        first <- .ps.escapeForRegex(substring(query, 1L, 1L))
        pattern <- sprintf("^[.]*[%s]", first)
        matches <- grep(pattern, matches, value = TRUE, perl = TRUE, ignore.case = TRUE)
    }
    
    matches
}

#' Pre-warm the help cache by doing initial discovery
#' This should be called once on service startup to make searches fast
.ps.warmHelpCache <- function() {
    tryCatch({
        # Trigger discovery which will populate the cache
        .ps.discoverAllHelpTopics()
    }, error = function(e) {
        # If pre-warming fails, searches will still work but be slower initially
        invisible(NULL)
    })
}

# Pre-warm cache on module load (like Rao does)
.ps.warmHelpCache()
