#
# r_parser.R
#
# Copyright (C) 2025 by Lotas Inc.
#
#

# R function extraction for auto-accept functionality
#' Parse R code to extract function calls using AST parsing
#' @param code Character string containing R code to parse
#' @param language Language identifier (must be "r")
#' @return List with functions (character vector), success (logical), and error (character or NULL)
#' @export
.ps.rpc.parse_functions <- function(code, language) {
    # Only support R language in this R runtime
    if (language != "r") {
        return(list(
            functions = character(0),
            success = FALSE,
            error = paste("R runtime cannot parse", language, "code")
        ))
    }
    
    tryCatch({
        # Handle empty or null input
        if (is.null(code) || is.na(code) || nchar(trimws(code)) == 0) {
            return(list(
                functions = character(0),
                success = TRUE,
                error = NULL
            ))
        }
        
        # Clean up code by removing markdown code fences
        trimmed_code <- gsub("^```[a-zA-Z]*\\s*\\n?", "", code, perl = TRUE)
        trimmed_code <- gsub("\\n?```\\s*$", "", trimmed_code, perl = TRUE)
        trimmed_code <- gsub("```\\n", "", trimmed_code, perl = TRUE)
        trimmed_code <- trimws(trimmed_code)
        
        if (nchar(trimmed_code) == 0) {
            return(list(
                functions = character(0),
                success = TRUE,
                error = NULL
            ))
        }
        
        # Parse the R code into AST
        parsed_ast <- parse(text = trimmed_code, keep.source = FALSE)
        if (length(parsed_ast) == 0) {
            return(list(
                functions = character(0),
                success = TRUE,
                error = NULL
            ))
        }
        
        # Recursive function to extract all function calls (excluding language constructs)
        extract_calls_recursive <- function(expr) {
            calls <- c()
            
            if (is.call(expr)) {
                func_name <- as.character(expr[[1]])
                expr_class <- class(expr)[1]
                
                # Only capture expressions with class 'call'
                # This excludes language constructs like 'if', 'for', 'while', '<-', etc.
                if (expr_class == "call") {
                    calls <- c(calls, func_name)
                }
                
                # Recursively examine all arguments
                for (i in 2:length(expr)) {
                    if (i <= length(expr)) {
                        nested_calls <- extract_calls_recursive(expr[[i]])
                        calls <- c(calls, nested_calls)
                    }
                }
            } else if (is.expression(expr)) {
                # Handle expression objects (top-level parsed result)
                if (length(expr) > 0) {
                    for (i in 1:length(expr)) {
                        nested_calls <- extract_calls_recursive(expr[[i]])
                        calls <- c(calls, nested_calls)
                    }
                }
            }
            
            return(calls)
        }
        
        # Extract all function calls and return result structure
        all_calls <- extract_calls_recursive(parsed_ast)
        unique_calls <- sort(unique(all_calls))
        
        list(
            functions = unique_calls,
            success = TRUE,
            error = NULL
        )
        
    }, error = function(e) {
        list(
            functions = character(0),
            success = FALSE,
            error = as.character(e$message)
        )
    })
}
