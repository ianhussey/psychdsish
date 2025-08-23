#' Check for objects that are created but never used within a file
#'
#' Parses an R script or an R/Quarto document, extracts the R code, and reports
#' symbols that are assigned to but never referenced anywhere else in the same file.
#' Supports `.R`, `.Rmd`, and `.qmd` (R code chunks only for the latter two).
#'
#' @param path Character. Path to a file.
#' @return A data frame with one column, `unused_objects`, listing orphaned objects.
#'   Returns an empty data frame if no orphans are found.
#'
#' @examples
#' \dontrun{
#' check_unused_objects_single_file("temp.R")
#' check_unused_objects_single_file("analysis.qmd")
#' }
#' 
#' @export
check_unused_objects_single_file <- function(path) {
  if (!file.exists(path)) stop("File not found: ", path)
  
  ext <- tolower(tools::file_ext(path))
  lines <- readLines(path, warn = FALSE)
  
  # Extract R code if file is Rmd/qmd; otherwise use all lines
  if (ext %in% c("rmd", "qmd")) {
    code <- extract_r_chunks(lines)
  } else {
    code <- lines
  }
  
  # Empty or no R chunks
  if (length(code) == 0L || all(!nzchar(code))) {
    return(data.frame(unused_objects = character(), stringsAsFactors = FALSE))
  }
  
  # Parse
  exprs <- tryCatch(parse(text = code, keep.source = TRUE),
                    error = function(e) stop("Could not parse R code in: ", path, "\n", conditionMessage(e)))
  
  assigned <- character()
  used     <- character()
  
  add_assigned <- function(sym) {
    if (is.symbol(sym)) assigned <<- c(assigned, as.character(sym))
    else if (is.character(sym) && length(sym) == 1L && nzchar(sym)) assigned <<- c(assigned, sym)
  }
  
  walk <- function(e) {
    if (is.call(e)) {
      fname <- as.character(e[[1]])
      
      # Handle assignments: <-, =, <<-
      if (fname %in% c("<-", "=", "<<-")) {
        add_assigned(e[[2]])
        walk(e[[3]])
        return(invisible())
      }
      
      # Handle right-arrow assignments: ->, ->>
      if (fname %in% c("->", "->>")) {
        add_assigned(e[[3]])
        walk(e[[2]])
        return(invisible())
      }
      
      # Handle assign("name", value)
      if (identical(fname, "assign")) {
        if (length(e) >= 2L) {
          lhs <- e[[2]]
          if (is.character(lhs)) add_assigned(lhs)
          if (is.symbol(lhs)) add_assigned(lhs)
        }
        lapply(as.list(e)[-1], walk)
        return(invisible())
      }
      
      # Otherwise, traverse children
      lapply(as.list(e), walk)
      return(invisible())
    }
    
    if (is.symbol(e)) {
      used <<- c(used, as.character(e))
    }
    invisible()
  }
  
  for (ex in exprs) walk(ex)
  
  orphans <- setdiff(unique(assigned), unique(used))
  data.frame(unused_objects = orphans, stringsAsFactors = FALSE)
}

# Helper: extract R code chunks from Rmd/qmd
extract_r_chunks <- function(lines) {
  code <- character()
  in_chunk <- FALSE
  fence_start <- "^\\s*```\\s*\\{\\s*r(\\s*,.*)?\\}\\s*$"
  fence_end   <- "^\\s*```\\s*$"
  
  for (ln in lines) {
    if (!in_chunk && grepl(fence_start, ln)) {
      in_chunk <- TRUE
      next
    }
    if (in_chunk && grepl(fence_end, ln)) {
      in_chunk <- FALSE
      next
    }
    if (in_chunk) code <- c(code, ln)
  }
  code
}