#' Check all project files for objects that are created but never used
#'
#' Recursively searches for R/Quarto files and applies
#' `check_unused_objects_single_file()` to each, collating results.
#'
#' Supported extensions: `.R`, `.r`, `.Rmd`, `.rmd`, `.qmd`, `.Qmd`.
#'
#' @param root Character scalar. Directory to search from (default `"."`).
#' @param patterns Character vector of regexes for file extensions to include.
#'   Defaults to common R and Quarto suffixes.
#' @param exclude_dirs Character vector of directory names to skip anywhere in
#'   the path (default: `c(".git","renv","_site","_freeze",".quarto","_book","tools")`).
#' @param verbose Logical; if `TRUE`, message progress (default `FALSE`).
#'
#' @return A data frame with columns:
#' \describe{
#'   \item{file}{File path (character).}
#'   \item{unused_objects}{Name of an object that is created but never used (character).}
#' }
#' One row per unused object. If none are found, returns an empty data frame
#' with the two columns.
#'
#' @examples
#' \dontrun{
#' # Scan the current project
#' check_unused_objects(".")
#'
#' # Show what it finds in a subdirectory
#' check_unused_objects("R")
#' }
#' @export
check_unused_objects <- function(
    root = ".",
    patterns = c("\\.R$", "\\.r$", "\\.Rmd$", "\\.rmd$", "\\.qmd$", "\\.Qmd$"),
    exclude_dirs = c(".git", "renv", "_site", "_freeze", ".quarto", "_book", "tools"),
    verbose = FALSE
) {
  if (!dir.exists(root)) stop("Directory not found: ", root)
  
  # Gather candidate files
  files <- list.files(
    path = root,
    pattern = paste(patterns, collapse = "|"),
    recursive = TRUE,
    full.names = TRUE
  )
  
  # Filter out excluded directories (match anywhere in path)
  in_excluded_dir <- function(path, dirs) {
    any(vapply(dirs, function(d) grepl(paste0("(^|/)", d, "(/|$)"), path), logical(1)))
  }
  files <- files[!vapply(files, in_excluded_dir, logical(1), dirs = exclude_dirs)]
  
  if (length(files) == 0L) {
    if (verbose) message("No matching files found under: ", normalizePath(root, winslash = "/"))
    return(data.frame(file = character(), unused_objects = character(), stringsAsFactors = FALSE))
  }
  
  # Apply the single-file checker safely
  results <- lapply(files, function(f) {
    if (verbose) message("Checking: ", f)
    out <- tryCatch(
      check_unused_objects_single_file(f),  # must return data.frame(unused_objects = ...)
      error = function(e) {
        if (verbose) message("  Skipping (parse/error): ", f, " — ", conditionMessage(e))
        return(data.frame(unused_objects = character(), stringsAsFactors = FALSE))
      }
    )
    if (!is.data.frame(out) || !"unused_objects" %in% names(out)) {
      # Coerce to expected shape if the single-file function returns a character vector
      if (is.character(out)) out <- data.frame(unused_objects = out, stringsAsFactors = FALSE)
      else out <- data.frame(unused_objects = character(), stringsAsFactors = FALSE)
    }
    if (nrow(out) == 0L) return(NULL)
    data.frame(file = rep(f, nrow(out)), unused_objects = out$unused_objects, stringsAsFactors = FALSE)
  })
  
  results <- do.call(rbind, results)
  if (is.null(results)) {
    data.frame(file = character(), unused_objects = character(), stringsAsFactors = FALSE)
  } else {
    # Optionally de-duplicate rows (in case the single-file function emits duplicates)
    unique(results[order(results$file, results$unused_objects), , drop = FALSE])
  }
}