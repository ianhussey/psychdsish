#' Scan a project for possibly-unused packages across all QMD/RMD files
#'
#' Recursively finds .qmd and .Rmd files under `root`, renders each with
#' `unused_dependencies()` (which tries Quarto first and falls back to R Markdown),
#' and returns a single tibble with a `file` column and the detected
#' `possibly_unused_packages`.
#' 
#' @import purrr
#' @import tibble
#' @import dplyr
#'
#' @param root Directory to scan (default ".").
#' @return A tibble with columns:
#'   - `file` (relative path to the document)
#'   - `possibly_unused_packages` (character; one row per package per file)
#'   Files that fail to render are reported via a message and skipped.
#'   
#' @examples
#' \dontrun{
#' res <- check_unused_dependencies(root = "examples")
#' res
#' }
#' 
#' @export
check_unused_dependencies <- function(root = "."){
  if (!dir.exists(root)) stop("Directory not found: ", root)
  
  # Find candidate files
  files <- list.files(root, pattern = "\\.(qmd|Rmd)$", recursive = TRUE, full.names = TRUE)
  if (length(files) == 0L) {
    return(tibble::tibble(file = character(), possibly_unused_packages = character()))
  }
  
  # Relative path helper
  norm_root <- normalizePath(root, winslash = "/", mustWork = TRUE)
  rel_path  <- function(p) sub(paste0("^", norm_root, "/?"), "", normalizePath(p, winslash = "/", mustWork = TRUE))
  
  # Safely run the single-file checker (Quarto-first inside)
  runner <- purrr::safely(function(path) {
    out <- .check_unused_dependencies_single_file(path)
    out$file <- rel_path(path)
    # ensure columns exist even if empty tibble returned
    dplyr::select(out, file, possibly_unused_packages)
  })
  
  results <- purrr::map(files, runner)
  
  # Report failures (if any)
  failed <- purrr::imap_chr(results, ~ if (is.null(.x$error)) {NA_character_} else {rel_path(files[.y])})
  failed <- stats::na.omit(failed)
  if (length(failed)) {
    message("Some files failed to render; skipping:\n  - ", paste(failed, collapse = "\n  - "))
  }
  
  # Collect successes
  oks <- purrr::map(results, "result")
  oks <- purrr::compact(oks)
  
  if (length(oks) == 0L) {
    return(tibble::tibble(file = character(), possibly_unused_packages = character()))
  }
  
  dplyr::bind_rows(oks) |>
    dplyr::arrange(file, possibly_unused_packages)
}