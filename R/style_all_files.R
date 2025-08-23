#' Style All Code in R, R Markdown, and Quarto Files in a Project
#'
#' Recursively finds `.R`, `.Rmd`, and `.qmd` files under a directory and
#' restyles them using [styler::style_file()]. This includes both code scripts
#' and R code chunks embedded in R Markdown or Quarto documents. Certain
#' directories can be excluded from the search.
#'
#' @param root Character scalar. Path to the root directory to search
#'   (default: current working directory `"."`).
#' @param patterns Character vector of regular expressions for file extensions
#'   to include. Defaults to common R and Quarto file suffixes.
#' @param exclude_dirs Character vector of directory names to skip (default:
#'   `.git`, `renv`, `_site`, `_freeze`, `.quarto`, `_book`, `tools`).
#'   Matching is done anywhere in the path.
#' @param dry_run Logical. If `TRUE`, returns the list of files that would be
#'   styled but does not modify them (default: `FALSE`).
#' @param ... Additional arguments passed on to [styler::style_file()], such as
#'   `strict = FALSE`.
#'
#' @return Invisibly returns the vector of file paths that were styled.
#'   If `dry_run = TRUE`, returns the vector of candidate file paths without
#'   styling.
#'
#' @details
#' - The search is recursive, starting at `root`.
#' - Excluded directories are filtered out using a regex match anywhere in the
#'   path.
#' - If no matching files are found, a message is issued and an empty character
#'   vector is returned invisibly.
#'
#' @examples
#' \dontrun{
#' # Preview which files would be styled
#' style_all_files(dry_run = TRUE)
#'
#' # Style all files under the current project
#' style_all_files()
#' }
#'
#' @seealso [styler::style_file()], [styler::style_dir()], [usethis::use_tidy_style()]
#' @export
style_all_files <- function(
    root = ".",
    patterns = c("\\.R$", "\\.r$", "\\.Rmd$", "\\.rmd$", "\\.qmd$", "\\.Qmd$"),
    exclude_dirs = c(".git", "renv", "_site", "_freeze", ".quarto", "_book", "tools"),
    dry_run = FALSE,
    ...
) {
  # Collect candidate files
  files <- list.files(
    path = root,
    pattern = paste(patterns, collapse = "|"),
    recursive = TRUE,
    full.names = TRUE
  )
  
  # Exclude files living under specified directories
  in_excluded_dir <- function(path, dirs) {
    any(vapply(dirs, function(d) grepl(paste0("(^|/)", d, "(/|$)"), path), logical(1)))
  }
  files <- files[!vapply(files, in_excluded_dir, logical(1), dirs = exclude_dirs)]
  
  if (dry_run) {
    return(files)  # show what would be styled
  }
  
  if (length(files) == 0) {
    message("No matching files found.")
    return(invisible(character()))
  }
  
  # Style all files (styler handles .R, and code chunks in .Rmd/.qmd)
  styler::style_file(files, ...)
  
  invisible(files)
}