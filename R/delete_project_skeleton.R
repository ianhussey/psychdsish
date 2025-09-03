#' Delete a project skeleton while protecting the running file
#'
#' This function deletes the contents of a project folder, but protects
#' the file from which it is being executed (and the minimal set of
#' ancestor directories required to keep that file). It is intended for
#' cleaning out automatically generated project skeletons while leaving
#' the active script or Quarto/Rmd file intact.
#'
#' Several safety mechanisms are built in to reduce the risk of
#' catastrophic deletion:
#'
#' \itemize{
#'   \item Refuses to operate on filesystem roots (e.g., `/` or `C:/`).
#'   \item Optionally requires a sentinel file (e.g., `.ok-to-prune`)
#'         or other project marker in the target directory.
#'   \item Displays an interactive confirmation prompt requiring the
#'         exact folder name to be typed.
#'   \item Aborts in non-interactive sessions unless `confirm = FALSE`
#'         is set explicitly.
#'   \item Supports a dry-run mode to preview deletions.
#'   \item Issues an extra confirmation if the number of items to be
#'         deleted is very large.
#' }
#'
#' @param project_root Path to the project folder whose contents should
#'   be deleted. Defaults to `"../"`.
#' @param require_sentinel Logical. If `TRUE` (default), at least one of
#'   the files listed in `sentinel_names` must exist in the project root,
#'   otherwise the function aborts.
#' @param sentinel_names Character vector of sentinel filenames checked
#'   when `require_sentinel = TRUE`. Defaults include `.ok-to-prune`,
#'   `.git`, `.here`, `_project.yml`, and `_quarto.yml`.
#' @param confirm Logical. If `TRUE` (default), asks the user to confirm
#'   the deletion interactively by typing the folder name. In
#'   non-interactive sessions this must be set to `FALSE` to proceed.
#' @param dry_run Logical. If `TRUE`, only reports what would be deleted
#'   without actually deleting files. Default is `FALSE`.
#' @param large_n_warn Integer. Threshold number of items that triggers
#'   an additional confirmation prompt. Default is `10000`.
#'
#' @return Invisibly returns `NULL` (after deleting) or a list with
#'   estimated deletion counts when `dry_run = TRUE`.
#'
#' @section Warning:
#' Deletion is irreversible. Always test with `dry_run = TRUE` first, and
#' keep `require_sentinel = TRUE` and `confirm = TRUE` for normal use.
#'
#' @examples
#' \dontrun{
#' # Preview what would be deleted from a skeleton project
#' delete_project_skeleton("my_project", dry_run = TRUE)
#'
#' # Actually delete (interactive confirmation required)
#' delete_project_skeleton("my_project")
#'
#' # Non-interactive usage (e.g., in CI), requires confirm = FALSE
#' delete_project_skeleton("my_project", confirm = FALSE)
#' }
#' @export
delete_project_skeleton <- function(
    project_root = "../",
    require_sentinel = TRUE,
    sentinel_names = c(".ok-to-prune", ".git", ".gitignore", ".here", "_project.yml", "_quarto.yml"),
    confirm = TRUE,
    dry_run = FALSE,
    large_n_warn = 10000
) {
  # ---------- helpers ----------
  normalize_safe <- function(p, mustWork = FALSE) {
    tryCatch(normalizePath(p, mustWork = mustWork), error = function(e) NA_character_)
  }
  same_path <- function(a, b) {
    aa <- normalize_safe(a, mustWork = FALSE); bb <- normalize_safe(b, mustWork = FALSE); isTRUE(aa == bb)
  }
  is_windows <- function() .Platform$OS.type == "windows"
  is_fs_root <- function(p) {
    p <- normalize_safe(p, mustWork = FALSE)
    if (is.na(p)) return(FALSE)
    if (!is_windows()) return(p == "/")
    # Windows drive root like C:\ or C:/ ; also UNC share root like \\server\share\
    grepl("^[A-Za-z]:[\\\\/]?$", p) || grepl("^\\\\\\\\[^\\\\]+\\\\[^\\\\]+[\\\\/]?$", p)
  }
  is_subpath <- function(child, parent) {
    child <- normalize_safe(child, mustWork = FALSE); parent <- normalize_safe(parent, mustWork = FALSE)
    if (anyNA(c(child, parent))) return(FALSE)
    sep <- .Platform$file.sep
    startsWith(paste0(child, sep), paste0(parent, sep))
  }
  path_depth <- function(paths) {
    vapply(strsplit(normalize_safe(paths, mustWork = FALSE), .Platform$file.sep, fixed = TRUE),
           function(x) sum(nzchar(x)), integer(1))
  }
  ancestor_chain <- function(path, stop_at) {
    out <- character(0)
    cur <- normalize_safe(dirname(path), mustWork = FALSE)
    stop_at <- normalize_safe(stop_at, mustWork = FALSE)
    while (!is.na(cur) && nzchar(cur) && !same_path(cur, stop_at)) {
      out <- c(out, cur)
      nxt <- dirname(cur); if (identical(nxt, cur)) break
      cur <- nxt
    }
    if (!is.na(stop_at) && nzchar(stop_at)) out <- c(out, stop_at)
    unique(out)
  }
  detect_current_file <- function() {
    # 1) knitr/quarto
    cf <- tryCatch({
      if (isTRUE(getOption("knitr.in.progress"))) {
        p <- knitr::current_input(); if (nzchar(p)) return(normalize_safe(p, mustWork = FALSE))
      }
      NA_character_
    }, error = function(e) NA_character_)
    # 2) RStudio
    if (is.na(cf)) cf <- tryCatch({
      if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
        p <- rstudioapi::getActiveDocumentContext()$path
        if (nzchar(p)) return(normalize_safe(p, mustWork = FALSE))
      }
      NA_character_
    }, error = function(e) NA_character_)
    # 3) Rscript --file=
    if (is.na(cf)) cf <- tryCatch({
      ca <- commandArgs(trailingOnly = FALSE)
      m <- grep("^--file=", ca)
      if (length(m) > 0) return(normalize_safe(sub("^--file=", "", ca[m[1]]), mustWork = FALSE))
      NA_character_
    }, error = function(e) NA_character_)
    # 4) source() / sys.frames
    if (is.na(cf)) cf <- tryCatch({
      of <- get0("ofile", envir = sys.frames()[[1]], ifnotfound = NULL)
      if (!is.null(of) && nzchar(of)) return(normalize_safe(of, mustWork = FALSE))
      NA_character_
    }, error = function(e) NA_character_)
    cf
  }
  
  # ---------- pre-flight safety ----------
  project_root <- normalize_safe(project_root, mustWork = TRUE)
  if (is.na(project_root)) stop("`project_root` does not exist.")
  
  if (is_fs_root(project_root)) {
    stop("Refusing to operate on a filesystem root: ", project_root)
  }
  
  if (require_sentinel) {
    has_sentinel <- any(file.exists(file.path(project_root, sentinel_names)))
    if (!has_sentinel) {
      stop("Safety check failed: none of the sentinel files found in `project_root`.\n",
           "Create one of: ", paste(sentinel_names, collapse = ", "), " or set `require_sentinel = FALSE` (not recommended).")
    }
  }
  
  current_file <- detect_current_file()
  if (is.na(current_file)) {
    stop("Could not detect the current file; aborting to avoid accidental deletion.")
  }
  
  entries <- list.files(project_root, full.names = TRUE, all.files = TRUE, no.. = TRUE)
  if (!length(entries)) { message("Nothing to delete."); return(invisible(NULL)) }
  
  # Count prospective deletions for warnings / confirmation text
  prospective_deleted <- (function() {
    count <- 0L
    for (e in entries) {
      e_norm <- normalize_safe(e, mustWork = FALSE)
      if (is.na(e_norm)) next
      if (dir.exists(e_norm)) {
        if (!is_subpath(current_file, e_norm)) {
          # whole dir
          count <- count + length(list.files(e_norm, all.files = TRUE, recursive = TRUE, include.dirs = TRUE, no.. = TRUE)) + 1L
        } else {
          # selective inside
          nested <- list.files(e_norm, full.names = TRUE, all.files = TRUE, recursive = TRUE, include.dirs = TRUE, no.. = TRUE)
          if (length(nested)) {
            keep_flags <- vapply(nested, function(p) {
              p_norm <- normalize_safe(p, mustWork = FALSE)
              isTRUE(same_path(p_norm, current_file) || is_subpath(current_file, p_norm))
            }, logical(1))
            count <- count + sum(!keep_flags)
          }
        }
      } else if (file.exists(e_norm)) {
        if (!same_path(e_norm, current_file)) count <- count + 1L
      }
    }
    count
  })()
  
  # Interactive confirmation
  if (confirm) {
    if (!interactive()) {
      stop("Confirmation required but session is non-interactive. Run interactively or set `confirm = FALSE` (only if you are sure).")
    }
    cat(
      sprintf(
        "Are you sure you want to delete EVERYTHING inside:\n  %s\n(except the running file and the minimal directories needed to keep it)\n",
        project_root
      ),
      sprintf("Prospective deletions: ~%s items.\n", format(prospective_deleted, big.mark = ",")),
      "This cannot be undone.\n",
      sprintf("To proceed, type the folder name exactly: \"%s\"\n> ", basename(project_root)),
      sep = ""
    )
    ans <- readline()
    if (!identical(ans, basename(project_root))) {
      stop("Confirmation failed. Aborting without deleting.")
    }
    if (prospective_deleted >= large_n_warn) {
      ans2 <- readline(sprintf("This is a very large deletion (>= %d items). Type 'YES' to proceed: ", large_n_warn))
      if (!identical(ans2, "YES")) stop("Large-deletion confirmation failed. Aborting.")
    }
  }
  
  if (dry_run) {
    message("Dry run: no files deleted. Set `dry_run = FALSE` to execute.")
    return(invisible(list(project_root = project_root, estimated_deletions = prospective_deleted)))
  }
  
  # ---------- deletion (same logic as before, with protections) ----------
  deleted <- character(0)
  
  for (e in entries) {
    e_norm <- normalize_safe(e, mustWork = FALSE)
    if (is.na(e_norm)) next
    
    if (dir.exists(e_norm)) {
      if (!is_subpath(current_file, e_norm)) {
        unlink(e_norm, recursive = TRUE, force = TRUE)
        deleted <- c(deleted, e_norm)
      } else {
        nested <- list.files(e_norm, full.names = TRUE, all.files = TRUE,
                             recursive = TRUE, include.dirs = TRUE, no.. = TRUE)
        if (length(nested)) {
          keep_flags <- vapply(nested, function(p) {
            p_norm <- normalize_safe(p, mustWork = FALSE)
            isTRUE(same_path(p_norm, current_file) || is_subpath(current_file, p_norm))
          }, logical(1))
          to_delete <- nested[!keep_flags]
          if (length(to_delete)) {
            to_delete <- to_delete[order(-path_depth(to_delete))]
            for (q in to_delete) {
              if (dir.exists(q)) {
                unlink(q, recursive = TRUE, force = TRUE)
              } else if (file.exists(q)) {
                unlink(q, force = TRUE)
              }
            }
            deleted <- c(deleted, to_delete)
          }
        }
      }
    } else if (file.exists(e_norm)) {
      if (!same_path(e_norm, current_file)) {
        unlink(e_norm, force = TRUE)
        deleted <- c(deleted, e_norm)
      }
    }
  }
  
  if (length(deleted)) {
    message("Deleted: ", paste(basename(deleted), collapse = ", "))
    message("Protected file: ", current_file)
  } else {
    message("Nothing to delete.")
    message("Protected file: ", current_file)
  }
  
  invisible(NULL)
}
