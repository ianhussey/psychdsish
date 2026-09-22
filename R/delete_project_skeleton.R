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
#'   be deleted, e.g., `"../"` from `tools/project_creator.qmd` in the
#'   project. There is no default, so that nothing is deleted unless you name
#'   the folder.
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
#' @param keep_file Path to the file to protect, or `NULL` (default) to
#'   detect the file the function is being called from. Supply it to run the
#'   function from the console or a script whose location cannot be
#'   detected. It must exist; if it is outside `project_root`, nothing is
#'   protected.
#'
#' @return Invisibly, a list with `project_root`, `keep_file`, `paths` (the
#'   top-level paths deleted, or that would be deleted if `dry_run = TRUE`,
#'   relative to `project_root`), and `n_items` (the number of files and
#'   directories they contain, including themselves).
#'
#' @section Warning:
#' Deletion is irreversible. Always test with `dry_run = TRUE` first, and
#' keep `require_sentinel = TRUE` and `confirm = TRUE` for normal use.
#'
#' @details
#' As an additional safeguard, unless `keep_file` is supplied, the function
#' must be able to identify the file it is being called from (via
#' knitr/Quarto, RStudio, or `Rscript --file=`) so that it never deletes the
#' running script or its parent directories. It is therefore intended to be
#' run from a script such as `tools/project_creator.qmd`, and it aborts if the
#' current file cannot be determined.
#'
#' A dry run (`dry_run = TRUE`) does not ask for confirmation, because it
#' deletes nothing.
#'
#' @examples
#' project <- file.path(tempdir(), "my_project")
#' create_project_skeleton(project, quiet = TRUE)
#' keep <- file.path(project, "tools", "project_creator.qmd")
#'
#' # Preview what would be deleted
#' delete_project_skeleton(project, keep_file = keep, dry_run = TRUE)
#'
#' # Delete everything except `keep`. confirm = FALSE skips the typed
#' # confirmation: only use it when you are sure.
#' delete_project_skeleton(project, keep_file = keep, confirm = FALSE)
#' list.files(project, recursive = TRUE)
#'
#' # From a script inside the project (e.g., tools/project_creator.qmd), the
#' # running file is detected and protected, so `keep_file` is not needed:
#' # delete_project_skeleton(project_root = "../")
#'
#' unlink(project, recursive = TRUE)
#'
#' @export
delete_project_skeleton <- function(
  project_root,
  require_sentinel = TRUE,
  sentinel_names = c(
    ".ok-to-prune",
    ".git",
    ".gitignore",
    ".here",
    "_project.yml",
    "_quarto.yml"
  ),
  confirm = TRUE,
  dry_run = FALSE,
  large_n_warn = 10000,
  keep_file = NULL
) {
  # ---------- helpers ----------
  # forward slashes on every platform, so that the path comparisons below
  # also work on Windows
  normalize_safe <- function(p, mustWork = FALSE) {
    tryCatch(
      normalizePath(p, winslash = "/", mustWork = mustWork),
      error = function(e) NA_character_
    )
  }
  same_path <- function(a, b) {
    aa <- normalize_safe(a, mustWork = FALSE)
    bb <- normalize_safe(b, mustWork = FALSE)
    isTRUE(aa == bb)
  }
  is_windows <- function() .Platform$OS.type == "windows"
  is_fs_root <- function(p) {
    p <- normalize_safe(p, mustWork = FALSE)
    if (is.na(p)) {
      return(FALSE)
    }
    if (!is_windows()) {
      return(p == "/")
    }
    # Windows drive root like C:\ or C:/ ; also UNC share root like \\server\share\
    grepl("^[A-Za-z]:[\\\\/]?$", p) ||
      grepl("^\\\\\\\\[^\\\\]+\\\\[^\\\\]+[\\\\/]?$", p)
  }
  is_subpath <- function(child, parent) {
    child <- normalize_safe(child, mustWork = FALSE)
    parent <- normalize_safe(parent, mustWork = FALSE)
    if (anyNA(c(child, parent))) {
      return(FALSE)
    }
    startsWith(paste0(child, "/"), paste0(sub("/$", "", parent), "/"))
  }
  ancestor_chain <- function(path, stop_at) {
    out <- character(0)
    cur <- normalize_safe(dirname(path), mustWork = FALSE)
    stop_at <- normalize_safe(stop_at, mustWork = FALSE)
    while (!is.na(cur) && nzchar(cur) && !same_path(cur, stop_at)) {
      out <- c(out, cur)
      nxt <- dirname(cur)
      if (identical(nxt, cur)) {
        break
      }
      cur <- nxt
    }
    if (!is.na(stop_at) && nzchar(stop_at)) {
      out <- c(out, stop_at)
    }
    unique(out)
  }
  detect_current_file <- function() {
    # 1) knitr/quarto
    cf <- tryCatch(
      {
        if (isTRUE(getOption("knitr.in.progress"))) {
          p <- knitr::current_input()
          if (nzchar(p)) return(normalize_safe(p, mustWork = FALSE))
        }
        NA_character_
      },
      error = function(e) NA_character_
    )
    # 2) RStudio
    if (is.na(cf)) {
      cf <- tryCatch(
        {
          if (
            requireNamespace("rstudioapi", quietly = TRUE) &&
              rstudioapi::isAvailable()
          ) {
            p <- rstudioapi::getActiveDocumentContext()$path
            if (nzchar(p)) return(normalize_safe(p, mustWork = FALSE))
          }
          NA_character_
        },
        error = function(e) NA_character_
      )
    }
    # 3) Rscript --file=
    if (is.na(cf)) {
      cf <- tryCatch(
        {
          ca <- commandArgs(trailingOnly = FALSE)
          m <- grep("^--file=", ca)
          if (length(m) > 0) {
            return(normalize_safe(
              sub("^--file=", "", ca[m[1]]),
              mustWork = FALSE
            ))
          }
          NA_character_
        },
        error = function(e) NA_character_
      )
    }
    # 4) source() / sys.frames
    if (is.na(cf)) {
      cf <- tryCatch(
        {
          of <- get0("ofile", envir = sys.frames()[[1]], ifnotfound = NULL)
          if (!is.null(of) && nzchar(of)) {
            return(normalize_safe(of, mustWork = FALSE))
          }
          NA_character_
        },
        error = function(e) NA_character_
      )
    }
    cf
  }

  # ---------- pre-flight safety ----------
  project_root <- normalize_safe(project_root, mustWork = TRUE)
  if (is.na(project_root)) {
    stop("`project_root` does not exist.")
  }

  if (is_fs_root(project_root)) {
    stop("Refusing to operate on a filesystem root: ", project_root)
  }

  if (require_sentinel) {
    has_sentinel <- any(file.exists(file.path(project_root, sentinel_names)))
    if (!has_sentinel) {
      stop(
        "Safety check failed: none of the sentinel files found in `project_root`.\n",
        "Create one of: ",
        paste(sentinel_names, collapse = ", "),
        " or set `require_sentinel = FALSE` (not recommended)."
      )
    }
  }

  if (is.null(keep_file)) {
    current_file <- detect_current_file()
    if (is.na(current_file)) {
      stop(
        "Could not detect the current file; aborting to avoid accidental deletion. ",
        "Supply `keep_file` to name the file to protect."
      )
    }
  } else {
    if (length(keep_file) != 1 || !file.exists(keep_file) || dir.exists(keep_file)) {
      stop("`keep_file` must be the path of an existing file.")
    }
    current_file <- normalize_safe(keep_file, mustWork = TRUE)
  }

  # ---------- plan: the top-level paths to delete ----------
  # Everything is deleted except the protected file and the directories that
  # lead to it; inside those directories, their other contents are deleted.
  children <- function(d) {
    list.files(d, full.names = TRUE, all.files = TRUE, no.. = TRUE)
  }
  if (is_subpath(current_file, project_root)) {
    keep_dirs <- ancestor_chain(current_file, project_root)
    to_delete <- unlist(lapply(keep_dirs, children))
    to_delete <- normalize_safe(to_delete, mustWork = FALSE)
    to_delete <- to_delete[
      !vapply(to_delete, same_path, logical(1), b = current_file) &
        !to_delete %in% normalize_safe(keep_dirs, mustWork = FALSE)
    ]
  } else {
    to_delete <- normalize_safe(children(project_root), mustWork = FALSE)
  }
  to_delete <- unique(to_delete[!is.na(to_delete)])

  n_items <- sum(vapply(
    to_delete,
    function(p) {
      if (dir.exists(p)) {
        1L +
          length(list.files(
            p,
            all.files = TRUE,
            recursive = TRUE,
            include.dirs = TRUE,
            no.. = TRUE
          ))
      } else {
        1L
      }
    },
    integer(1)
  ))
  result <- list(
    project_root = project_root,
    keep_file = current_file,
    paths = sort(substring(to_delete, nchar(project_root) + 2)),
    n_items = n_items
  )

  if (!length(to_delete)) {
    message("Nothing to delete.")
    return(invisible(result))
  }

  if (dry_run) {
    message(
      "Dry run: would delete ",
      format(n_items, big.mark = ","),
      " items: ",
      paste(result$paths, collapse = ", "),
      "\nNo files deleted. Set `dry_run = FALSE` to execute."
    )
    return(invisible(result))
  }

  # Interactive confirmation
  if (confirm) {
    if (!interactive()) {
      stop(
        "Confirmation required but session is non-interactive. Run interactively or set `confirm = FALSE` (only if you are sure)."
      )
    }
    cat(
      sprintf(
        "Are you sure you want to delete EVERYTHING inside:\n  %s\n(except %s and the directories needed to keep it)\n",
        project_root,
        current_file
      ),
      sprintf(
        "Prospective deletions: %s items.\n",
        format(n_items, big.mark = ",")
      ),
      "This cannot be undone.\n",
      sprintf(
        "To proceed, type the folder name exactly: \"%s\"\n> ",
        basename(project_root)
      ),
      sep = ""
    )
    ans <- readline()
    if (!identical(ans, basename(project_root))) {
      stop("Confirmation failed. Aborting without deleting.")
    }
    if (n_items >= large_n_warn) {
      ans2 <- readline(sprintf(
        "This is a very large deletion (>= %d items). Type 'YES' to proceed: ",
        large_n_warn
      ))
      if (!identical(ans2, "YES")) {
        stop("Large-deletion confirmation failed. Aborting.")
      }
    }
  }

  # ---------- deletion ----------
  unlink(to_delete, recursive = TRUE, force = TRUE)

  message("Deleted: ", paste(result$paths, collapse = ", "))
  message("Protected file: ", current_file)
  invisible(result)
}
