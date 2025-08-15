#' Delete all files and folders in a project, except a specified directory
#'
#' This function removes ALL files and folders inside a given project root,
#' including the .git folder (!), except for a single directory that you 
#' choose to preserve. It is useful for quickly clearing a project created by 
#' `create_project_skeleton()` while keeping reusable tools
#' (e.g., helper scripts containing `create_project_skeleton()`, in /tools) 
#' intact. I use it mostly for testing `create_project_skeleton()`.
#'
#' @param project_root Character scalar. Path to the root directory of the
#'   project to clean. Defaults to `"../"` on the assumption it is being
#'   run from "project_name/tools/project_creator.qmd". Must exist.
#' @param keep_dir Character scalar. Name of the directory (relative to
#'   `project_root`) to preserve. Defaults to `"tools"` on the assumption 
#'   it is being run from "project_name/tools/project_creator.qmd". 
#'   If the directory does not exist, it will simply be ignored.
#'
#' @details
#' \itemize{
#'   \item All files and folders in `project_root` will be deleted except the
#'     one specified in `keep_dir`.
#'   \item The function uses `unlink()` for recursive deletion, so **deleted
#'     items cannot be recovered** unless under version control.
#'   \item Hidden files (e.g., `.gitignore`, `.Rhistory`) are also removed unless
#'     they are inside `keep_dir`.
#' }
#'
#' @return
#' Invisibly returns `NULL`. Prints a message listing deleted items or noting
#' that nothing was deleted.
#'
#' @examples
#' \dontrun{
#' # Delete everything in the parent directory except the 'tools' folder
#' delete_project_skeleton(project_root = "../", keep_dir = "tools")
#'
#' # Delete everything except 'scripts'
#' delete_project_skeleton(project_root = ".", keep_dir = "scripts")
#' }
#'
#' @export
delete_project_skeleton <- function(project_root = "../", keep_dir = "tools") {
  # Normalize paths
  project_root <- normalizePath(project_root, mustWork = TRUE)
  keep_path <- normalizePath(file.path(project_root, keep_dir), mustWork = FALSE)
  
  # List all entries in project_root
  entries <- list.files(project_root, full.names = TRUE, all.files = TRUE, no.. = TRUE)
  
  # Identify items to delete (exclude keep_path)
  to_delete <- entries[normalizePath(entries, mustWork = FALSE) != keep_path]
  
  # Delete recursively
  if (length(to_delete) > 0) {
    for (p in to_delete) {
      if (dir.exists(p)) {
        unlink(p, recursive = TRUE, force = TRUE)
      } else if (file.exists(p)) {
        unlink(p, force = TRUE)
      }
    }
    message("Deleted: ", paste(basename(to_delete), collapse = ", "))
  } else {
    message("Nothing to delete.")
  }
}