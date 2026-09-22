#' Create a psych-DS-ish project from the RStudio New Project wizard
#'
#' Binding for the RStudio project template, so that *File > New Project >
#' New Directory > psych-DS-ish Project* creates a project skeleton with
#' [create_project_skeleton()]. It is not intended to be called directly; use
#' [create_project_skeleton()] instead.
#'
#' @param path Character scalar. Path to the new project directory, supplied
#'   by RStudio.
#' @param quarto_yml Logical. Passed to [create_project_skeleton()]. Set by the
#'   checkbox in the New Project wizard.
#' @param ... Other inputs from the New Project wizard (ignored).
#'
#' @return (Invisibly) the `data.frame` returned by
#'   [create_project_skeleton()].
#'
#' @keywords internal
#' @export
create_psychdsish_project <- function(path, quarto_yml = TRUE, ...) {
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  create_project_skeleton(
    project_root = path,
    overwrite = FALSE,
    rproj = TRUE,
    quarto_yml = isTRUE(quarto_yml),
    quiet = TRUE
  )
}
