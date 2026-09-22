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
#' @param studies Number of studies, as text from the New Project wizard.
#'   Passed to [create_project_skeleton()].
#' @param layout Multi-study layout, `"by_study"` or `"by_type"`. Passed to
#'   [create_project_skeleton()].
#' @param ... Other inputs from the New Project wizard (ignored).
#'
#' @return (Invisibly) the `data.frame` returned by
#'   [create_project_skeleton()].
#'
#' @keywords internal
#' @export
create_psychdsish_project <- function(
  path,
  quarto_yml = TRUE,
  studies = "1",
  layout = "by_study",
  ...
) {
  studies_n <- suppressWarnings(as.integer(trimws(studies)))
  if (is.na(studies_n) || studies_n < 1) {
    stop("'Number of studies' must be a whole number of 1 or more.", call. = FALSE)
  }
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  create_project_skeleton(
    project_root = path,
    overwrite = FALSE,
    rproj = TRUE,
    quarto_yml = isTRUE(quarto_yml),
    studies = studies_n,
    layout = layout,
    quiet = TRUE
  )
}
