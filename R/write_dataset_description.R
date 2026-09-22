#' Write a psych-DS dataset_description.json from the project's codebooks
#'
#' Creates or updates `dataset_description.json` in the project root, which
#' the [psych-DS](https://psych-ds.github.io/) standard requires. The
#' variables it describes are taken from the codebooks created by the codebook
#' chunk in `code/processing.qmd`, so that each variable is described once, in
#' the codebook, rather than twice.
#'
#' Documenting variables in a codebook `.csv` is the simpler option, and is
#' all that [validator()] requires. Writing them to
#' `dataset_description.json` as well is more work, but makes the project
#' closer to a valid psych-DS dataset, which is machine-readable and easier
#' for others to reuse. Use whichever suits the project; you do not need both.
#'
#' @param project_root Character scalar. Path to the project root, e.g.,
#'   `"../"` from `code/processing.qmd`.
#' @param name Character scalar. The dataset's name. Defaults to the name in
#'   an existing `dataset_description.json`, or the project folder's name.
#' @param description Character scalar. A description of the dataset.
#'   Defaults to the description in an existing file, or a placeholder to
#'   complete by hand.
#' @param quiet Logical. If `FALSE` (default), a one-line summary is printed
#'   as a message.
#'
#' @details
#' Each codebook row becomes a `PropertyValue` entry in `variableMeasured`,
#' mapping the codebook's `variable`, `description`, and `units` columns onto
#' `name`, `description`, and `unitText`. A `coding` entry that is not
#' "none" is appended to the description, because psych-DS has no separate
#' field for it. Where the same variable appears in more than one codebook,
#' the first description is used.
#'
#' Fields that the file already contains and that this function does not
#' write (e.g., `author` or `license`) are kept.
#'
#' Data file names in the templates follow the psych-DS convention of
#' `key-value` pairs ending in `_data`, e.g.,
#' `study-1_stage-processed_data.csv`. To check a project against the
#' standard itself, use the psych-DS validator
#' (<https://psych-ds.github.io/validator/>).
#'
#' @return
#' (Invisibly) a list with `path`, `name`, `description`, and `variables`
#' (the variable names written).
#'
#' @examples
#' project <- file.path(tempdir(), "psychds_project")
#' create_project_skeleton(project, quiet = TRUE)
#'
#' # normally written by the codebook chunk in code/processing.qmd
#' write.csv(
#'   data.frame(
#'     variable = c("id", "rt"),
#'     description = c("Participant identifier", "Response time"),
#'     units = c("none", "milliseconds"),
#'     coding = c("none", "none")
#'   ),
#'   file.path(project, "data", "processed", "stage-processed_codebook.csv"),
#'   row.names = FALSE
#' )
#'
#' write_dataset_description(project, name = "My study", description = "A demo")
#' cat(readLines(file.path(project, "dataset_description.json")), sep = "\n")
#'
#' unlink(project, recursive = TRUE)
#'
#' @seealso [create_project_skeleton()], [validator()]
#'
#' @export
write_dataset_description <- function(
  project_root,
  name = NULL,
  description = NULL,
  quiet = FALSE
) {
  if (!dir.exists(project_root)) {
    stop("Directory not found: ", project_root)
  }
  placeholder <- "TO BE COMPLETED MANUALLY"
  json_path <- file.path(project_root, "dataset_description.json")

  # keep anything the file already contains (e.g., author, license)
  existing <- if (file.exists(json_path)) {
    tryCatch(
      jsonlite::read_json(json_path, simplifyVector = FALSE),
      error = function(e) {
        stop("Could not read ", json_path, ": ", conditionMessage(e))
      }
    )
  } else {
    list()
  }

  if (is.null(name)) {
    name <- existing$name %||% basename(normalizePath(project_root))
  }
  if (is.null(description)) {
    description <- existing$description %||% placeholder
  }

  # codebooks live alongside the data they describe
  detected <- detect_layout(project_root)
  data_dirs <- file.path(
    project_root,
    layout_expand(
      c("data/raw", "data/processed"),
      detected$layout,
      detected$studies
    )
  )
  codebooks <- unlist(lapply(data_dirs[dir.exists(data_dirs)], function(d) {
    list.files(d, pattern = "_codebook\\.csv$", recursive = TRUE, full.names = TRUE)
  }))

  variables <- list()
  for (f in sort(codebooks)) {
    cb <- tryCatch(
      utils::read.csv(f, colClasses = "character"),
      error = function(e) NULL
    )
    if (is.null(cb) || !"variable" %in% names(cb)) {
      warning("Skipping codebook without a 'variable' column: ", f)
      next
    }
    for (i in seq_len(nrow(cb))) {
      variable <- trimws(cb$variable[i])
      if (!nzchar(variable) || variable %in% names(variables)) {
        next
      }
      desc <- if ("description" %in% names(cb)) trimws(cb$description[i]) else ""
      coding <- if ("coding" %in% names(cb)) trimws(cb$coding[i]) else ""
      units <- if ("units" %in% names(cb)) trimws(cb$units[i]) else ""
      # psych-DS has no field for the coding, so it goes in the description
      if (
        nzchar(coding) &&
          !tolower(coding) %in% c("none", "na", "-") &&
          coding != placeholder
      ) {
        desc <- paste0(if (nzchar(desc)) paste0(desc, ". "), "Coding: ", coding)
      }
      variables[[variable]] <- list(
        "@type" = "PropertyValue",
        name = variable,
        description = if (nzchar(desc)) desc else placeholder,
        unitText = if (nzchar(units) && units != placeholder) units else "none"
      )
    }
  }

  if (length(variables) == 0) {
    warning(
      "No codebooks found under data/. psych-DS requires 'variableMeasured', ",
      "so run the codebook chunk in code/processing.qmd first."
    )
  }

  out <- existing
  out[["@context"]] <- "https://schema.org/"
  out[["@type"]] <- "Dataset"
  out[["name"]] <- name
  out[["description"]] <- description
  out[["variableMeasured"]] <- unname(variables)

  jsonlite::write_json(out, json_path, pretty = TRUE, auto_unbox = TRUE)

  todo <- sum(vapply(
    variables,
    function(v) grepl(placeholder, v$description, fixed = TRUE),
    logical(1)
  ))
  if (!quiet) {
    message(
      "Wrote ",
      json_path,
      " with ",
      length(variables),
      " variable(s)",
      if (todo > 0) {
        paste0(
          ", ",
          todo,
          " of which still need a description (complete the codebook, then run this again)"
        )
      },
      "."
    )
  }

  invisible(list(
    path = json_path,
    name = name,
    description = description,
    variables = names(variables)
  ))
}

`%||%` <- function(x, y) if (is.null(x)) y else x
