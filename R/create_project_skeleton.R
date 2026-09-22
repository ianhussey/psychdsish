#' Create an opinionated project directory skeleton following psych-ds-ish standard
#'
#' This function creates a standardised-ish directory and file structure for a
#' research project. The layout is loosely inspired by the
#' [psych-DS](https://psych-ds.github.io/) specification, but is **not**
#' fully psych-DS compliant. It adds extra conventions for reproducibility
#' and common analysis workflows, including Quarto file templates and
#' a `.gitignore` file. It does not comply with psych-ds's .json requirement.
#'
#' The created structure is designed to separate raw data, processed data,
#' analysis code, outputs, and documentation, while including sensible
#' defaults for licensing, reproducibility, and version control.
#'
#' @param project_root Character scalar. Path to the root directory where
#'   the project skeleton should be created. Defaults to `"../"` on the
#'   assumption that this function is run from
#'   "project_name/tools/project_creator.qmd", but can also be run from the
#'   console.
#' @param overwrite Logical. If `TRUE`, existing files will be overwritten.
#'   Defaults to `FALSE`.
#' @param rproj Logical. If `TRUE` (default), an RStudio project file named
#'   after the project root directory (e.g., `my_project.Rproj`) is created,
#'   unless the project root already contains an `.Rproj` file.
#' @param quiet Logical. If `FALSE` (default), a one-line summary of what was
#'   created, overwritten, or skipped is printed as a message.
#'
#' @details
#' The following directories are created (if not already present):
#' \itemize{
#'   \item `code/` - analysis and processing scripts (`.qmd`/`.Rmd`)
#'   \item `reports/` - manuscripts, slides, preprints, etc.
#'   \item `data/`
#'     \itemize{
#'       \item `data/raw/` - immutable raw data + dictionaries/codebooks
#'       \item `data/processed/` - cleaned datasets and dictionaries
#'       \item `data/outputs/` - outputs from processing/analysis scripts
#'         \itemize{
#'           \item `plots/` - figures (`.png`, `.pdf`, etc.)
#'           \item `fitted_models/` - saved model objects (e.g., `.rds`)
#'           \item `results/` - tables, summaries, descriptive statistics
#'         }
#'     }
#'   \item `methods/` - measurement instruments, implementation files
#'   \item `preregistration/` - preregistration documents
#'   \item `tools/` - utility scripts and reproducibility helpers
#' }
#'
#' Directories that would otherwise be empty receive an empty `.gitkeep` file
#' so that they are tracked by Git.
#'
#' The following files are created (if not already present):
#' \itemize{
#'   \item `LICENSE` - CC BY 4.0 license text
#'   \item `README.md` - skeleton README describing project aims and structure
#'   \item `<project_name>.Rproj` - RStudio project file that does not save or
#'     restore the workspace (if `rproj = TRUE`)
#'   \item `.gitignore` - ignores R history, session data, caches, temp files,
#'     OS-specific clutter, and large output directories
#'   \item `code/analysis.qmd` - Quarto analysis template with metadata, setup
#'     chunk, and `sessionInfo()` chunk
#'   \item `code/processing.qmd` - Quarto processing template (same structure)
#'   \item `tools/project_creator.qmd` - re-runs `create_project_skeleton()`
#'     from within the project, with an optional (not evaluated by default)
#'     chunk calling `delete_project_skeleton()` for testing
#'   \item `tools/project_validator.qmd` - runs `validator()` on the project
#'   \item `tools/style_all_files.qmd` - reproducibility tool to apply
#'     tidyverse code style to all `.qmd`, `.Rmd`, and `.R` files
#' }
#'
#' Quarto `.qmd` files are pre-filled with:
#' \itemize{
#'   \item YAML front matter including title, author, date, HTML output settings
#'   \item Global options to suppress scientific notation, warnings, and messages
#'   \item Setup chunk for loading packages and configuring the session
#'   \item Session information chunk
#' }
#'
#' @return
#' (Invisibly) a `data.frame` with one row per directory or file the function
#' handled, containing:
#' \describe{
#'   \item{path}{Path relative to `project_root`.}
#'   \item{type}{Either `"dir"` or `"file"`.}
#'   \item{status}{`"created"` if it did not exist, `"overwritten"` if it
#'     existed and `overwrite = TRUE`, `"skipped"` if a file existed and
#'     `overwrite = FALSE`, or `"exists"` if a directory already existed.}
#' }
#'
#' @examples
#' \dontrun{
#' # Create a skeleton in a parent directory
#' create_project_skeleton(project_root = "../", overwrite = FALSE)
#'
#' # Create in the current working directory and overwrite any existing templates
#' create_project_skeleton(project_root = ".", overwrite = TRUE)
#'
#' # Create in a specified directory
#' create_project_skeleton("~/path/to/github_repository_name", overwrite = FALSE)
#' }
#'
#' @seealso [psych-DS specification](https://psych-ds.github.io/)
#'
#' @export

# \item `tools/detect_unused_dependencies.qmd` - reproducibility tool to
#   check whether there are unused dependencies in a project
# \item `tools/detect_unused_objects.qmd` - reproducibility tool to
#   check whether there are unused objects in a project

create_project_skeleton <- function(
  project_root = "../",
  overwrite = FALSE,
  rproj = TRUE,
  quiet = FALSE
) {
  # minimal dependencies: base R only
  join <- function(...) file.path(..., fsep = .Platform$file.sep)

  # record every directory/file handled, with paths relative to project_root
  log <- list()
  root_prefix_n <- nchar(join(project_root, ""))
  record <- function(path, type, status) {
    log[[length(log) + 1]] <<- data.frame(
      path = substring(path, root_prefix_n + 1),
      type = type,
      status = status
    )
  }
  mkd <- function(p) {
    if (dir.exists(p)) {
      record(p, "dir", "exists")
    } else {
      dir.create(p, recursive = TRUE, showWarnings = FALSE)
      record(p, "dir", "created")
    }
  }
  write_if_absent <- function(path, text) {
    existed <- file.exists(path)
    if (existed && !overwrite) {
      record(path, "file", "skipped")
      return(invisible(FALSE))
    }
    cat(text, file = path)
    record(path, "file", if (existed) "overwritten" else "created")
    invisible(TRUE)
  }

  dirs <- c(
    "code",
    "reports",
    "data",
    "data/raw",
    "data/processed",
    "data/outputs",
    "data/outputs/plots",
    "data/outputs/fitted_models",
    "data/outputs/results",
    "methods",
    "preregistration",
    "tools" # <- added to ensure the tools dir exists
  )

  # create all directories
  paths_dir <- file.path(project_root, dirs)
  invisible(lapply(paths_dir, mkd))

  # .gitkeep files so that otherwise-empty directories are tracked by Git
  gitkeep_dirs <- c(
    "reports",
    "data/raw",
    "data/processed",
    "data/outputs/plots",
    "data/outputs/fitted_models",
    "data/outputs/results",
    "methods",
    "preregistration"
  )
  invisible(lapply(gitkeep_dirs, function(d) {
    write_if_absent(join(project_root, d, ".gitkeep"), "")
  }))

  # --- files: LICENSE (CC BY 4.0) & README ---
  license_path <- join(project_root, "LICENSE")
  license_text <- paste(
    "Creative Commons Attribution 4.0 International (CC BY 4.0)\n",
    "This work is licensed under the Creative Commons Attribution 4.0",
    "International License. You are free to share and adapt the material for",
    "any purpose, even commercially, under the terms below:",
    "",
    "  - Attribution - You must give appropriate credit, provide a link to the",
    "    license, and indicate if changes were made.",
    "",
    "No additional restrictions - You may not apply legal terms or technological",
    "measures that legally restrict others from doing anything the license permits.",
    "",
    "Full license text: https://creativecommons.org/licenses/by/4.0/legalcode",
    sep = "\n"
  )
  write_if_absent(license_path, license_text)

  # helper for string concatenation
  `%+%` <- function(a, b) paste0(a, b)

  readme_path <- join(project_root, "README.md")
  readme_text <- paste(
    "# Project Title",
    "",
    "## Overview",
    "Add aims, data sources, and reproduction steps.",
    "",
    "## Structure",
    "```\n" %+%
      "code/                 # analysis and processing scripts (.qmd/.Rmd) and their rendered .html\n" %+%
      "reports/              # thesis, manuscript, preprints, slides, etc.\n" %+%
      "data/\n" %+%
      "  raw/                # raw data and codebooks/data dictionaries (should be read-only, except for removal of private data)\n" %+%
      "  processed/          # cleaned datasets and codebooks/data dictionaries\n" %+%
      "  outputs/            # outputs of the processing and analyses scripts\n" %+%
      "    plots/            # plots and figures, .png/.pdf/etc.\n" %+%
      "    fitted_models/    # fitted model objects, eg from brms, lme4, lavaan, etc.\n" %+%
      "    results/          # tables and matrices, eg for descriptive statistics, formatted statistical results, correlation tables\n" %+%
      "methods/              # measures, implementations (qualtrics, lab.js, psychopy files, etc.), .docx files with items, etc.\n" %+%
      "preregistration/      # preregistration documents\n" %+%
      "tools/                # utility scripts, e.g., project validator and code styler\n" %+%
      "LICENSE               # suggested: CC BY 4.0\n" %+%
      "README.md             # this file\n" %+%
      "*.Rproj               # RStudio project file: open this to work on the project in RStudio\n" %+%
      "```",
    "",
    "## Reproducibility",
    "- Place raw data in `data/raw/`.",
    "- Write processing in `code/processing.qmd` and analyses in `code/analysis.qmd`.",
    "- Re-run data processing with `code/processing.qmd`. This will create `code/processing.html` and files in `data/processed/`.",
    "- Re-run analyses with `code/analysis.qmd`. This will create `code/analysis.html`, plots in `data/outputs/plots/`, fitted model objects in `data/outputs/fitted_models/`, and tables in `data/outputs/results/`.",
    "",
    "## License",
    "CC BY 4.0 (see `LICENSE`).",
    "",
    "## Suggested citation",
    "Authors (Year). Title. URL.",
    sep = "\n"
  )
  write_if_absent(readme_path, readme_text)

  # --- .gitignore ---
  gitignore_path <- join(project_root, ".gitignore")
  gitignore_text <- paste(
    "# History files",
    ".Rhistory",
    ".Rapp.history",
    "",
    "# Session Data files",
    ".RData",
    "",
    "# User-specific files",
    ".Rproj.user/",
    "",
    "# Quarto / R Markdown caches",
    "_cache/",
    "*/_cache/",
    "*.knit.md",
    "*.utf8.md",
    "",
    "# Temporary files",
    "*.tmp",
    "*.log",
    "",
    "# Large data (use Git LFS or external storage)",
    "data/outputs/fitted_models/*",
    "!data/outputs/fitted_models/.gitkeep",
    "data/outputs/plots/*",
    "!data/outputs/plots/.gitkeep",
    "",
    "# OS-specific files",
    ".DS_Store",
    "Thumbs.db",
    sep = "\n"
  )
  write_if_absent(gitignore_path, gitignore_text)

  # --- .gitattributes ---
  gitattributes_path <- join(project_root, ".gitattributes")
  gitattributes_text <- paste(
    "# Auto detect text files and perform LF normalization",
    "* text=auto",
    "",
    "# Prevent GitHub Linguist from detecting generated HTML",
    "*.html linguist-detectable=false",
    sep = "\n"
  )
  write_if_absent(gitattributes_path, gitattributes_text)

  # --- .Rproj ---
  rproj_path <- character(0)
  if (rproj) {
    existing_rproj <- list.files(project_root, pattern = "\\.Rproj$")
    if (length(existing_rproj) > 0) {
      rproj_path <- join(project_root, existing_rproj[1])
    } else {
      project_name <- basename(normalizePath(project_root, mustWork = FALSE))
      rproj_path <- join(project_root, paste0(project_name, ".Rproj"))
    }
    rproj_text <- paste(
      "Version: 1.0",
      "",
      "RestoreWorkspace: No",
      "SaveWorkspace: No",
      "AlwaysSaveHistory: Default",
      "",
      "EnableCodeIndexing: Yes",
      "UseSpacesForTab: Yes",
      "NumSpacesForTab: 2",
      "Encoding: UTF-8",
      "",
      "RnwWeave: Sweave",
      "LaTeX: pdfLaTeX",
      "",
      sep = "\n"
    )
    write_if_absent(rproj_path, rproj_text)
  }

  # shared YAML header for all generated .qmd files
  yaml_header <- function(title) {
    paste(
      "---",
      paste0("title: \"", title, "\""),
      "author: \"author goes here\"",
      "date: today",
      "editor: source",
      "format:",
      "  html:",
      "    theme:",
      "      light: flatly",
      "      dark: darkly",
      "    toc: true",
      "    toc-location: right",
      "    toc-depth: 3",
      "    number-sections: true",
      "    code-fold: show",
      "    code-tools: true",
      "    code-copy: true",
      "    code-link: true",
      "    code-overflow: wrap",
      "    df-print: paged",
      "    embed-resources: true",
      "    fig-width: 7",
      "    fig-height: 5",
      "execute:",
      "  message: false",
      "  warning: false",
      "---",
      "",
      sep = "\n"
    )
  }

  # --- empty .qmd stubs ---
  qmd_files <- c(
    "code/analysis.qmd",
    "code/processing.qmd"
  )
  qmd_header <- function(title) {
    project_root_norm <- normalizePath(
      project_root,
      winslash = "/",
      mustWork = FALSE
    )
    title_norm <- normalizePath(title, winslash = "/", mustWork = FALSE)
    if (startsWith(title_norm, project_root_norm)) {
      title_clean <- substr(
        title_norm,
        nchar(project_root_norm) + 2,
        nchar(title_norm)
      )
    } else {
      title_clean <- title
    }
    title_clean <- sub("^code/", "", title_clean)
    title_clean <- sub("\\.qmd$", "", title_clean)
    paste0(
      yaml_header(title_clean),
      "\n",
      "```{r}\n",
      "#| label: setup\n",
      "#| include: false\n",
      "# Turn off scientific notation globally\n",
      "options(scipen = 999)\n",
      "```\n\n",
      "# Dependencies\n",
      "```{r}\n",
      "# packages and setup here\n",
      "```\n\n",
      "# Session info\n",
      "```{r}\n",
      "sessionInfo()\n",
      "```\n"
    )
  }
  invisible(lapply(qmd_files, function(rel) {
    write_if_absent(
      join(project_root, rel),
      qmd_header(gsub("^code/|\\.qmd$", "", rel))
    )
  }))

  # --- tools/project_validator.qmd ---
  tools_validator_qmd_path <- join(
    project_root,
    "tools",
    "project_validator.qmd"
  )
  tools_validator_qmd_text <- paste(
    yaml_header("Check repository compliance against psych-ds-ish standard"),
    "```{r}",
    "",
    "library(psychdsish)",
    "library(knitr)",
    "library(kableExtra)",
    "",
    'results <- validator("../")',
    "",
    "results |>",
    "  knitr::kable() |>",
    "  kableExtra::kable_classic(full_width = FALSE)",
    "```",
    sep = "\n"
  )
  write_if_absent(tools_validator_qmd_path, tools_validator_qmd_text)

  # --- tools/project_creator.qmd ---
  tools_creator_qmd_path <- join(project_root, "tools", "project_creator.qmd")
  tools_creator_qmd_text <- paste(
    yaml_header("Create a project skeleton following the psych-ds-ish standard"),
    "## Create project skeleton",
    "",
    "```{r}",
    "",
    "library(psychdsish)",
    "",
    'create_project_skeleton(project_root = "../")',
    "",
    "```",
    "",
    "Note that you could also do this from the console without this .qmd file, if you know your project's file path, e.g., `psychdsish::create_project_skeleton(project_root = \"~/git/my_project\")`.",
    "",
    "## Delete project files and directories except this file",
    "",
    "For testing.",
    "",
    "WARNING: DELETES ALL FILES IN PARENT DIRECTORY OTHER THAN THE CURRENT FILE!",
    "",
    "```{r}",
    "#| eval: false",
    "#| include: false",
    "",
    'delete_project_skeleton(project_root = "../")',
    "",
    "```",
    sep = "\n"
  )
  write_if_absent(tools_creator_qmd_path, tools_creator_qmd_text)

  # --- tools/style_all_files.qmd ---
  tools_style_qmd_path <- join(project_root, "tools", "style_all_files.qmd")
  tools_style_qmd_text <- paste(
    yaml_header("Apply {tidyverse} code style to all .qmd, .Rmd, and .R files in a project"),
    "```{r}",
    "",
    "library(psychdsish)",
    "",
    'style_all_files(root = "../")',
    "",
    "```",
    sep = "\n"
  )
  write_if_absent(tools_style_qmd_path, tools_style_qmd_text)

  # # --- tools/detect_unused_dependencies.qmd ---
  # tools_dependencies_qmd_path <- join(project_root, "tools", "check_unused_dependencies.qmd")
  # tools_dependencies_qmd_text <- paste(
  #   "---",
  #   'title: "Check if there are unused dependencies in a project"',
  #   "format:",
  #   "  html:",
  #   "    toc: true",
  #   "    code-fold: true",
  #   "execute:",
  #   "  warning: false",
  #   "  message: false",
  #   "---",
  #   "",
  #   "```{r}",
  #   "",
  #   "library(psychdsish)",
  #   "library(knitr)",
  #   "library(kableExtra)",
  #   "",
  #   "res <- check_unused_dependencies(root = '../')",
  #   "",
  #   "res |>",
  #   "  kable() |>",
  #   "  kable_classic(full_width = FALSE)",
  #   "",
  #   "```",
  #   sep = "\n"
  # )
  # write_if_absent(tools_dependencies_qmd_path, tools_dependencies_qmd_text)
  #
  #
  # # --- tools/check_unused_objects.qmd ---
  # tools_unused_objects_qmd_path <- join(project_root, "tools", "check_unused_objects.qmd")
  # tools_unused_objects_qmd_text <- paste(
  #   "---",
  #   'title: "Check if there are unused objects in a project"',
  #   "format:",
  #   "  html:",
  #   "    toc: true",
  #   "    code-fold: true",
  #   "execute:",
  #   "  warning: false",
  #   "  message: false",
  #   "---",
  #   "",
  #   "When learning to code, it's very easy to accidentally create objects (e.g., data frames) and then never use them in your code. Sometimes, users create 'df2' from 'df1' but, later in the code, go back to calling 'df1'. These 'orphan' objects can represent errors or generally make code confusing - why create an object that is never used?",
  #   "",
  #   "This function lets you scan all .qmd, .Rmd, and .R files in your project for unused 'orphan' objects. If you find your project contains them, you should think about whether they're redundant and can be removed, or whether maybe you have an error (e.g., maybe subsequent code should call these objects and not others).",
  #   "",
  #   "```{r}",
  #   "",
  #   "library(psychdsish)",
  #   "library(knitr)",
  #   "library(kableExtra)",
  #   "",
  #   "res <- check_unused_objects(root = '../')",
  #   "",
  #   "res |>",
  #   "  kable() |>",
  #   "  kable_classic(full_width = FALSE)",
  #   "",
  #   "```",
  #   sep = "\n"
  # )
  # write_if_absent(tools_unused_objects_qmd_path, tools_unused_objects_qmd_text)

  # return a summary
  created <- do.call(rbind, log)

  if (!quiet) {
    n <- function(x) sum(created$type == "file" & created$status == x)
    msg <- sprintf(
      "Created %d, overwrote %d, and skipped %d existing files in %s",
      n("created"),
      n("overwritten"),
      n("skipped"),
      normalizePath(project_root, mustWork = FALSE)
    )
    if (n("skipped") > 0) {
      msg <- paste0(msg, " (use overwrite = TRUE to replace them)")
    }
    message(msg, ".")
  }

  invisible(created)
}
