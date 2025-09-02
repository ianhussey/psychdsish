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
#' The following files are created (if not already present):
#' \itemize{
#'   \item `LICENSE` - CC BY 4.0 license text
#'   \item `readme.md` - skeleton README describing project aims and structure
#'   \item `.gitignore` - ignores R history, session data, caches, temp files,
#'     OS-specific clutter, and large output directories
#'   \item `code/analysis.qmd` - Quarto analysis template with metadata, setup
#'     chunk, and `sessionInfo()` chunk
#'   \item `code/processing.qmd` - Quarto processing template (same structure)
#'   \item `tools/style_all_files.qmd` - reproducibility tool to apply
#'     {tidyverse} code style to all `.qmd`, `.Rmd`, and `.R` files
#'   \item `tools/detect_unused_dependencies.qmd` - reproducibility tool to
#'     check whether there are unused dependencies in a project
#'   \item `tools/detect_unused_objects.qmd` - reproducibility tool to
#'     check whether there are unused objects in a project
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
#' (Invisibly) a `data.frame` listing the created paths and their type
#' (`"dir"` or `"file"`).
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
create_project_skeleton <- function(project_root = "../", overwrite = FALSE) {
  # minimal dependencies: base R only
  join <- function(...) file.path(..., fsep = .Platform$file.sep)
  mkd  <- function(p) if (!dir.exists(p)) dir.create(p, recursive = TRUE, showWarnings = FALSE)
  write_if_absent <- function(path, text) {
    if (file.exists(path) && !overwrite) return(invisible(FALSE))
    cat(text, file = path)
    invisible(TRUE)
  }
  touch <- function(path) {
    if (file.exists(path) && !overwrite) return(invisible(FALSE))
    file.create(path)
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
    "tools"                      # <- added to ensure the tools dir exists
  )
  
  # create all directories
  paths_dir <- file.path(project_root, dirs)
  invisible(lapply(paths_dir, mkd))
  
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
  
  readme_path <- join(project_root, "readme.md")
  readme_text <- paste(
    "# Project Title",
    "",
    "## Overview",
    "Add aims, data sources, and reproduction steps.",
    "",
    "## Structure",
    "```\n" %+%
      "code/                 # analysis and processing scripts (.qmd/.Rmd)\n" %+%
      "  models/             # fitted model objects (.rds)\n" %+%
      "  plots/              # generated figures (.png)\n" %+%
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
      "LICENSE               # suggested: CC BY 4.0\n" %+%
      "readme.md             # this file\n" %+%
      "```",
    "",
    "## Reproducibility",
    "- Place raw data in `data/raw/`.",
    "- Write processing in `code/processing.qmd` and analyses in `code/analyses.qmd`.",
    "- Re-run data processing with `code/processing.qmd`. This will create `code/processing.html` and files in `data/processed/` and `data/results/`.",
    "- Re-run analyses with `code/analysis.Rmd`. This will create `code/processing.html`, plots in `code/plots/` and fitted model objects in `code/models/`.",
    "",
    "## License",
    "CC BY 4.0 (see `LICENSE`).",
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
    "data/outputs/fitted_models/",
    "data/outputs/plots/",
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
  
  # --- empty .qmd stubs ---
  qmd_files <- c(
    "code/analysis.qmd",
    "code/processing.qmd"
  )
  qmd_header <- function(title) {
    project_root_norm <- normalizePath(project_root, winslash = "/", mustWork = FALSE)
    title_norm <- normalizePath(title, winslash = "/", mustWork = FALSE)
    if (startsWith(title_norm, project_root_norm)) {
      title_clean <- substr(title_norm, nchar(project_root_norm) + 2, nchar(title_norm))
    } else {
      title_clean <- title
    }
    title_clean <- sub("^code/", "", title_clean)
    title_clean <- sub("\\.qmd$", "", title_clean)
    paste0(
      "---\n",
      "title: \"", title_clean, "\"\n",
      "author: \"author\"\n",
      "date: today\n",
      "format:\n",
      "  html:\n",
      "    code-fold: true\n",
      "    highlight-style: haddock\n",
      "    theme: flatly\n",
      "    toc: true\n",
      "    toc-location: left\n",
      "execute:\n",
      "  warning: false\n",
      "  message: false\n",
      "---\n\n",
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
    write_if_absent(join(project_root, rel), qmd_header(gsub("^code/|\\.qmd$", "", rel)))
  }))
  
  # --- tools/style_all_files.qmd ---
  tools_style_qmd_path <- join(project_root, "tools", "style_all_files.qmd")
  tools_style_qmd_text <- paste(
    "---",
    'title: "Apply {tidyverse} code style to all .qmd, .Rmd, and .R files in a project"',
    "format:",
    "  html:",
    "    toc: true",
    "    code-fold: true",
    "execute:",
    "  warning: false",
    "  message: false",
    "---",
    "",
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
  created <- data.frame(
    path = c(
      paths_dir,
      license_path,
      readme_path,
      file.path(project_root, qmd_files),
      tools_style_qmd_path,
      tools_dependencies_qmd_path,
      tools_unused_objects_qmd_path
    ),
    type = c(
      rep("dir", length(paths_dir)),
      "file", "file", rep("file", length(qmd_files)), "file", "file", "file"
    )
  )
  invisible(created)
}