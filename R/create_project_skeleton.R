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
#' @param quarto_yml Logical. If `TRUE` (default), a `_quarto.yml` file is
#'   created that makes the project a Quarto project, so that rendering the
#'   project renders `code/processing.qmd` and then `code/analysis.qmd`.
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
#'   \item `_quarto.yml` - Quarto project file listing the `.qmd` files to
#'     render, in order, when the whole project is rendered (processing
#'     before analysis) (if `quarto_yml = TRUE`)
#'   \item `CITATION.cff` - citation metadata template; GitHub uses it to show
#'     a "Cite this repository" button
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
  quarto_yml = TRUE,
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

  readme_path <- join(project_root, "README.md")
  readme_structure <- c(
    "code/                 # analysis and processing scripts (.qmd/.Rmd) and their rendered .html",
    "reports/              # thesis, manuscript, preprints, slides, etc.",
    "data/",
    "  raw/                # raw data and codebooks/data dictionaries (should be read-only, except for removal of private data)",
    "  processed/          # cleaned datasets and codebooks/data dictionaries",
    "  outputs/            # outputs of the processing and analyses scripts",
    "    plots/            # plots and figures, .png/.pdf/etc.",
    "    fitted_models/    # fitted model objects, eg from brms, lme4, lavaan, etc.",
    "    results/          # tables and matrices, eg for descriptive statistics, formatted statistical results, correlation tables",
    "methods/              # measures, implementations (qualtrics, lab.js, psychopy files, etc.), .docx files with items, etc.",
    "preregistration/      # preregistration documents",
    "tools/                # utility scripts, e.g., project validator and code styler",
    "CITATION.cff          # citation metadata: gives the 'Cite this repository' button on GitHub",
    "LICENSE               # suggested: CC BY 4.0",
    "README.md             # this file",
    if (rproj) {
      "*.Rproj               # RStudio project file: open this to work on the project in RStudio"
    },
    if (quarto_yml) {
      "_quarto.yml           # lists which .qmd files to render, in order, when rendering the whole project"
    }
  )

  readme_render <- if (quarto_yml) {
    c(
      "`_quarto.yml` lists the files to render and the order to render them in (processing before analysis). Render the whole project using any one of these options:",
      "",
      if (rproj) {
        "- **RStudio:** open the `.Rproj` file, then click *Build > Render Project* in the Build pane (top right)."
      },
      if (rproj) {
        "- **R console:** with the working directory set to the project root (automatic when the `.Rproj` file is open), run `quarto::quarto_render()`."
      } else {
        "- **R console:** with the working directory set to the project root, run `quarto::quarto_render()`."
      },
      "- **Terminal:** from the project root, run `quarto render`.",
      "",
      "Rendering stops at the first error, so analyses never run on stale or partially processed data. Each file runs with its own folder as the working directory, so paths in the code are relative to `code/` (e.g., `../data/raw/`).",
      "",
      "Clicking *Render* in an individual `.qmd` file renders only that file. Use it while developing, but render the whole project before sharing results.",
      "",
      "### Adding new files",
      "If you add another processing or analysis file (e.g., `code/processing_study_2.qmd`), add it to the `render:` list in `_quarto.yml` in the position it should run, otherwise it will not be rendered with the rest of the project."
    )
  } else {
    c(
      "Render the files in this order, e.g., by clicking *Render* in each file:",
      "",
      "1. `code/processing.qmd`",
      "2. `code/analysis.qmd`",
      "",
      "Each file runs with its own folder as the working directory, so paths in the code are relative to `code/` (e.g., `../data/raw/`)."
    )
  }

  readme_text <- paste(
    c(
      "# Project Title",
      "",
      "## Overview",
      "Add aims, data sources, and reproduction steps.",
      "",
      "## Structure",
      "```",
      readme_structure,
      "```",
      "",
      "## Reproducibility",
      "",
      "### Workflow",
      "- Raw data lives in `data/raw/` and is never modified by code.",
      "- `code/processing.qmd` reads the raw data and writes cleaned datasets to `data/processed/`. Rendering it also creates `code/processing.html`.",
      "- `code/analysis.qmd` reads the processed data and writes plots to `data/outputs/plots/`, fitted model objects to `data/outputs/fitted_models/`, and tables to `data/outputs/results/`. Rendering it also creates `code/analysis.html`.",
      "",
      "### Reproduce all results",
      readme_render,
      "",
      "## License",
      "CC BY 4.0 (see `LICENSE`).",
      "",
      "## Suggested citation",
      "Authors (Year). Title. URL.",
      "",
      "`CITATION.cff` holds this citation in a machine-readable format. On GitHub, it adds a *Cite this repository* button to the repository page (right-hand sidebar) that gives APA and BibTeX citations, and Zenodo reads it when archiving a release.",
      "",
      "To customise it, open `CITATION.cff` and replace the placeholders:",
      "",
      "- `title`: the project title.",
      "- `authors`: one `- family-names:` / `given-names:` block per author, in author order. Add each author's ORCID or delete the `orcid:` line.",
      "- `date-released`: the date of the version people should cite.",
      "- `repository-code`: the repository URL.",
      "- `doi`: uncomment and fill in once you have one (e.g., from a Zenodo release).",
      "",
      "Keep it consistent with the citation above. See https://citation-file-format.github.io for all available fields.",
      "",
      "After editing, check that the file is still valid. GitHub silently drops the *Cite this repository* button if it is not. From the project root, in the R console, run:",
      "",
      "```r",
      "# install.packages(\"cffr\")",
      "cffr::cff_validate(\"CITATION.cff\")",
      "```",
      "",
      "This reports whether the file is valid and, if not, which fields are wrong."
    ),
    collapse = "\n"
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
    ".quarto/",
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

  # --- _quarto.yml ---
  # defines the files rendered by `quarto render` and the order they run in
  if (quarto_yml) {
    quarto_yml_path <- join(project_root, "_quarto.yml")
    quarto_yml_text <- paste(
      "# Rendering the project (RStudio: Build > Render Project;",
      "# R console: quarto::quarto_render(); terminal: quarto render)",
      "# renders the files below in the order they are listed.",
      "# Add new .qmd files to this list in the order they should run.",
      "project:",
      "  type: default",
      "  render:",
      "    - code/processing.qmd",
      "    - code/analysis.qmd",
      "  # run each file with its own folder as the working directory",
      "  execute-dir: file",
      "",
      sep = "\n"
    )
    write_if_absent(quarto_yml_path, quarto_yml_text)
  }

  # --- CITATION.cff ---
  cff_path <- join(project_root, "CITATION.cff")
  cff_text <- paste(
    "# Citation metadata for this project, in Citation File Format (CFF).",
    "# On GitHub, this file adds a 'Cite this repository' button to the",
    "# repository page, which gives APA and BibTeX citations. Zenodo also",
    "# reads it when archiving a GitHub release.",
    "#",
    "# To customise: replace the placeholder values below, add one",
    "# '- family-names:' block per author, and uncomment 'doi' once you have",
    "# one. Documentation: https://citation-file-format.github.io",
    "cff-version: 1.2.0",
    "message: \"If you use this project, please cite it as below.\"",
    "title: \"title goes here\"",
    "authors:",
    "  - family-names: \"family name goes here\"",
    "    given-names: \"given name goes here\"",
    "    orcid: \"https://orcid.org/0000-0000-0000-0000\"",
    paste0("date-released: \"", format(Sys.Date(), "%Y-%m-%d"), "\""),
    "license: CC-BY-4.0",
    "repository-code: \"https://github.com/username/repository\"",
    "# doi: 10.5281/zenodo.0000000",
    "",
    sep = "\n"
  )
  write_if_absent(cff_path, cff_text)

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
