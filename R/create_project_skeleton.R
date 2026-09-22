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
#'   the project skeleton should be created, e.g., `"~/git/my_project"` from
#'   the console, or `"../"` from `tools/project_creator.qmd` in the project.
#'   There is no default, so that files are only written where you say.
#' @param overwrite Logical. If `TRUE`, existing files will be overwritten.
#'   Defaults to `FALSE`.
#' @param rproj Logical. If `TRUE` (default), an RStudio project file named
#'   after the project root directory (e.g., `my_project.Rproj`) is created,
#'   unless the project root already contains an `.Rproj` file.
#' @param quarto_yml Logical. If `TRUE` (default), a `_quarto.yml` file is
#'   created that makes the project a Quarto project, so that rendering the
#'   project renders `code/processing.qmd` and then `code/analysis.qmd`.
#' @param studies Whole number. The number of studies in the project.
#'   Defaults to `1`, a single-study project. With more than one study, each
#'   study gets its own `code/`, `data/`, `methods/`, and `preregistration/`
#'   folders, arranged according to `layout`.
#' @param layout Character. How a multi-study project is arranged (ignored if
#'   `studies = 1`):
#'   * `"by_study"` (default): one folder per study at the project root,
#'     each with the single-study structure, e.g.,
#'     `study_1/code/processing.qmd` and `study_1/data/raw/`. Paths in the
#'     code are the same as in a single-study project (e.g., `../data/raw/`).
#'   * `"by_type"`: one subfolder per study inside each folder, e.g.,
#'     `code/study_1/processing.qmd` and `data/raw/study_1/`. Paths in the
#'     code gain a level (e.g., `../../data/raw/study_1/`).
#'
#'   In both layouts, `reports/` and `tools/` are shared by all studies at
#'   the project root. Re-running with a larger `studies` adds the new
#'   studies' folders and `.qmd` files without changing existing files.
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
#' # These examples write to a temporary folder; use your project's path instead,
#' # e.g., create_project_skeleton("~/git/my_project")
#' project <- file.path(tempdir(), "my_project")
#' create_project_skeleton(project)
#' list.files(project, all.files = TRUE)
#'
#' # Running it again skips existing files, unless overwrite = TRUE
#' create_project_skeleton(project)
#'
#' # A project with two studies, one folder per study (study_1/code/, ...)
#' two_studies <- file.path(tempdir(), "two_studies")
#' create_project_skeleton(two_studies, studies = 2)
#'
#' # The same, with one subfolder per study inside each folder (code/study_1/, ...)
#' by_type <- file.path(tempdir(), "two_studies_by_type")
#' create_project_skeleton(by_type, studies = 2, layout = "by_type")
#'
#' unlink(c(project, two_studies, by_type), recursive = TRUE)
#'
#' @seealso [psych-DS specification](https://psych-ds.github.io/)
#'
#' @export

# \item `tools/detect_unused_dependencies.qmd` - reproducibility tool to
#   check whether there are unused dependencies in a project
# \item `tools/detect_unused_objects.qmd` - reproducibility tool to
#   check whether there are unused objects in a project

create_project_skeleton <- function(
  project_root,
  overwrite = FALSE,
  rproj = TRUE,
  quarto_yml = TRUE,
  studies = 1,
  layout = c("by_study", "by_type"),
  quiet = FALSE
) {
  studies <- as.integer(studies)
  if (length(studies) != 1 || is.na(studies) || studies < 1) {
    stop("`studies` must be a single whole number of 1 or more.", call. = FALSE)
  }
  layout <- match.arg(layout)
  multi <- studies > 1
  lay <- if (multi) layout else "single"
  study_ids <- if (multi) study_names(studies) else "study_1"

  # project-relative path of a folder, for study 1 by default
  P <- function(logical, study = study_ids[1]) layout_path(logical, study, lay)

  # minimal dependencies: base R only
  join <- function(...) file.path(..., fsep = .Platform$file.sep)

  # record every directory/file handled, with paths relative to project_root
  log <- list()
  # length of "<project_root>/", measured from a child path because Windows
  # drops the trailing separator from file.path(project_root, "")
  root_prefix_n <- nchar(join(project_root, "x")) - 1
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
    # text files end with a newline
    if (nzchar(text) && !endsWith(text, "\n")) {
      text <- paste0(text, "\n")
    }
    cat(text, file = path)
    record(path, "file", if (existed) "overwritten" else "created")
    invisible(TRUE)
  }

  dirs <- layout_all_dirs(lay, study_ids)

  # create all directories
  paths_dir <- file.path(project_root, dirs)
  invisible(lapply(paths_dir, mkd))

  # .gitkeep files so that otherwise-empty directories are tracked by Git
  gitkeep_dirs <- c(
    "reports",
    unlist(lapply(study_ids, function(s) {
      vapply(setdiff(layout_study_dirs, "code"), function(d) P(d, s), "")
    }))
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
    readme_tree(lay, study_ids),
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
  layout_hint <- switch(
    lay,
    by_study = "the other studies' folders are named `study_2/`, `study_3/`, etc.",
    by_type = "the other studies' folders end in `study_2/`, `study_3/`, etc.",
    ""
  )
  qmd_order <- unlist(lapply(study_ids, function(s) {
    paste0(P("code", s), c("/processing.qmd", "/analysis.qmd"))
  }))
  render_order_list <- paste0(seq_along(qmd_order), ". `", qmd_order, "`")

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
      "Rendering stops at the first error, so analyses never run on stale or partially processed data. Each file runs with its own folder as the working directory, so paths in the code are relative to the file's folder (e.g., from `@@CODE@@/`, raw data is in `@@REL_RAW@@/`).",
      "",
      "Clicking *Render* in an individual `.qmd` file renders only that file. Use it while developing, but render the whole project before sharing results.",
      "",
      "### Adding new files",
      "If you add another processing or analysis file (e.g., `@@CODE@@/processing_part_2.qmd`), add it to the `render:` list in `_quarto.yml` in the position it should run, otherwise it will not be rendered with the rest of the project."
    )
  } else {
    c(
      "Render the files in this order, e.g., by clicking *Render* in each file:",
      "",
      render_order_list,
      "",
      "Each file runs with its own folder as the working directory, so paths in the code are relative to the file's folder (e.g., from `@@CODE@@/`, raw data is in `@@REL_RAW@@/`)."
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
      "This project was created with [psychdsish](https://github.com/ianhussey/psychdsish), an R package that sets up a standardised project structure loosely based on the [psych-DS](https://psych-ds.github.io/) standard.",
      "",
      "To check that the project still follows this standard, use any one of these options:",
      "",
      "- **RStudio:** with the project open, click *Addins > Validate psych-DS-ish project* in the toolbar. The results appear in the console and as a table in the Viewer pane.",
      "- **Report:** render `tools/project_validator.qmd`.",
      "- **R console:** from the project root, run:",
      "",
      "```r",
      "# install.packages(\"remotes\")",
      "# remotes::install_github(\"ianhussey/psychdsish\")",
      "psychdsish::validator(\".\")",
      "```",
      "",
      "This lists each check as PASS, FAIL, or SKIP, with guidance on how to fix any failures. Use `psychdsish::validator(\".\", strict = TRUE)` to throw an error if any check fails, e.g., to fail a GitHub Actions job.",
      "",
      "## Reproducibility",
      "",
      "### Workflow",
      if (multi) {
        paste0("Each study has its own folders and follows the same workflow. Paths below are for study 1; ", layout_hint)
      },
      "- Raw data lives in `@@RAW@@/` and is never modified by code.",
      "- `@@CODE@@/processing.qmd` reads the raw data and writes cleaned datasets to `@@PROCESSED@@/`. Rendering it also creates `@@CODE@@/processing.html`.",
      "- `@@CODE@@/analysis.qmd` reads the processed data and writes plots to `@@PLOTS@@/`, fitted model objects to `@@MODELS@@/`, and tables to `@@RESULTS@@/`. Rendering it also creates `@@CODE@@/analysis.html`.",
      "",
      if (multi) {
        c(
          "### Adding a study",
          paste0(
            "Increase `studies` in `tools/project_creator.qmd` and render it: this creates the new study's folders and .qmd files, and leaves existing files untouched.",
            if (quarto_yml) " Then add the new study's processing and analysis files to the `render:` list in `_quarto.yml`." else ""
          ),
          ""
        )
      },
      "### Reproduce all results",
      readme_render,
      "",
      "## Codebooks",
      "Every data file in `@@PROCESSED_ALL@@/` should have a codebook (data dictionary) that describes each of its variables, named after the data file (e.g., `study_1_data.csv` -> `study_1_codebook.csv`).",
      "",
      "`@@CODE@@/processing.qmd` contains a chunk that creates the codebook from the processed data. It fills in each variable's type, number of missing values, and range or values, and marks the columns that only you can complete as \"TO BE COMPLETED MANUALLY\":",
      "",
      "- `description`: what the variable is, e.g., the item wording, or how a score was calculated.",
      "- `units`: e.g., years or milliseconds. Write \"none\" if it does not apply.",
      "- `coding`: what the values mean, e.g., \"1 = strongly disagree to 7 = strongly agree\", reverse-scored items, or missing-value codes such as -99.",
      "",
      "Open the .csv (e.g., in Excel), replace every placeholder, and save it as .csv. Re-rendering `@@CODE@@/processing.qmd` keeps your entries, adds new variables, and removes variables that are no longer in the data. `psychdsish::validator()` reports data files without a codebook, and codebooks that still contain placeholders.",
      "",
      "**Using AI assistants:** an AI assistant can help draft descriptions, but only from information it can actually see. For example, ask it to read `@@CODE@@/processing.qmd` and describe how each variable was created. It cannot know what your items said or what your codes mean, and will guess convincingly if asked. Check every entry against your study materials (e.g., in `@@METHODS@@/`), and do not keep any description you cannot verify.",
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
  readme_tokens <- c(
    "@@CODE@@" = P("code"),
    "@@RAW@@" = P("data/raw"),
    "@@PROCESSED_ALL@@" = layout_label("data/processed", lay),
    "@@PROCESSED@@" = P("data/processed"),
    "@@PLOTS@@" = P("data/outputs/plots"),
    "@@MODELS@@" = P("data/outputs/fitted_models"),
    "@@RESULTS@@" = P("data/outputs/results"),
    "@@METHODS@@" = P("methods"),
    "@@REL_RAW@@" = rel_path(P("code"), P("data/raw"))
  )
  for (tok in names(readme_tokens)) {
    readme_text <- gsub(tok, readme_tokens[[tok]], readme_text, fixed = TRUE)
  }

  write_if_absent(readme_path, readme_text)

  # --- .gitignore ---
  gitignore_path <- join(project_root, ".gitignore")
  gitignore_text <- paste(
    c(
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
      unlist(lapply(c("data/outputs/fitted_models", "data/outputs/plots"), function(d) {
        c(
          paste0(layout_label(d, lay), "/*"),
          paste0("!", layout_label(d, lay), "/.gitkeep")
        )
      })),
      "",
      "# OS-specific files",
      ".DS_Store",
      "Thumbs.db"
    ),
    collapse = "\n"
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
      c(
        "# Rendering the project (RStudio: Build > Render Project;",
        "# R console: quarto::quarto_render(); terminal: quarto render)",
        "# renders the files below in the order they are listed.",
        "# Add new .qmd files to this list in the order they should run.",
        "project:",
        "  type: default",
        "  render:",
        paste0("    - ", qmd_order),
        "  # run each file with its own folder as the working directory",
        "  execute-dir: file",
        ""
      ),
      collapse = "\n"
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
  # codebook chunk, added to each processing.qmd only
  codebook_section <- paste(
    c(
      "# Codebook",
      "",
      "A codebook (data dictionary) describes every variable in the processed data. The chunk below creates `../data/processed/processed_codebook.csv` from `data_processed`, or updates it if it already exists. The automatic columns (`type`, `n_missing`, `values`) are refreshed on every render. The manual columns (`description`, `units`, `coding`) start as \"TO BE COMPLETED MANUALLY\": open the .csv (e.g., in Excel), replace them for every variable (write \"none\" where a column does not apply), and save it as .csv. Your entries are kept when the codebook is updated.",
      "",
      "```{r}",
      "#| label: codebook",
      "",
      "# Rename `data_processed` and the file names to match your data. Save the data",
      "# with a name ending in \"_data\" and the codebook with the same name ending in",
      "# \"_codebook\", so that psychdsish::validator() can match them, e.g.:",
      "# write.csv(data_processed, \"../data/processed/processed_data.csv\", row.names = FALSE)",
      "",
      "placeholder <- \"TO BE COMPLETED MANUALLY\"",
      "codebook_path <- \"../data/processed/processed_codebook.csv\"",
      "",
      "# range for numeric and date variables, unique values otherwise",
      "summarise_values <- function(x) {",
      "  if (all(is.na(x))) {",
      "    return(\"all missing\")",
      "  }",
      "  if (is.numeric(x) || inherits(x, c(\"Date\", \"POSIXt\"))) {",
      "    return(paste(min(x, na.rm = TRUE), \"to\", max(x, na.rm = TRUE)))",
      "  }",
      "  values <- sort(unique(as.character(x[!is.na(x)])))",
      "  if (length(values) > 10) {",
      "    paste0(length(values), \" unique values, e.g., \", paste(head(values, 3), collapse = \"; \"))",
      "  } else {",
      "    paste(values, collapse = \"; \")",
      "  }",
      "}",
      "",
      "if (exists(\"data_processed\")) {",
      "  codebook <- data.frame(",
      "    variable = names(data_processed),",
      "    type = vapply(data_processed, function(x) class(x)[1], character(1)),",
      "    n_missing = vapply(data_processed, function(x) sum(is.na(x)), integer(1)),",
      "    values = vapply(data_processed, summarise_values, character(1)),",
      "    description = placeholder,",
      "    units = placeholder,",
      "    coding = placeholder",
      "  )",
      "",
      "  # keep the manual entries from an existing codebook",
      "  if (file.exists(codebook_path)) {",
      "    existing <- read.csv(codebook_path, colClasses = \"character\")",
      "    matched <- match(codebook$variable, existing$variable)",
      "    for (col in intersect(c(\"description\", \"units\", \"coding\"), names(existing))) {",
      "      codebook[[col]] <- ifelse(is.na(matched), placeholder, existing[[col]][matched])",
      "    }",
      "    dropped <- setdiff(existing$variable, codebook$variable)",
      "    if (length(dropped) > 0) {",
      "      message(\"Removed from the codebook (no longer in the data): \", paste(dropped, collapse = \", \"))",
      "    }",
      "  }",
      "",
      "  write.csv(codebook, codebook_path, row.names = FALSE)",
      "  codebook",
      "} else {",
      "  message(\"Create `data_processed` above to generate its codebook.\")",
      "}",
      "```",
      "",
      ""
    ),
    collapse = "\n"
  )
  qmd_header <- function(title, codebook = NULL) {
    paste0(
      yaml_header(title),
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
      if (is.null(codebook)) "" else codebook,
      "# Session info\n",
      "```{r}\n",
      "sessionInfo()\n",
      "```\n"
    )
  }
  qmd_files <- character(0)
  for (s in study_ids) {
    code_dir <- P("code", s)
    prefix <- if (multi) paste0("Study ", sub("^study_", "", s), ": ") else ""
    # codebook paths are relative to the .qmd's folder
    codebook <- gsub(
      "../data/processed",
      rel_path(code_dir, P("data/processed", s)),
      codebook_section,
      fixed = TRUE
    )
    for (f in c("processing", "analysis")) {
      rel <- paste0(code_dir, "/", f, ".qmd")
      qmd_files <- c(qmd_files, rel)
      write_if_absent(
        join(project_root, rel),
        qmd_header(
          paste0(prefix, f),
          codebook = if (f == "processing") codebook
        )
      )
    }
  }

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
  # re-running it must recreate the same layout, so non-default settings are
  # written into the call
  creator_args <- paste(
    c(
      'project_root = "../"',
      if (multi) paste0("studies = ", studies),
      if (multi) paste0('layout = "', layout, '"'),
      if (!rproj) "rproj = FALSE",
      if (!quarto_yml) "quarto_yml = FALSE"
    ),
    collapse = ", "
  )
  tools_creator_qmd_path <- join(project_root, "tools", "project_creator.qmd")
  tools_creator_qmd_text <- paste(
    yaml_header("Create a project skeleton following the psych-ds-ish standard"),
    "## Create project skeleton",
    "",
    "```{r}",
    "",
    "library(psychdsish)",
    "",
    paste0("create_project_skeleton(", creator_args, ")"),
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
