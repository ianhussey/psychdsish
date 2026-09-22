#' Validate a project directory structure against psych-ds-ish requirements
#'
#' This function checks whether a project directory follows a defined set
#' of structural and file-placement rules. The layout is loosely inspired by the
#' [psych-DS](https://psych-ds.github.io/) specification, but is **not**
#' fully psych-DS compliant. It adds extra conventions for reproducibility
#' and common analysis workflows, including Quarto file templates and
#' a `.gitignore` file. It does not comply with psych-ds's .json requirement.
#'
#' @param project_root Character scalar. Path to the project root directory.
#'   Defaults to `"../"`. The path is normalized internally.
#' @param strict Logical. If `TRUE`, throw an error when any check fails,
#'   listing the failed checks and how to fix them. Useful in continuous
#'   integration (e.g., GitHub Actions), where the error makes the job fail.
#'   Defaults to `FALSE`.
#'
#' @details
#' The validator performs several categories of checks:
#' \itemize{
#'   \item **Required directories** — Ensures that expected top-level and
#'         nested folders exist (e.g., `code/`, `data/raw/`, `methods/`).
#'   \item **Required files** — Confirms presence of a `readme.md` and
#'         a license file (e.g., `LICENSE`).
#'   \item **Filetype-location constraints** — Ensures specific file types
#'         (e.g., `.qmd`, `.csv`, `.rds`, `.png`) only appear in approved
#'         subdirectories and not elsewhere in the project.
#'   \item **Special HTML rule** — All `.html` files must be under `code/`
#'         or `methods/`.
#'   \item **Hygiene checks** — Optional rules that discourage spaces in
#'         filenames, check for `.gitignore` presence, and prevent data
#'         files being stored under `code/`.
#'   \item **Reproducibility checks** — Raw data files have not been modified
#'         or deleted since they were first committed to git (including
#'         uncommitted changes); rendered `.html` files are newer than their
#'         `.qmd` (using git commit times for committed, unmodified files, and
#'         file modification times otherwise); `README.md` no longer contains
#'         the template placeholders written by [create_project_skeleton()];
#'         and R code in `.R` files and in `.qmd`/`.Rmd` code chunks contains
#'         no `setwd()` calls or absolute file paths (e.g., `"~/"`,
#'         `"/Users/"`, `"C:/"`). Comment lines are ignored.
#'   \item **Codebook checks** — Every data file in `data/processed/` has a
#'         codebook named after it (e.g., `study_1_data.csv` ->
#'         `study_1_codebook.csv`, any data file extension), and `.csv`/`.tsv`
#'         codebooks no longer contain the "TO BE COMPLETED MANUALLY"
#'         placeholders written by the codebook chunk in `code/processing.qmd`.
#' }
#'
#' Checks that cannot be run are reported as `"SKIP"` rather than `"PASS"`,
#' e.g., the raw data check when the project is not a git repository, or the
#' `.html` check when nothing has been rendered yet. Skipped checks do not
#' cause `strict = TRUE` to fail.
#'
#' @return
#' A tibble of class `psychdsish_validation` with one row per check
#' (failures first, then skipped checks), containing:
#' \describe{
#'   \item{Test}{Description of the check performed.}
#'   \item{Status}{`"PASS"`, `"FAIL"`, or `"SKIP"` (the check could not be
#'         run).}
#'   \item{Details / Guidance}{Additional information, such as instructions
#'         for fixing failures or offending file paths.}
#' }
#' Printing it shows coloured PASS/FAIL results with guidance for failures.
#' `summary()` returns a list with `project_root`, `n_pass`, `n_fail`,
#' `n_skip`, and `passed` (`TRUE` if no check failed). If `strict = TRUE` and any check
#' fails, an error is thrown instead.
#'
#' @examples
#' \dontrun{
#' # Run validation on the parent directory
#' results <- validator("../")
#'
#' # Pretty-print results in the console
#' print(results)
#'
#' # Overall result
#' summary(results)$passed
#'
#' # Fail a CI job (e.g., GitHub Actions) if any check fails
#' validator(".", strict = TRUE)
#'
#' # Display results as a styled HTML table
#' library(knitr)
#' library(kableExtra)
#' results |>
#'   knitr::kable() |>
#'   kableExtra::kable_classic(full_width = FALSE)
#' }
#'
#' @export
validator <- function(project_root = "../", strict = FALSE) {
  # --- Configuration ---
  project_root <- fs::path_abs(project_root)
  all_paths <- fs::dir_ls(
    project_root,
    recurse = TRUE,
    type = "any",
    fail = FALSE,
    all = TRUE
  )

  # --- Helpers ---
  mk_test <- function(test, passed, details = "") {
    tibble::tibble(
      test = test,
      status = ifelse(passed, "PASS", "FAIL"),
      details = details
    )
  }

  mk_skip <- function(test, details) {
    tibble::tibble(test = test, status = "SKIP", details = details)
  }

  exists_ci <- function(relpath) {
    parts <- fs::path_split(relpath)[[1]]
    cur <- project_root
    for (p in parts) {
      cand <- fs::dir_ls(
        cur,
        type = "any",
        recurse = FALSE,
        fail = FALSE,
        all = TRUE
      )
      hit <- cand[tolower(fs::path_file(cand)) == tolower(p)]
      if (length(hit) == 0) {
        return(FALSE)
      }
      cur <- hit[[1]]
    }
    TRUE
  }

  list_ext <- function(ext) {
    ix <- tolower(fs::path_ext(all_paths)) == tolower(ext)
    all_paths[ix & fs::is_file(all_paths)]
  }

  starts_with_any <- function(paths, allowed_dirs_rel) {
    allowed_abs <- fs::path_abs(fs::path(project_root, allowed_dirs_rel))
    purrr::map_lgl(paths, function(p) {
      pd <- fs::path_dir(p)
      any(startsWith(pd, allowed_abs))
    })
  }

  results <- tibble::tibble(
    test = character(),
    status = character(),
    details = character()
  )

  # --- 1) Required directories (must exist) ---
  required_dirs <- c(
    "code",
    "data",
    "data/raw",
    "data/processed",
    "data/outputs",
    "data/outputs/plots",
    "data/outputs/fitted_models",
    "data/outputs/results",
    "methods",
    "reports",
    #"reports/preprint",
    #"reports/presentations",
    "preregistration"
  )
  for (d in required_dirs) {
    exists_dir <- fs::dir_exists(fs::path(project_root, d))
    results <- dplyr::bind_rows(
      results,
      mk_test(
        paste0("Directory exists: ", d),
        exists_dir,
        if (!exists_dir) "Create this directory." else ""
      )
    )
  }

  # --- 2) Required files (must exist) ---
  readme_ok <- exists_ci("readme.md")
  license_any_ok <- exists_ci("licence") ||
    exists_ci("license") ||
    exists_ci("LICENSE")

  results <- dplyr::bind_rows(
    results,
    mk_test(
      'File exists: readme.md',
      readme_ok,
      if (!readme_ok) "Add a README (readme.md)." else ""
    ),
    mk_test(
      'File exists: licence/license',
      license_any_ok,
      if (!license_any_ok) "Add a licence file (prefer `LICENSE`)." else ""
    )
  )

  license_paths <- all_paths[
    stringr::str_to_lower(fs::path_file(all_paths)) %in%
      c(
        "licence",
        "license",
        "license.txt",
        "licence.txt",
        "license.md",
        "licence.md",
        "license.rst",
        "licence.rst",
        "license"
      )
  ]
  if (length(license_paths) > 0) {
    canonical <- any(fs::path_file(license_paths) == "LICENSE")
    results <- dplyr::bind_rows(
      results,
      mk_test(
        "License name is canonical (LICENSE)",
        canonical,
        if (!canonical) {
          "Consider renaming to `LICENSE` (all caps, no extension)."
        } else {
          ""
        }
      )
    )
  }

  # --- 3) Filetype-location constraints ---
  constraints <- list(
    qmd = list(
      must_be_in = c("code", "tools"),
      must_exist_in_each = FALSE,
      forbidden_elsewhere = TRUE
    ),
    Rmd = list(
      must_be_in = c("code", "tools"),
      must_exist_in_each = FALSE,
      forbidden_elsewhere = TRUE
    ),
    R = list(
      must_be_in = c("code", "tools"),
      must_exist_in_each = FALSE,
      forbidden_elsewhere = TRUE
    ),
    #html  = list(must_be_in = c("code", "methods"), must_exist_in_each = FALSE, forbidden_elsewhere = TRUE),
    csv = list(
      must_be_in = c("data"),
      must_exist_in_each = FALSE,
      forbidden_elsewhere = TRUE
    ),
    xlsx = list(
      must_be_in = c("data"),
      must_exist_in_each = FALSE,
      forbidden_elsewhere = TRUE
    ),
    rds = list(
      must_be_in = c("data"),
      must_exist_in_each = FALSE,
      forbidden_elsewhere = TRUE
    ),
    tsv = list(
      must_be_in = c("data"),
      must_exist_in_each = FALSE,
      forbidden_elsewhere = TRUE
    ),
    dta = list(
      must_be_in = c("data"),
      must_exist_in_each = FALSE,
      forbidden_elsewhere = TRUE
    ),
    pdf = list(
      must_be_in = c(
        "data/outputs",
        "data/raw",
        "methods",
        "reports",
        "preregistration"
      ),
      must_exist_in_each = FALSE,
      forbidden_elsewhere = TRUE
    ),
    feather = list(
      must_be_in = c("data"),
      must_exist_in_each = FALSE,
      forbidden_elsewhere = TRUE
    ),
    sav = list(
      must_be_in = c("data"),
      must_exist_in_each = FALSE,
      forbidden_elsewhere = TRUE
    ),
    png = list(
      must_be_in = c("data/outputs/plots", "data/raw"),
      must_exist_in_each = FALSE,
      forbidden_elsewhere = TRUE
    ),
    docx = list(
      must_be_in = c(
        "reports",
        "methods",
        "preregistration",
        "data/outputs/results",
        "data/raw"
      ),
      must_exist_in_each = FALSE,
      forbidden_elsewhere = TRUE
    )
  )

  # HTML restriction: only under code/ or methods/
  html_files <- list_ext("html")
  if (length(html_files)) {
    allowed_dirs <- c("code", "methods")
    html_ok <- all(starts_with_any(html_files, allowed_dirs))
    offenders <- character(0)
    if (!html_ok) {
      offenders <- fs::path_rel(
        html_files[!starts_with_any(html_files, allowed_dirs)],
        start = project_root
      )
    }
    results <- dplyr::bind_rows(
      results,
      mk_test(
        "All .html files are under code/ or methods/",
        html_ok,
        if (!html_ok) {
          paste0(
            ".html files must be under code/ (if output of processing or analysis code) or methods/ (if part of the method delivery). Offenders: ",
            paste(offenders, collapse = "; ")
          )
        } else {
          ""
        }
      )
    )
  }

  check_constraint <- function(rule, ext) {
    files <- list_ext(ext)
    rel <- fs::path_rel(files, start = project_root)

    # 3a) Required presence (if requested)
    if (isTRUE(rule$must_exist_in_each) && length(rule$must_be_in) > 0) {
      for (dir_req in rule$must_be_in) {
        under_dir <- files[starts_with_any(files, dir_req)]
        results <<- dplyr::bind_rows(
          results,
          mk_test(
            paste0("At least one .", ext, " in ", dir_req),
            length(under_dir) > 0,
            if (length(under_dir) == 0) {
              paste0("Add a .", ext, " file under ", dir_req, ".")
            } else {
              ""
            }
          )
        )
      }
    }

    # 3b) Forbidden elsewhere
    if (isTRUE(rule$forbidden_elsewhere)) {
      if (length(files)) {
        allowed_ok <- if (length(rule$must_be_in) == 0) {
          length(files) == 0
        } else {
          all(starts_with_any(files, rule$must_be_in))
        }
        offenders <- character(0)
        if (!allowed_ok) {
          allowed_mask <- starts_with_any(files, rule$must_be_in)
          offenders <- fs::path_rel(files[!allowed_mask], start = project_root)
        }
        results <<- dplyr::bind_rows(
          results,
          mk_test(
            paste0(
              "All .",
              ext,
              if (length(rule$must_be_in)) {
                paste0(
                  " files reside in ",
                  paste(rule$must_be_in, collapse = ", ")
                )
              } else {
                " files are absent"
              }
            ),
            allowed_ok,
            if (!allowed_ok) {
              paste0("Move/remove: ", paste(offenders, collapse = "; "))
            } else {
              ""
            }
          )
        )
      } else if (isFALSE(rule$must_exist_in_each)) {
        results <<- dplyr::bind_rows(
          results,
          mk_test(
            paste0("No .", ext, " files present (as expected)"),
            TRUE,
            ""
          )
        )
      }
    }
  }

  purrr::iwalk(constraints, check_constraint)

  # --- 4) Optional hygiene checks ---
  enable_hygiene <- TRUE

  if (enable_hygiene) {
    # No data-like files under code/ at all
    data_exts <- c(
      "csv",
      "xlsx",
      "tsv",
      "sav",
      "dta",
      "parquet",
      "feather",
      "rds"
    )
    under_code <- all_paths[
      startsWith(fs::path_dir(all_paths), fs::path(project_root, "code")) &
        fs::is_file(all_paths)
    ]
    offenders <- under_code[tolower(fs::path_ext(under_code)) %in% data_exts]
    results <- dplyr::bind_rows(
      results,
      mk_test(
        "No data files stored under code/",
        length(offenders) == 0,
        if (length(offenders)) {
          paste0(
            "Move: ",
            paste(
              fs::path_rel(offenders, start = project_root),
              collapse = "; "
            )
          )
        } else {
          ""
        }
      )
    )

    # .gitignore presence
    has_gitignore <- exists_ci(".gitignore")
    results <- dplyr::bind_rows(
      results,
      mk_test(
        "Has .gitignore",
        has_gitignore,
        if (!has_gitignore) {
          "Add a .gitignore (ignore .Rhistory, .RData, .Rproj.user, cache, large tmp files)."
        } else {
          ""
        }
      )
    )

    # Filenames: discourage spaces
    bad_names <- fs::path_file(all_paths[
      fs::is_file(all_paths) &
        stringr::str_detect(fs::path_file(all_paths), "\\s")
    ])
    results <- dplyr::bind_rows(
      results,
      mk_test(
        "No spaces in filenames",
        length(bad_names) == 0,
        if (length(bad_names)) {
          paste0("Rename: ", paste(bad_names, collapse = "; "))
        } else {
          ""
        }
      )
    )
  }

  # --- 5) Reproducibility checks ---
  rel <- function(p) as.character(fs::path_rel(p, start = project_root))

  # git helpers: all commands run from project_root and fail quietly
  git <- function(...) {
    out <- tryCatch(
      suppressWarnings(system2(
        "git",
        c("-C", shQuote(project_root), ...),
        stdout = TRUE,
        stderr = FALSE
      )),
      error = function(e) character(0)
    )
    if (!is.null(attr(out, "status"))) character(0) else out
  }
  in_git <- nzchar(Sys.which("git")) &&
    identical(git("rev-parse", "--is-inside-work-tree"), "true")
  git_prefix <- if (in_git) git("rev-parse", "--show-prefix") else character(0)
  git_prefix <- if (length(git_prefix)) git_prefix else ""
  # `git status --porcelain` paths are relative to the repository root
  strip_prefix <- function(paths) {
    ifelse(
      startsWith(paths, git_prefix),
      substring(paths, nchar(git_prefix) + 1),
      paths
    )
  }

  # Raw data unchanged since first committed
  raw_test <- "Raw data unchanged since first committed (git)"
  if (!exists_ci("data/raw")) {
    results <- dplyr::bind_rows(
      results,
      mk_skip(raw_test, "No data/raw/ directory.")
    )
  } else if (!in_git) {
    results <- dplyr::bind_rows(
      results,
      mk_skip(
        raw_test,
        "Not a git repository (or git is not installed), so the history of data/raw/ cannot be checked."
      )
    )
  } else {
    # files modified (M) or deleted (D) in any commit after they were added
    changed_committed <- git(
      "log",
      "--diff-filter=MD",
      "--name-only",
      "--format=",
      "--relative",
      "--",
      "data/raw"
    )
    # uncommitted modifications or deletions of tracked files
    porcelain <- git("status", "--porcelain", "--", "data/raw")
    xy <- substr(porcelain, 1, 2)
    changed_uncommitted <- strip_prefix(
      substring(porcelain[grepl("[MD]", xy)], 4)
    )
    changed <- unique(c(changed_committed, changed_uncommitted))
    changed <- sort(changed[nzchar(changed) & basename(changed) != ".gitkeep"])
    results <- dplyr::bind_rows(
      results,
      mk_test(
        raw_test,
        length(changed) == 0,
        if (length(changed)) {
          paste0(
            "Modified or deleted since first committed: ",
            paste(changed, collapse = "; "),
            ". Raw data should be read-only: make changes in code instead, and restore the originals from git. ",
            "If the change was to remove private data, this is expected, but the private data are still in the git history ",
            "(and on GitHub, if pushed) and must be purged separately, e.g., with git filter-repo or BFG Repo-Cleaner."
          )
        } else {
          ""
        }
      )
    )
  }

  # Rendered .html files are newer than their .qmd
  html_test <- "Rendered .html files are up to date with their .qmd"
  qmds <- list_ext("qmd")
  htmls <- fs::path_ext_set(qmds, "html")
  has_html <- fs::file_exists(htmls)
  qmds <- qmds[has_html]
  htmls <- htmls[has_html]
  if (length(qmds) == 0) {
    results <- dplyr::bind_rows(
      results,
      mk_skip(html_test, "No rendered .html files found next to .qmd files.")
    )
  } else {
    # last commit time for committed, unmodified files; otherwise modification
    # time (which, after a fresh clone, only reflects the checkout)
    file_time <- function(f) {
      if (in_git && length(git("status", "--porcelain", "--", shQuote(f))) == 0) {
        ct <- git("log", "-1", "--format=%ct", "--", shQuote(f))
        if (length(ct) == 1 && nzchar(ct)) {
          return(as.numeric(ct))
        }
      }
      as.numeric(fs::file_info(f)$modification_time)
    }
    stale <- qmds[purrr::map2_lgl(qmds, htmls, function(q, h) {
      file_time(q) > file_time(h) + 1
    })]
    results <- dplyr::bind_rows(
      results,
      mk_test(
        html_test,
        length(stale) == 0,
        if (length(stale)) {
          paste0(
            "Changed since last rendered: ",
            paste(rel(stale), collapse = "; "),
            ". Re-render them (see the README's Reproducibility section)."
          )
        } else {
          ""
        }
      )
    )
  }

  # README customised
  readme_test <- "README has been customised (no template placeholders)"
  readme_file <- all_paths[
    fs::path_dir(all_paths) == project_root &
      tolower(fs::path_file(all_paths)) == "readme.md"
  ]
  if (length(readme_file) == 0) {
    results <- dplyr::bind_rows(
      results,
      mk_skip(readme_test, "No README.md found.")
    )
  } else {
    readme_lines <- readLines(readme_file[1], warn = FALSE)
    placeholders <- c(
      "# Project Title",
      "Add aims, data sources, and reproduction steps.",
      "Authors (Year). Title. URL."
    )
    found <- placeholders[purrr::map_lgl(placeholders, function(ph) {
      any(trimws(readme_lines) == ph)
    })]
    results <- dplyr::bind_rows(
      results,
      mk_test(
        readme_test,
        length(found) == 0,
        if (length(found)) {
          paste0(
            "Replace the template text in README.md: ",
            paste(paste0("'", found, "'"), collapse = "; ")
          )
        } else {
          ""
        }
      )
    )
  }

  # No setwd() or absolute paths in code
  code_files <- c(list_ext("R"), list_ext("qmd"), list_ext("Rmd"))
  code_files <- code_files[
    !grepl("(^|/)(\\.[^/]+|renv)/", rel(code_files))
  ]
  code_lines <- purrr::map_dfr(code_files, function(f) {
    lines <- readLines(f, warn = FALSE)
    keep <- rep(TRUE, length(lines))
    if (tolower(fs::path_ext(f)) %in% c("qmd", "rmd")) {
      # only lines inside R code chunks
      in_chunk <- FALSE
      for (i in seq_along(lines)) {
        if (!in_chunk && grepl("^\\s*```+\\s*\\{r", lines[i])) {
          in_chunk <- TRUE
          keep[i] <- FALSE
        } else if (in_chunk && grepl("^\\s*```+\\s*$", lines[i])) {
          in_chunk <- FALSE
          keep[i] <- FALSE
        } else {
          keep[i] <- in_chunk
        }
      }
    }
    # ignore comment lines (including #| chunk options)
    keep <- keep & !grepl("^\\s*#", lines)
    tibble::tibble(
      where = paste0(rel(f), ":", seq_along(lines))[keep],
      code = lines[keep]
    )
  })
  if (nrow(code_lines) == 0) {
    code_lines <- tibble::tibble(where = character(), code = character())
  }
  setwd_hits <- code_lines$where[grepl("\\bsetwd\\s*\\(", code_lines$code)]
  abs_pattern <- paste0(
    "[\"'](",
    "~[/\\\\]", # home directory
    "|/(Users|home|Volumes|mnt|media|opt|srv|tmp|var|private)/", # Unix
    "|[A-Za-z]:[/\\\\]", # Windows drive
    ")"
  )
  abs_hits <- code_lines$where[grepl(abs_pattern, code_lines$code)]
  results <- dplyr::bind_rows(
    results,
    mk_test(
      "No setwd() calls in code",
      length(setwd_hits) == 0,
      if (length(setwd_hits)) {
        paste0(
          "Remove setwd() from: ",
          paste(setwd_hits, collapse = "; "),
          ". Each .qmd runs from its own folder, so use relative paths (e.g., ../data/raw/)."
        )
      } else {
        ""
      }
    ),
    mk_test(
      "No absolute file paths in code",
      length(abs_hits) == 0,
      if (length(abs_hits)) {
        paste0(
          "Replace absolute paths with relative ones (e.g., ../data/raw/) in: ",
          paste(abs_hits, collapse = "; ")
        )
      } else {
        ""
      }
    )
  )

  # Codebooks for processed data
  codebook_test <- "Every processed data file has a codebook"
  codebook_done_test <- "Codebooks are completed (no 'TO BE COMPLETED MANUALLY')"
  data_exts_processed <- c(
    "csv",
    "tsv",
    "xlsx",
    "sav",
    "dta",
    "parquet",
    "feather",
    "rds"
  )
  processed_dir <- fs::path(project_root, "data", "processed")
  processed_files <- if (fs::dir_exists(processed_dir)) {
    fs::dir_ls(processed_dir, type = "file", recurse = TRUE)
  } else {
    character(0)
  }
  processed_files <- processed_files[
    tolower(fs::path_ext(processed_files)) %in% data_exts_processed
  ]
  is_codebook <- grepl("codebook", tolower(fs::path_file(processed_files)))
  codebook_files <- processed_files[is_codebook]
  data_files <- processed_files[!is_codebook]

  if (length(data_files) == 0) {
    results <- dplyr::bind_rows(
      results,
      mk_skip(codebook_test, "No data files in data/processed/."),
      mk_skip(codebook_done_test, "No data files in data/processed/.")
    )
  } else {
    # "x_data.csv" is documented by "x_codebook.<any extension>" in the same
    # folder; files not ending in "_data" by "<name>_codebook.<ext>"
    codebook_keys <- tolower(fs::path(
      fs::path_dir(codebook_files),
      fs::path_ext_remove(fs::path_file(codebook_files))
    ))
    expected_keys <- tolower(fs::path(
      fs::path_dir(data_files),
      paste0(
        sub("_data$", "", fs::path_ext_remove(fs::path_file(data_files))),
        "_codebook"
      )
    ))
    undocumented <- data_files[!expected_keys %in% codebook_keys]
    results <- dplyr::bind_rows(
      results,
      mk_test(
        codebook_test,
        length(undocumented) == 0,
        if (length(undocumented)) {
          paste0(
            "Add a codebook for: ",
            paste(rel(undocumented), collapse = "; "),
            ". Name it after the data file, ending in '_codebook' (e.g., study_1_data.csv -> study_1_codebook.csv). ",
            "code/processing.qmd contains a chunk that creates one."
          )
        } else {
          ""
        }
      )
    )

    # manual columns still containing the template placeholder (.csv/.tsv only)
    text_codebooks <- codebook_files[
      tolower(fs::path_ext(codebook_files)) %in% c("csv", "tsv")
    ]
    if (length(text_codebooks) == 0) {
      results <- dplyr::bind_rows(
        results,
        mk_skip(
          codebook_done_test,
          "No .csv or .tsv codebooks to check (other formats are not read)."
        )
      )
    } else {
      incomplete <- purrr::map_chr(text_codebooks, function(f) {
        sep <- if (tolower(fs::path_ext(f)) == "tsv") "\t" else ","
        cb <- tryCatch(
          utils::read.csv(f, sep = sep, colClasses = "character"),
          error = function(e) NULL
        )
        if (is.null(cb)) {
          return(paste0(rel(f), " (could not be read)"))
        }
        todo <- vapply(
          cb,
          function(col) sum(trimws(col) == "TO BE COMPLETED MANUALLY", na.rm = TRUE),
          integer(1)
        )
        todo <- todo[todo > 0]
        if (length(todo) == 0) {
          return(NA_character_)
        }
        paste0(
          rel(f),
          " (",
          paste(paste0(names(todo), ": ", todo), collapse = ", "),
          ")"
        )
      })
      incomplete <- incomplete[!is.na(incomplete)]
      results <- dplyr::bind_rows(
        results,
        mk_test(
          codebook_done_test,
          length(incomplete) == 0,
          if (length(incomplete)) {
            paste0(
              "Replace 'TO BE COMPLETED MANUALLY' (write 'none' where a column does not apply) in: ",
              paste(incomplete, collapse = "; ")
            )
          } else {
            ""
          }
        )
      )
    }
  }

  # --- 6) Present results ---
  results <- results |>
    dplyr::mutate(
      status = factor(status, levels = c("FAIL", "SKIP", "PASS"))
    ) |>
    dplyr::arrange(status, test)

  n_fail <- sum(results$status == "FAIL")
  n_pass <- sum(results$status == "PASS")

  res <- results |>
    dplyr::mutate(details = ifelse(details == "", "-", details)) |>
    dplyr::rename(
      `Test` = test,
      `Status` = status,
      `Details / Guidance` = details
    )

  attr(res, "project_root") <- as.character(project_root)
  class(res) <- c("psychdsish_validation", class(res))

  if (strict && n_fail > 0) {
    failed <- res[res$Status == "FAIL", ]
    cli::cli_abort(c(
      "{n_fail} of {n_pass + n_fail} psych-DS-ish check{?s} failed in {.path {project_root}}.",
      stats::setNames(
        paste0(failed$Test, ": ", failed$`Details / Guidance`),
        rep("x", nrow(failed))
      )
    ))
  }

  res
}

#' @export
print.psychdsish_validation <- function(x, ...) {
  needed <- c("Test", "Status", "Details / Guidance")
  # fall back to the default tibble print if the structure has been modified
  if (!all(needed %in% names(x))) {
    return(NextMethod())
  }
  smry <- summary(x)
  line <- function(...) cat(..., "\n", sep = "")

  line(cli::rule(left = "psych-DS-ish validation"))
  if (!is.null(smry$project_root)) {
    line(cli::col_grey(smry$project_root))
  }
  for (i in seq_len(nrow(x))) {
    if (x$Status[i] == "FAIL") {
      line(cli::col_red(paste(cli::symbol$cross, "FAIL")), " ", x$Test[i])
      details <- x$`Details / Guidance`[i]
      if (!is.na(details) && details != "-") {
        line("       ", cli::col_grey(details))
      }
    } else if (x$Status[i] == "SKIP") {
      line(cli::col_yellow(paste(cli::symbol$circle, "SKIP")), " ", x$Test[i])
      line("       ", cli::col_grey(x$`Details / Guidance`[i]))
    } else {
      line(cli::col_green(paste(cli::symbol$tick, "PASS")), " ", x$Test[i])
    }
  }
  line(cli::rule())
  skipped <- if (smry$n_skip > 0) sprintf(" (%d skipped)", smry$n_skip) else ""
  if (smry$passed) {
    line(cli::col_green(sprintf("All %d checks passed.", smry$n_pass)), skipped)
  } else {
    line(
      cli::col_red(sprintf(
        "%d of %d checks failed.",
        smry$n_fail,
        smry$n_pass + smry$n_fail
      )),
      skipped
    )
  }
  invisible(x)
}

#' @export
summary.psychdsish_validation <- function(object, ...) {
  n_fail <- sum(object$Status == "FAIL")
  list(
    project_root = attr(object, "project_root"),
    n_pass = sum(object$Status == "PASS"),
    n_fail = n_fail,
    n_skip = sum(object$Status == "SKIP"),
    passed = n_fail == 0
  )
}
