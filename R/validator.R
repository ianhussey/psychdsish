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
#' }
#'
#' @return
#' A tibble with one row per check, containing:
#' \describe{
#'   \item{Test}{Description of the check performed.}
#'   \item{Status}{Either `"PASS"` or `"FAIL"`.}
#'   \item{Details / Guidance}{Additional information, such as instructions
#'         for fixing failures or offending file paths.}
#' }
#'
#' @examples
#' \dontrun{
#' # Run validation on the parent directory
#' results <- validator("../")
#'
#' # Pretty-print results in the console
#' print(results)
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
validator <- function(project_root = "../"){
  # --- Configuration ---
  project_root <- fs::path_abs(project_root)  
  all_paths <- fs::dir_ls(project_root, recurse = TRUE, type = "any", fail = FALSE, all = TRUE)
  
  # --- Helpers ---
  mk_test <- function(test, passed, details = "") {
    tibble::tibble(test = test, status = ifelse(passed, "PASS", "FAIL"), details = details)
  }
  
  exists_ci <- function(relpath) {
    parts <- fs::path_split(relpath)[[1]]
    cur <- project_root
    for (p in parts) {
      cand <- fs::dir_ls(cur, type = "any", recurse = FALSE, fail = FALSE, all = TRUE)
      hit <- cand[tolower(fs::path_file(cand)) == tolower(p)]
      if (length(hit) == 0) return(FALSE)
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
  
  results <- tibble::tibble(test = character(), status = character(), details = character())
  
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
    results <- dplyr::bind_rows(results, mk_test(
      paste0("Directory exists: ", d),
      exists_dir,
      if (!exists_dir) "Create this directory." else ""
    ))
  }
  
  # --- 2) Required files (must exist) ---
  readme_ok <- exists_ci("readme.md")
  license_any_ok <- exists_ci("licence") || exists_ci("license") || exists_ci("LICENSE")
  
  results <- dplyr::bind_rows(
    results,
    mk_test('File exists: readme.md', readme_ok, if (!readme_ok) "Add a README (readme.md)." else ""),
    mk_test('File exists: licence/license', license_any_ok,
            if (!license_any_ok) "Add a licence file (prefer `LICENSE`)." else "")
  )
  
  license_paths <- all_paths[stringr::str_to_lower(fs::path_file(all_paths)) %in% c(
    "licence","license","license.txt","licence.txt","license.md","licence.md","license.rst","licence.rst","license"
  )]
  if (length(license_paths) > 0) {
    canonical <- any(fs::path_file(license_paths) == "LICENSE")
    results <- dplyr::bind_rows(
      results,
      mk_test("License name is canonical (LICENSE)", canonical,
              if (!canonical) "Consider renaming to `LICENSE` (all caps, no extension)." else "")
    )
  }
  
  # --- 3) Filetype-location constraints ---
  constraints <- list(
    qmd  = list(must_be_in = c("code", "tools"), must_exist_in_each = FALSE, forbidden_elsewhere = TRUE),
    Rmd  = list(must_be_in = c("code", "tools"), must_exist_in_each = FALSE, forbidden_elsewhere = TRUE),
    R    = list(must_be_in = c("code", "tools"), must_exist_in_each = FALSE, forbidden_elsewhere = TRUE),
    #html  = list(must_be_in = c("code", "methods"), must_exist_in_each = FALSE, forbidden_elsewhere = TRUE),
    csv  = list(must_be_in = c("data"), must_exist_in_each = FALSE, forbidden_elsewhere = TRUE),
    xlsx = list(must_be_in = c("data"), must_exist_in_each = FALSE, forbidden_elsewhere = TRUE),
    rds  = list(must_be_in = c("data"), must_exist_in_each = FALSE, forbidden_elsewhere = TRUE),
    tsv  = list(must_be_in = c("data"), must_exist_in_each = FALSE, forbidden_elsewhere = TRUE),
    dta  = list(must_be_in = c("data"), must_exist_in_each = FALSE, forbidden_elsewhere = TRUE),
    pdf  = list(must_be_in = c("data/outputs", "data/raw", "methods", "reports",  "preregistration"), must_exist_in_each = FALSE, forbidden_elsewhere = TRUE),
    feather  = list(must_be_in = c("data"), must_exist_in_each = FALSE, forbidden_elsewhere = TRUE),
    sav  = list(must_be_in = c("data"), must_exist_in_each = FALSE, forbidden_elsewhere = TRUE),
    png  = list(must_be_in = c("data/outputs/plots", "data/raw"), must_exist_in_each = FALSE, forbidden_elsewhere = TRUE),
    docx  = list(must_be_in = c("reports", "methods", "preregistration", "data/outputs/results", "data/raw"), must_exist_in_each = FALSE, forbidden_elsewhere = TRUE)
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
        if (!html_ok)
          paste0(".html files must be under code/ (if output of processing or analysis code) or methods/ (if part of the method delivery). Offenders: ",
                 paste(offenders, collapse = "; "))
        else ""
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
        results <<- dplyr::bind_rows(results, mk_test(
          paste0("At least one .", ext, " in ", dir_req),
          length(under_dir) > 0,
          if (length(under_dir) == 0) paste0("Add a .", ext, " file under ", dir_req, ".") else ""
        ))
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
        results <<- dplyr::bind_rows(results, mk_test(
          paste0("All .", ext,
                 if (length(rule$must_be_in)) paste0(" files reside in ", paste(rule$must_be_in, collapse = ", "))
                 else " files are absent"),
          allowed_ok,
          if (!allowed_ok) paste0("Move/remove: ", paste(offenders, collapse = "; ")) else ""
        ))
      } else if (isFALSE(rule$must_exist_in_each)) {
        results <<- dplyr::bind_rows(results, mk_test(
          paste0("No .", ext, " files present (as expected)"),
          TRUE, ""
        ))
      }
    }
  }
  
  purrr::iwalk(constraints, check_constraint)
  
  # --- 4) Optional hygiene checks ---
  enable_hygiene <- TRUE
  
  if (enable_hygiene) {
    # No data-like files under code/ at all
    data_exts <- c("csv","xlsx","tsv","sav","dta","parquet","feather","rds")
    under_code <- all_paths[
      startsWith(fs::path_dir(all_paths), fs::path(project_root, "code")) & fs::is_file(all_paths)
    ]
    offenders <- under_code[tolower(fs::path_ext(under_code)) %in% data_exts]
    results <- dplyr::bind_rows(results, mk_test(
      "No data files stored under code/",
      length(offenders) == 0,
      if (length(offenders)) paste0("Move: ", paste(fs::path_rel(offenders, start = project_root), collapse = "; ")) else ""
    ))
    
    # .gitignore presence
    has_gitignore <- exists_ci(".gitignore")
    results <- dplyr::bind_rows(results, mk_test(
      "Has .gitignore",
      has_gitignore,
      if (!has_gitignore) "Add a .gitignore (ignore .Rhistory, .RData, .Rproj.user, cache, large tmp files)." else ""
    ))
    
    # Filenames: discourage spaces
    bad_names <- fs::path_file(all_paths[fs::is_file(all_paths) & stringr::str_detect(fs::path_file(all_paths), "\\s")])
    results <- dplyr::bind_rows(results, mk_test(
      "No spaces in filenames",
      length(bad_names) == 0,
      if (length(bad_names)) paste0("Rename: ", paste(bad_names, collapse = "; ")) else ""
    ))
  }
  
  # --- 5) Present results ---
  results <- results |>
    dplyr::mutate(status = factor(status, levels = c("PASS","FAIL"))) |>
    dplyr::arrange(dplyr::desc(status), test)
  
  n_fail <- sum(results$status == "FAIL")
  n_pass <- sum(results$status == "PASS")
  
  res <- results |>
    dplyr::mutate(details = ifelse(details == "", "-", details)) |>
    dplyr::rename(`Test` = test, `Status` = status, `Details / Guidance` = details)
  
  return(res)
}