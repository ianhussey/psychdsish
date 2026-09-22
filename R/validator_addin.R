#' Validate the current project from the RStudio Addins menu
#'
#' Binding for the RStudio addin *Addins > Validate psych-DS-ish project*. It
#' runs [validator()] on the open RStudio project (or, if no project is open,
#' the working directory), prints the results to the console, and shows them
#' as a colour-coded table in the Viewer pane.
#'
#' @param project_root Character scalar or `NULL`. Path to the project root.
#'   If `NULL` (default), the open RStudio project is used, or the working
#'   directory if no project is open.
#' @param viewer Logical. If `TRUE` (default), show the results table in the
#'   RStudio Viewer pane when running inside RStudio.
#'
#' @return (Invisibly) the `psychdsish_validation` results from [validator()].
#'
#' @keywords internal
#' @export
validator_addin <- function(project_root = NULL, viewer = TRUE) {
  in_rstudio <- requireNamespace("rstudioapi", quietly = TRUE) &&
    rstudioapi::isAvailable()

  if (is.null(project_root)) {
    active <- if (in_rstudio) rstudioapi::getActiveProject() else NULL
    project_root <- if (is.null(active)) getwd() else active
  }

  res <- validator(project_root = project_root)
  print(res)

  if (viewer && in_rstudio) {
    html_file <- tempfile("psychdsish_validation_", fileext = ".html")
    writeLines(validation_html(res), html_file)
    rstudioapi::viewer(html_file)
  }

  invisible(res)
}

# Build a standalone HTML page showing validator() results as a table
validation_html <- function(res) {
  esc <- function(x) {
    x <- gsub("&", "&amp;", x, fixed = TRUE)
    x <- gsub("<", "&lt;", x, fixed = TRUE)
    gsub(">", "&gt;", x, fixed = TRUE)
  }
  smry <- summary(res)
  status <- as.character(res$Status)
  details <- res$`Details / Guidance`
  details[is.na(details) | details == "-"] <- ""

  rows <- paste0(
    "<tr class=\"", tolower(status), "\">",
    "<td class=\"status\">", status, "</td>",
    "<td>", esc(res$Test), "</td>",
    "<td>", esc(details), "</td>",
    "</tr>",
    collapse = "\n"
  )
  headline <- if (smry$passed) {
    sprintf("All %d checks passed.", smry$n_pass)
  } else {
    sprintf("%d of %d checks failed.", smry$n_fail, smry$n_pass + smry$n_fail)
  }
  if (smry$n_skip > 0) {
    headline <- paste0(headline, sprintf(" (%d skipped)", smry$n_skip))
  }

  c(
    "<!DOCTYPE html>",
    "<html><head><meta charset=\"utf-8\">",
    "<title>psych-DS-ish validation</title>",
    "<style>",
    "body { font-family: -apple-system, 'Segoe UI', Roboto, sans-serif; margin: 16px; font-size: 14px; color: #222; background: #fff; }",
    "h1 { font-size: 18px; margin: 0 0 4px; }",
    ".root { color: #666; margin: 0 0 8px; word-break: break-all; }",
    ".headline { font-weight: 600; margin: 0 0 12px; }",
    ".headline.passed { color: #1a7f37; } .headline.failed { color: #cf222e; }",
    "table { border-collapse: collapse; width: 100%; }",
    "th, td { text-align: left; vertical-align: top; padding: 6px 8px; border-bottom: 1px solid #ddd; }",
    "td.status { font-weight: 600; white-space: nowrap; }",
    "tr.fail td.status { color: #cf222e; } tr.fail { background: #fff5f5; }",
    "tr.skip td.status { color: #9a6700; } tr.skip { background: #fffbeb; }",
    "tr.pass td.status { color: #1a7f37; }",
    "@media (prefers-color-scheme: dark) {",
    "  body { color: #ddd; background: #1e1e1e; } .root { color: #999; }",
    "  th, td { border-bottom-color: #444; }",
    "  tr.fail { background: #3a1d1d; } tr.skip { background: #3a321a; }",
    "  tr.fail td.status, .headline.failed { color: #ff7b72; }",
    "  tr.skip td.status { color: #e3b341; }",
    "  tr.pass td.status, .headline.passed { color: #56d364; }",
    "}",
    "</style></head><body>",
    "<h1>psych-DS-ish validation</h1>",
    paste0("<p class=\"root\">", esc(smry$project_root), "</p>"),
    paste0(
      "<p class=\"headline ", if (smry$passed) "passed" else "failed", "\">",
      headline, "</p>"
    ),
    "<table><thead><tr><th>Status</th><th>Test</th><th>Details / Guidance</th></tr></thead>",
    "<tbody>", rows, "</tbody></table>",
    "</body></html>"
  )
}
