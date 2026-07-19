# psychdsish 0.1.3

* Prepared the package for CRAN submission: added a `testthat` test suite,
  a GitHub Actions `R-CMD-check` workflow, spelling checks (`inst/WORDLIST`),
  and `cran-comments.md`.
* Added a "Get started" vignette walking through
  `create_project_skeleton()`, `validator()`, and `delete_project_skeleton()`.
* Cleaned up `DESCRIPTION` (URLs, `BugReports`, selective imports) and split
  the licence into the CRAN `LICENSE` stub plus `LICENSE.md`.
* `R CMD check --as-cran` now passes with no errors or warnings.

# psychdsish 0.1.2

* Development versions: project skeleton creation (`create_project_skeleton()`),
  validation (`validator()`), deletion (`delete_project_skeleton()`), and code
  hygiene helpers.
