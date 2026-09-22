## Submission

This is a new submission.

## Test environments

- local macOS (aarch64), R 4.5.2 (release)
- GitHub Actions R-CMD-check: macOS (release), Windows (release),
  Ubuntu (devel, release, oldrel-1)
- TODO before submitting: win-builder R-devel and R-release
  (`devtools::check_win_devel()`, `devtools::check_win_release()`) and
  mac-builder (`devtools::check_mac_release()`). Record the results here,
  and remove this item.

## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new submission.

(Local `R CMD check --as-cran --run-donttest` gives 0 errors, 0 warnings,
and 0 notes; the "New submission" NOTE appears on the CRAN incoming checks.)

## Notes for the reviewer

* All examples run, and write only inside `tempdir()`, removing what they
  create. The example for `check_unused_dependencies()` is wrapped in
  `\donttest{}` and runs only if the Quarto command line tools are
  installed, because the function renders documents with Quarto.
* Functions that write or delete files (`create_project_skeleton()`,
  `delete_project_skeleton()`, `style_all_files()`) have no default path, so
  they only write where the user says. `delete_project_skeleton()` also
  refuses to run without a project marker file, asks for typed confirmation
  unless `confirm = FALSE`, and always protects the calling file (or
  `keep_file`).
* Quarto and Git are optional system requirements (see
  `SystemRequirements`). Tests that need Quarto are skipped on CRAN and when
  Quarto is not installed; tests that need Git are skipped when it is not
  installed.
* Tests create, validate, and remove projects entirely within `tempdir()`
  and clean up after themselves.
* The package uses domain terms and names that a spell-checker does not
  recognise but which are spelled correctly and used intentionally, e.g.,
  'psych-DS', 'psych-DS-ish', 'tidyverse', 'Quarto', 'codebook', and the
  author name 'Struhl'. These are recorded in `inst/WORDLIST`.
* The 'psych-DS' data standard is referenced with an angle-bracketed URL
  (`<https://psych-ds.github.io/>`) in the Description, as it has no
  associated DOI or ISBN.
* The package has no compiled code.
