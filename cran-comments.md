## Submission

This is a new submission.

## Test environments

- local macOS (aarch64), R 4.5.2 (release)
- GitHub Actions R-CMD-check: macOS (release), Windows (release),
  Ubuntu (devel, release, oldrel-1)
- win-builder, Windows Server 2022 x64, R 4.6.1 (release): 1 NOTE
- win-builder, Windows Server 2022 x64, R-devel (2026-09-21 r90579): 1 NOTE
- mac-builder, macOS 26.6 (aarch64), R 4.6.1 (release): OK, no notes

## R CMD check results

0 errors | 0 warnings | 1 note

The one NOTE is from the CRAN incoming checks on win-builder:

* New submission.

* Possibly misspelled words in DESCRIPTION: Addins, Hussey, README,
  codebook, validator. These are spelled correctly: 'Hussey' is the
  maintainer's surname, 'Addins' is the name of the 'RStudio' menu,
  'README', 'codebook', and 'validator' are used in their ordinary sense.

Local `R CMD check --as-cran --run-donttest` (including the PDF manual) and
the mac-builder check both give 0 errors, 0 warnings, and 0 notes.

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
