## Submission

This is a new submission.

## Test environments

- local macOS (aarch64), R 4.5.2 (release)
- win-builder (x86_64-w64-mingw32), R-devel, via `devtools::check_win_devel()`
- GitHub Actions R-CMD-check (macOS, Windows, Ubuntu; release and devel)

## R CMD check results

0 errors | 0 warnings | 1 note.

The one NOTE is the standard "New submission" flag; there are no other notes.

## Notes for the reviewer

* The package uses domain terms and names that a spell-checker does not
  recognise but which are spelled correctly and used intentionally: for
  example 'psych-DS', 'psych-DS-ish', 'tidyverse', 'Quarto', 'codebook',
  and the author name 'Struhl'. These are recorded in `inst/WORDLIST`, so
  `spelling::spell_check_package()` and the incoming spell-check pass with
  no spelling NOTE.
* The 'psych-DS' data standard is referenced with an angle-bracketed URL
  (`<https://psych-ds.github.io/>`) in the Description, as it has no
  associated DOI or ISBN.
* All examples that write to disk do so only inside `tempdir()`, and the
  destructive `delete_project_skeleton()` example is wrapped in
  `\dontrun{}`. The package has no compiled code.
* Tests create, validate, and remove a skeleton entirely within
  `tempdir()` and clean up after themselves.
