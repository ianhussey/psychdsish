# Snapshot the generated text files for each layout, so that any change to a
# template shows up as a reviewable diff (testthat::snapshot_review()).
# CITATION.cff is not snapshotted because it contains today's date.

snapshot_project <- function(layout, studies) {
  # a fixed folder name, because the .Rproj file is named after it
  parent <- file.path(tempdir(), paste0("psychdsish_snap_", sample.int(1e6, 1)))
  root <- file.path(parent, "my_project")
  create_project_skeleton(
    project_root = root,
    studies = studies,
    layout = layout,
    quiet = TRUE
  )
  list(root = root, parent = parent)
}

for (case in list(
  list(name = "single", layout = "by_study", studies = 1),
  list(name = "by_study", layout = "by_study", studies = 2),
  list(name = "by_type", layout = "by_type", studies = 2)
)) {
  local({
    case <- case
    test_that(paste0("generated files are unchanged: ", case$name), {
      p <- snapshot_project(case$layout, case$studies)
      on.exit(unlink(p$parent, recursive = TRUE, force = TRUE), add = TRUE)
      code_dir <- if (case$name == "by_study") {
        "study_1/code"
      } else if (case$name == "by_type") {
        "code/study_1"
      } else {
        "code"
      }
      files <- c(
        "README.md",
        "_quarto.yml",
        ".gitignore",
        "tools/project_creator.qmd",
        file.path(code_dir, "processing.qmd"),
        file.path(code_dir, "analysis.qmd")
      )
      for (f in files) {
        expect_snapshot_file(
          file.path(p$root, f),
          name = paste0(case$name, "-", gsub("/", "-", sub("^\\.", "dot-", f))),
          compare = testthat::compare_file_text
        )
      }
    })
  })
}
