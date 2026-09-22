# delete_project_skeleton() with an explicit keep_file, so that the deletion
# itself can be tested (under testthat, the calling file cannot be detected).

new_project <- function(...) {
  root <- file.path(tempdir(), paste0("psychdsish_delete_", sample.int(1e6, 1)))
  create_project_skeleton(project_root = root, quiet = TRUE, ...)
  root
}

all_files <- function(root) {
  sort(list.files(root, recursive = TRUE, all.files = TRUE, include.dirs = TRUE))
}

test_that("a dry run lists what would be deleted and deletes nothing", {
  root <- new_project()
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)
  keep <- file.path(root, "tools", "project_creator.qmd")
  before <- all_files(root)

  # no confirmation needed for a dry run, even with confirm = TRUE
  expect_message(
    res <- delete_project_skeleton(root, keep_file = keep, dry_run = TRUE),
    "Dry run"
  )
  expect_equal(all_files(root), before)
  expect_true(all(c("code", "data", "README.md", ".gitignore") %in% res$paths))
  expect_true("tools/project_validator.qmd" %in% res$paths)
  expect_false(any(c("tools", "tools/project_creator.qmd") %in% res$paths))
  # every file and directory except tools/ and the kept file
  expect_equal(res$n_items, length(before) - 2)
})

test_that("deletion removes everything except the kept file and its folders", {
  root <- new_project(studies = 2, layout = "by_study")
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)
  keep <- file.path(root, "study_2", "code", "processing.qmd")
  kept_text <- readLines(keep)

  msgs <- character(0)
  res <- withCallingHandlers(
    delete_project_skeleton(root, keep_file = keep, confirm = FALSE),
    message = function(m) {
      msgs <<- c(msgs, conditionMessage(m))
      invokeRestart("muffleMessage")
    }
  )
  expect_true(any(grepl("Protected file", msgs)))
  expect_equal(
    all_files(root),
    c("study_2", "study_2/code", "study_2/code/processing.qmd")
  )
  expect_equal(readLines(keep), kept_text)
  expect_true("study_2/code/analysis.qmd" %in% res$paths)

  # nothing left to delete
  expect_message(
    delete_project_skeleton(
      root,
      keep_file = keep,
      confirm = FALSE,
      require_sentinel = FALSE
    ),
    "Nothing to delete"
  )
})

test_that("delete_project_skeleton validates keep_file and requires confirmation", {
  root <- new_project()
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)
  before <- all_files(root)

  expect_error(
    delete_project_skeleton(root, keep_file = file.path(root, "nope.qmd"), confirm = FALSE),
    "keep_file"
  )
  expect_error(
    delete_project_skeleton(root, keep_file = file.path(root, "tools"), confirm = FALSE),
    "keep_file"
  )
  # confirm = TRUE cannot be answered in a non-interactive session
  expect_error(
    delete_project_skeleton(root, keep_file = file.path(root, "README.md")),
    "non-interactive"
  )
  expect_equal(all_files(root), before)
})

test_that("a keep_file outside the project protects nothing inside it", {
  root <- new_project()
  outside <- tempfile(fileext = ".R")
  writeLines("x <- 1", outside)
  on.exit(unlink(c(root, outside), recursive = TRUE, force = TRUE), add = TRUE)

  delete_project_skeleton(root, keep_file = outside, confirm = FALSE) |>
    suppressMessages()
  expect_equal(all_files(root), character(0))
  expect_true(file.exists(outside))
})
