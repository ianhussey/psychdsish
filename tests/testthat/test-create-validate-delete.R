# Round-trip tests entirely inside tempdir(): build a skeleton, confirm it
# validates cleanly, and exercise the safety guards of the deleter.

make_skeleton <- function() {
  root <- file.path(
    tempdir(),
    paste0("psychdsish_test_", as.integer(Sys.time()), "_", sample.int(1e6, 1))
  )
  dir.create(root, showWarnings = FALSE, recursive = TRUE)
  create_project_skeleton(project_root = root, overwrite = TRUE)
  root
}

test_that("create_project_skeleton builds the expected directories and files", {
  root <- make_skeleton()
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)

  expected_dirs <- c(
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
    "preregistration"
  )
  for (d in expected_dirs) {
    expect_true(dir.exists(file.path(root, d)), info = d)
  }

  expect_true(file.exists(file.path(root, ".gitignore")))
  expect_true(file.exists(file.path(root, "LICENSE")))
  expect_true(any(tolower(list.files(root)) == "readme.md"))
})

test_that("validator reports no failures for a freshly created skeleton", {
  root <- make_skeleton()
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)

  res <- validator(project_root = root)

  expect_s3_class(res, "data.frame")
  expect_true(all(c("Test", "Status") %in% names(res)))
  expect_equal(sum(as.character(res$Status) == "FAIL"), 0L)
})

test_that("delete_project_skeleton enforces the sentinel safety check", {
  bare <- file.path(tempdir(), paste0("psychdsish_bare_", sample.int(1e6, 1)))
  dir.create(bare, showWarnings = FALSE, recursive = TRUE)
  on.exit(unlink(bare, recursive = TRUE, force = TRUE), add = TRUE)

  # No sentinel file present -> refuse before doing anything.
  expect_error(
    delete_project_skeleton(project_root = bare, confirm = FALSE),
    "sentinel"
  )
  expect_true(dir.exists(bare))
})

test_that("delete_project_skeleton aborts when the current file cannot be detected", {
  root <- make_skeleton()
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)

  # The deleter refuses to run unless it can identify its calling file, so that
  # it never removes the running script. Under test_check() no current file is
  # detectable, so it must abort and leave the skeleton untouched.
  expect_error(
    delete_project_skeleton(project_root = root, confirm = FALSE),
    "Could not detect the current file"
  )
  expect_true(dir.exists(root))
})
