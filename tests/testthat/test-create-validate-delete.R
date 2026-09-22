# Round-trip tests entirely inside tempdir(): build a skeleton, confirm it
# validates cleanly, and exercise the safety guards of the deleter.

make_skeleton <- function() {
  root <- file.path(
    tempdir(),
    paste0("psychdsish_test_", as.integer(Sys.time()), "_", sample.int(1e6, 1))
  )
  dir.create(root, showWarnings = FALSE, recursive = TRUE)
  create_project_skeleton(project_root = root, overwrite = TRUE, quiet = TRUE)
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
  expect_true(file.exists(file.path(root, "_quarto.yml")))
  expect_true(file.exists(file.path(root, "CITATION.cff")))
  expect_true(file.exists(file.path(root, paste0(basename(root), ".Rproj"))))
  for (f in c("project_creator.qmd", "project_validator.qmd", "style_all_files.qmd")) {
    expect_true(file.exists(file.path(root, "tools", f)), info = f)
  }
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

test_that("create_project_skeleton respects rproj = FALSE and existing .Rproj files", {
  root <- file.path(tempdir(), paste0("psychdsish_rproj_", sample.int(1e6, 1)))
  dir.create(root, showWarnings = FALSE, recursive = TRUE)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)

  create_project_skeleton(project_root = root, rproj = FALSE, quiet = TRUE)
  expect_length(list.files(root, pattern = "\\.Rproj$"), 0)

  file.create(file.path(root, "existing.Rproj"))
  create_project_skeleton(project_root = root, quiet = TRUE)
  expect_equal(list.files(root, pattern = "\\.Rproj$"), "existing.Rproj")
})

test_that("create_project_skeleton reports created, skipped, and overwritten files", {
  root <- file.path(tempdir(), paste0("psychdsish_status_", sample.int(1e6, 1)))
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)

  expect_message(
    first <- create_project_skeleton(project_root = root),
    "skipped 0 existing files"
  )
  expect_true(all(first$status == "created"))
  expect_true(all(c(".gitignore", ".gitattributes", "LICENSE") %in% first$path))
  expect_true(file.exists(file.path(root, "data", "raw", ".gitkeep")))

  expect_message(
    second <- create_project_skeleton(project_root = root),
    "use overwrite = TRUE"
  )
  expect_true(all(second$status[second$type == "file"] == "skipped"))
  expect_true(all(second$status[second$type == "dir"] == "exists"))

  third <- create_project_skeleton(
    project_root = root,
    overwrite = TRUE,
    quiet = TRUE
  )
  expect_true(all(third$status[third$type == "file"] == "overwritten"))
  expect_silent(create_project_skeleton(project_root = root, quiet = TRUE))
})

test_that("create_project_skeleton respects quarto_yml = FALSE", {
  root <- file.path(tempdir(), paste0("psychdsish_noquarto_", sample.int(1e6, 1)))
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)

  create_project_skeleton(project_root = root, quarto_yml = FALSE, quiet = TRUE)
  expect_false(file.exists(file.path(root, "_quarto.yml")))

  readme <- readLines(file.path(root, "README.md"), warn = FALSE)
  expect_false(any(grepl("_quarto.yml", readme, fixed = TRUE)))
})

test_that("validator print, summary, and strict mode work", {
  root <- make_skeleton()
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)

  res <- validator(project_root = root)
  expect_s3_class(res, "psychdsish_validation")
  expect_true(summary(res)$passed)
  expect_equal(summary(res)$n_fail, 0L)
  expect_output(print(res), "All [0-9]+ checks passed")
  expect_no_error(validator(project_root = root, strict = TRUE))

  # introduce a failure: a filename containing a space
  file.create(file.path(root, "methods", "bad name.docx"))
  res <- validator(project_root = root)
  expect_false(summary(res)$passed)
  expect_equal(summary(res)$n_fail, 1L)
  expect_output(print(res), "1 of [0-9]+ checks failed")
  expect_output(print(res), "bad name.docx")
  expect_error(
    validator(project_root = root, strict = TRUE),
    "No spaces in filenames"
  )
})
