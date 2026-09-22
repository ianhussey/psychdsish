# style_all_files() rewrites users' files, so check exactly what it touches.

make_messy_project <- function() {
  root <- file.path(tempdir(), paste0("psychdsish_style_", sample.int(1e6, 1)))
  dir.create(file.path(root, "code"), recursive = TRUE)
  dir.create(file.path(root, "tools"))
  writeLines("x<-c(1,2 ,3)\nif(x[1]>0){y=2}", file.path(root, "code", "script.R"))
  writeLines(
    c(
      "---",
      "title: messy",
      "---",
      "",
      "Prose with  odd   spacing,x<-1 and `a<-b` stays as written.",
      "",
      "```{r}",
      "z<-mean(c(1,2))",
      "```"
    ),
    file.path(root, "code", "report.qmd")
  )
  writeLines("t<-1", file.path(root, "tools", "helper.R"))
  root
}

test_that("style_all_files restyles code files and code chunks only", {
  skip_if_not_installed("styler")
  root <- make_messy_project()
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)

  # dry run: lists files and changes nothing (tools/ is excluded by default)
  before <- readLines(file.path(root, "code", "script.R"))
  listed <- style_all_files(root, dry_run = TRUE)
  expect_setequal(basename(listed), c("script.R", "report.qmd"))
  expect_equal(readLines(file.path(root, "code", "script.R")), before)

  suppressMessages(capture.output(style_all_files(root)))
  script <- readLines(file.path(root, "code", "script.R"))
  expect_true("x <- c(1, 2, 3)" %in% script)
  report <- readLines(file.path(root, "code", "report.qmd"))
  expect_true("z <- mean(c(1, 2))" %in% report)
  expect_true(
    "Prose with  odd   spacing,x<-1 and `a<-b` stays as written." %in% report
  )
  expect_equal(readLines(file.path(root, "tools", "helper.R")), "t<-1")

  # styling is idempotent
  styled <- lapply(listed, readLines)
  suppressMessages(capture.output(style_all_files(root)))
  expect_equal(lapply(listed, readLines), styled)
})

test_that("style_all_files handles no matching files", {
  root <- file.path(tempdir(), paste0("psychdsish_style_empty_", sample.int(1e6, 1)))
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)
  expect_message(res <- style_all_files(root), "No matching files")
  expect_length(res, 0)
})

test_that("print_poorly_styled_code prints parseable code that styler changes", {
  skip_if_not_installed("styler")
  code <- capture.output(print_poorly_styled_code())
  expect_true(any(grepl("mtcars", code, fixed = TRUE)))
  expect_no_error(parse(text = code))
  expect_false(identical(as.character(styler::style_text(code)), code))
})
