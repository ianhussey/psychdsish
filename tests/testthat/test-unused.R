# check_unused_objects() and check_unused_dependencies()

make_code_project <- function() {
  root <- file.path(tempdir(), paste0("psychdsish_unused_", sample.int(1e6, 1)))
  dir.create(file.path(root, "code"), recursive = TRUE)
  dir.create(file.path(root, "tools"))
  writeLines(
    c(
      "a <- 1",
      "b <- a + 1",
      "unused_in_r <- 3",
      "f <- function(x) x * 2",
      "print(f(b))"
    ),
    file.path(root, "code", "script.R")
  )
  writeLines(
    c(
      "---",
      "title: test",
      "---",
      "",
      "```{r}",
      "df1 <- data.frame(x = 1:3)",
      "df2 <- transform(df1, y = x * 2)",
      "orphan <- 5",
      "print(df2)",
      "```"
    ),
    file.path(root, "code", "report.qmd")
  )
  # tools/ is excluded by default
  writeLines("tool_orphan <- 1", file.path(root, "tools", "helper.R"))
  root
}

test_that("check_unused_objects finds objects that are never used", {
  root <- make_code_project()
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)

  res <- check_unused_objects(root)
  expect_s3_class(res, "data.frame")
  expect_setequal(res$unused_objects, c("unused_in_r", "orphan"))
  expect_equal(
    basename(res$file[res$unused_objects == "orphan"]),
    "report.qmd"
  )

  single <- check_unused_objects_single_file(file.path(root, "code", "script.R"))
  expect_equal(single$unused_objects, "unused_in_r")

  # including tools/ finds its orphan too
  res_all <- check_unused_objects(root, exclude_dirs = character(0))
  expect_true("tool_orphan" %in% res_all$unused_objects)
})

test_that("check_unused_objects handles clean code, no files, and parse errors", {
  root <- file.path(tempdir(), paste0("psychdsish_unused_clean_", sample.int(1e6, 1)))
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)

  expect_equal(nrow(check_unused_objects(root)), 0)

  writeLines(c("a <- 1", "print(a)"), file.path(root, "clean.R"))
  expect_equal(nrow(check_unused_objects(root)), 0)

  # a file that does not parse is skipped rather than stopping the check
  writeLines("x <- (", file.path(root, "broken.R"))
  expect_no_error(res <- check_unused_objects(root))
  expect_equal(nrow(res), 0)

  expect_error(check_unused_objects(file.path(root, "missing")), "not found")
})

test_that("check_unused_dependencies flags attached packages that are never called", {
  skip_on_cran()
  skip_if(is.null(quarto::quarto_path()), "Quarto is not installed")
  skip_if_not_installed("dplyr")
  skip_if_not_installed("stringr")

  root <- file.path(tempdir(), paste0("psychdsish_deps_", sample.int(1e6, 1)))
  dir.create(file.path(root, "code"), recursive = TRUE)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)
  writeLines(
    c(
      "---",
      "title: test",
      "---",
      "",
      "```{r}",
      "library(dplyr)",
      "library(stringr)",
      "library(\"tibble\")",
      "df <- data.frame(x = 1:3) |> mutate(y = x * 2)",
      "t <- tibble::tibble(a = 1)",
      "```"
    ),
    file.path(root, "code", "report.qmd")
  )

  res <- suppressMessages(check_unused_dependencies(root))
  # dplyr is called unqualified, tibble with ::, stringr not at all
  expect_equal(res$possibly_unused_packages, "stringr")
  expect_equal(res$file, "code/report.qmd")
})

test_that("check_unused_dependencies handles folders without documents", {
  root <- file.path(tempdir(), paste0("psychdsish_deps_empty_", sample.int(1e6, 1)))
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)
  expect_equal(nrow(check_unused_dependencies(root)), 0)
  expect_error(check_unused_dependencies(file.path(root, "missing")), "not found")
})
