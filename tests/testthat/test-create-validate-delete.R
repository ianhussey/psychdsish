# Round-trip tests entirely inside tempdir(): build a skeleton, check the
# validator's results, and exercise the safety guards of the deleter.

make_skeleton <- function() {
  root <- file.path(
    tempdir(),
    paste0("psychdsish_test_", as.integer(Sys.time()), "_", sample.int(1e6, 1))
  )
  dir.create(root, showWarnings = FALSE, recursive = TRUE)
  create_project_skeleton(project_root = root, overwrite = TRUE, quiet = TRUE)
  root
}

# replace the template README so the placeholder check passes
customise_readme <- function(root) {
  writeLines(
    c("# My study", "", "Aims, data sources, and reproduction steps."),
    file.path(root, "README.md")
  )
}

failed_tests <- function(res) {
  as.character(res$Test[res$Status == "FAIL"])
}

git_run <- function(root, ...) {
  system2("git", c("-C", shQuote(root), ...), stdout = FALSE, stderr = FALSE)
}

git_init <- function(root) {
  git_run(root, "init", "-q")
  git_run(root, "config", "user.email", "test@example.com")
  git_run(root, "config", "user.name", "test")
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

test_that("a fresh skeleton fails only the README placeholder check", {
  root <- make_skeleton()
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)

  res <- validator(project_root = root)

  expect_s3_class(res, "data.frame")
  expect_true(all(c("Test", "Status") %in% names(res)))
  expect_equal(
    failed_tests(res),
    "README has been customised (no template placeholders)"
  )
  # not a git repository, nothing rendered, and no processed data yet
  expect_equal(summary(res)$n_skip, 4L)

  customise_readme(root)
  expect_true(summary(validator(project_root = root))$passed)
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
  customise_readme(root)

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

test_that("RStudio project template binding creates a valid skeleton", {
  root <- file.path(tempdir(), paste0("psychdsish_template_", sample.int(1e6, 1)))
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)

  dcf <- system.file(
    "rstudio", "templates", "project", "psychdsish.dcf",
    package = "psychdsish"
  )
  expect_true(nzchar(dcf))
  expect_equal(unname(read.dcf(dcf)[1, "Binding"]), "create_psychdsish_project")

  create_psychdsish_project(root, quarto_yml = FALSE)
  expect_true(file.exists(file.path(root, paste0(basename(root), ".Rproj"))))
  expect_false(file.exists(file.path(root, "_quarto.yml")))
  expect_equal(
    failed_tests(validator(project_root = root)),
    "README has been customised (no template placeholders)"
  )
})

test_that("validator flags setwd() and absolute paths only in R code", {
  root <- make_skeleton()
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)
  customise_readme(root)

  qmd <- file.path(root, "code", "analysis.qmd")
  # prose, comments, and URLs are not flagged
  cat(
    "\nSee `setwd(\"~/x\")` in prose.\n",
    "```{r}",
    "# setwd(\"~/in/a/comment\")",
    "url <- \"https://example.com/data\"",
    "```\n",
    file = qmd, append = TRUE, sep = "\n"
  )
  expect_true(summary(validator(project_root = root))$passed)

  cat(
    "```{r}",
    "setwd(\"~/project\")",
    "d <- read.csv(\"C:/Users/me/data.csv\")",
    "```\n",
    file = qmd, append = TRUE, sep = "\n"
  )
  writeLines("x <- read.csv('/Users/me/x.csv')", file.path(root, "code", "helpers.R"))
  res <- validator(project_root = root)
  expect_setequal(
    failed_tests(res),
    c("No setwd() calls in code", "No absolute file paths in code")
  )
  details <- res$`Details / Guidance`[res$Test == "No absolute file paths in code"]
  expect_match(details, "code/helpers.R:1", fixed = TRUE)
  expect_match(details, "code/analysis.qmd:", fixed = TRUE)
})

test_that("validator flags stale .html files", {
  root <- make_skeleton()
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)
  customise_readme(root)

  qmd <- file.path(root, "code", "analysis.qmd")
  html <- file.path(root, "code", "analysis.html")
  writeLines("<html></html>", html)
  Sys.setFileTime(qmd, Sys.time() - 60)
  expect_true(summary(validator(project_root = root))$passed)

  Sys.setFileTime(html, Sys.time() - 120)
  expect_equal(
    failed_tests(validator(project_root = root)),
    "Rendered .html files are up to date with their .qmd"
  )
})

test_that("validator flags raw data changed since first committed", {
  skip_if(!nzchar(Sys.which("git")), "git is not installed")
  root <- make_skeleton()
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)
  customise_readme(root)
  git_init(root)

  writeLines("a,b", file.path(root, "data", "raw", "raw.csv"))
  git_run(root, "add", "-A")
  git_run(root, "commit", "-q", "-m", "initial")
  res <- validator(project_root = root)
  expect_true(summary(res)$passed)
  expect_equal(
    as.character(res$Status[res$Test == "Raw data unchanged since first committed (git)"]),
    "PASS"
  )

  # adding new raw files is fine
  writeLines("c,d", file.path(root, "data", "raw", "raw_2.csv"))
  expect_true(summary(validator(project_root = root))$passed)

  # modifying a committed raw file is not (even before committing)
  writeLines("a,b,c", file.path(root, "data", "raw", "raw.csv"))
  res <- validator(project_root = root)
  expect_equal(failed_tests(res), "Raw data unchanged since first committed (git)")
  details <- res$`Details / Guidance`[res$Status == "FAIL"]
  expect_match(details, "data/raw/raw.csv", fixed = TRUE)
  expect_match(details, "git history", fixed = TRUE)

  # still flagged once committed
  git_run(root, "commit", "-q", "-a", "-m", "modify raw")
  expect_equal(
    failed_tests(validator(project_root = root)),
    "Raw data unchanged since first committed (git)"
  )
})

test_that("validator addin prints results and builds the Viewer table", {
  root <- make_skeleton()
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)

  addins <- system.file("rstudio", "addins.dcf", package = "psychdsish")
  expect_true(nzchar(addins))
  expect_equal(unname(read.dcf(addins)[1, "Binding"]), "validator_addin")

  expect_output(
    res <- validator_addin(project_root = root, viewer = FALSE),
    "1 of [0-9]+ checks failed"
  )
  expect_s3_class(res, "psychdsish_validation")

  html <- paste(validation_html(res), collapse = "\n")
  expect_match(html, "<tr class=\"fail\">", fixed = TRUE)
  expect_match(html, "<tr class=\"skip\">", fixed = TRUE)
  expect_match(html, "README has been customised", fixed = TRUE)
  expect_match(html, "&#39;|'# Project Title'")
})

test_that("validator checks processed data files have completed codebooks", {
  root <- make_skeleton()
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)
  customise_readme(root)
  processed <- file.path(root, "data", "processed")

  write.csv(data.frame(x = 1:3), file.path(processed, "study_1_data.csv"))
  saveRDS(data.frame(x = 1:3), file.path(processed, "study_2.rds"))
  res <- validator(project_root = root)
  expect_equal(failed_tests(res), "Every processed data file has a codebook")
  details <- res$`Details / Guidance`[res$Status == "FAIL"]
  expect_match(details, "study_1_data.csv", fixed = TRUE)
  expect_match(details, "study_2.rds", fixed = TRUE)

  # codebooks matched by name, with any extension; placeholders flagged
  write.csv(
    data.frame(
      variable = "x",
      description = "TO BE COMPLETED MANUALLY",
      units = "none",
      coding = "TO BE COMPLETED MANUALLY"
    ),
    file.path(processed, "study_1_codebook.csv"),
    row.names = FALSE
  )
  file.create(file.path(processed, "study_2_codebook.xlsx"))
  res <- validator(project_root = root)
  expect_equal(
    failed_tests(res),
    "Codebooks are completed (no 'TO BE COMPLETED MANUALLY')"
  )
  expect_match(
    res$`Details / Guidance`[res$Status == "FAIL"],
    "description: 1, coding: 1",
    fixed = TRUE
  )

  write.csv(
    data.frame(variable = "x", description = "Score", units = "none", coding = "none"),
    file.path(processed, "study_1_codebook.csv"),
    row.names = FALSE
  )
  expect_true(summary(validator(project_root = root))$passed)
})

test_that("processing.qmd codebook chunk creates and updates a codebook", {
  root <- make_skeleton()
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)

  # extract the codebook chunk from the template and run it in code/
  lines <- readLines(file.path(root, "code", "processing.qmd"))
  start <- grep("#| label: codebook", lines, fixed = TRUE)
  end <- start + which(lines[(start + 1):length(lines)] == "```")[1]
  chunk <- lines[(start + 1):(end - 1)]
  old_wd <- setwd(file.path(root, "code"))
  on.exit(setwd(old_wd), add = TRUE)
  run_chunk <- function(data_processed) {
    eval(parse(text = chunk), envir = environment())
  }

  run_chunk(data.frame(id = 1:3, age = c(20, NA, 30)))
  path <- file.path(root, "data", "processed", "processed_codebook.csv")
  cb <- read.csv(path, colClasses = "character")
  expect_equal(cb$variable, c("id", "age"))
  expect_equal(cb$values, c("1 to 3", "20 to 30"))
  expect_true(all(cb$description == "TO BE COMPLETED MANUALLY"))

  # manual entries are kept; new variables added; removed ones dropped
  cb$description[cb$variable == "age"] <- "Age in years"
  write.csv(cb, path, row.names = FALSE)
  expect_message(
    run_chunk(data.frame(age = c(20, NA, 30), group = c("a", "b", "a"))),
    "id"
  )
  cb <- read.csv(path, colClasses = "character")
  expect_equal(cb$variable, c("age", "group"))
  expect_equal(cb$description, c("Age in years", "TO BE COMPLETED MANUALLY"))
  expect_equal(cb$values[cb$variable == "group"], "a; b")
})
