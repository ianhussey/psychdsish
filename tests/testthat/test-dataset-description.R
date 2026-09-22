# write_dataset_description(): psych-DS metadata built from the codebooks

make_codebook <- function(dir, stem, variables) {
  write.csv(
    variables,
    file.path(dir, paste0(stem, "_codebook.csv")),
    row.names = FALSE
  )
}

test_that("write_dataset_description builds variableMeasured from codebooks", {
  root <- file.path(tempdir(), paste0("psychdsish_json_", sample.int(1e6, 1)))
  create_project_skeleton(root, quiet = TRUE)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)

  make_codebook(
    file.path(root, "data", "processed"),
    "stage-processed",
    data.frame(
      variable = c("id", "rt", "condition"),
      description = c("Participant identifier", "Response time", "Condition"),
      units = c("none", "milliseconds", "none"),
      coding = c("none", "none", "1 = control, 2 = treatment")
    )
  )

  res <- write_dataset_description(
    root,
    name = "My study",
    description = "A demo dataset",
    quiet = TRUE
  )
  expect_equal(res$variables, c("id", "rt", "condition"))

  json <- jsonlite::read_json(file.path(root, "dataset_description.json"))
  expect_equal(json$`@context`, "https://schema.org/")
  expect_equal(json$`@type`, "Dataset")
  expect_equal(json$name, "My study")
  expect_equal(json$description, "A demo dataset")
  expect_length(json$variableMeasured, 3)
  expect_equal(json$variableMeasured[[2]]$`@type`, "PropertyValue")
  expect_equal(json$variableMeasured[[2]]$name, "rt")
  expect_equal(json$variableMeasured[[2]]$unitText, "milliseconds")
  # the coding is folded into the description, as psych-DS has no field for it
  expect_equal(
    json$variableMeasured[[3]]$description,
    "Condition. Coding: 1 = control, 2 = treatment"
  )
})

test_that("write_dataset_description updates a file without losing other fields", {
  root <- file.path(tempdir(), paste0("psychdsish_json_upd_", sample.int(1e6, 1)))
  create_project_skeleton(root, quiet = TRUE)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)
  processed <- file.path(root, "data", "processed")

  make_codebook(
    processed,
    "stage-processed",
    data.frame(variable = "id", description = "Identifier", units = "none", coding = "none")
  )
  write_dataset_description(root, name = "Study", description = "Demo", quiet = TRUE)

  # a field psychdsish does not write
  path <- file.path(root, "dataset_description.json")
  json <- jsonlite::read_json(path)
  json$license <- "CC-BY-4.0"
  jsonlite::write_json(json, path, pretty = TRUE, auto_unbox = TRUE)

  # a second codebook, and no name/description given
  make_codebook(
    processed,
    "stage-raw",
    data.frame(variable = "age", description = "Age", units = "years", coding = "none")
  )
  res <- write_dataset_description(root, quiet = TRUE)

  json <- jsonlite::read_json(path)
  expect_equal(json$license, "CC-BY-4.0")
  expect_equal(json$name, "Study")
  expect_equal(json$description, "Demo")
  expect_setequal(res$variables, c("id", "age"))
})

test_that("write_dataset_description reports missing codebooks and descriptions", {
  root <- file.path(tempdir(), paste0("psychdsish_json_todo_", sample.int(1e6, 1)))
  create_project_skeleton(root, quiet = TRUE)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)

  expect_warning(write_dataset_description(root, quiet = TRUE), "No codebooks")

  make_codebook(
    file.path(root, "data", "processed"),
    "stage-processed",
    data.frame(
      variable = "id",
      description = "TO BE COMPLETED MANUALLY",
      units = "TO BE COMPLETED MANUALLY",
      coding = "TO BE COMPLETED MANUALLY"
    )
  )
  expect_message(write_dataset_description(root), "still need a description")

  # defaults to the project folder's name
  res <- write_dataset_description(root, quiet = TRUE)
  expect_equal(res$name, basename(root))

  expect_error(write_dataset_description(file.path(root, "missing")), "not found")
})

test_that("write_dataset_description finds codebooks in multi-study projects", {
  root <- file.path(tempdir(), paste0("psychdsish_json_multi_", sample.int(1e6, 1)))
  create_project_skeleton(root, studies = 2, layout = "by_type", quiet = TRUE)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)

  for (s in 1:2) {
    make_codebook(
      file.path(root, "data", "processed", paste0("study_", s)),
      paste0("study-", s, "_stage-processed"),
      data.frame(
        variable = c("id", paste0("score_", s)),
        description = c("Identifier", "Score"),
        units = "none",
        coding = "none"
      )
    )
  }

  res <- write_dataset_description(root, quiet = TRUE)
  # "id" appears in both codebooks and is written once
  expect_equal(res$variables, c("id", "score_1", "score_2"))
})

# validator()'s psych-DS checks, which report WARN rather than FAIL

test_that("validator warns about psych-DS file names and csv-only codebooks", {
  root <- file.path(tempdir(), paste0("psychdsish_warn_", sample.int(1e6, 1)))
  create_project_skeleton(root, quiet = TRUE)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)
  writeLines(c("# My study", "", "Aims."), file.path(root, "README.md"))
  processed <- file.path(root, "data", "processed")

  # a psych-DS name, documented by a .csv codebook only
  write.csv(
    data.frame(id = 1:2, rt = c(250, 300)),
    file.path(processed, "stage-processed_data.csv"),
    row.names = FALSE
  )
  make_codebook(
    processed,
    "stage-processed",
    data.frame(
      variable = c("id", "rt"),
      description = c("Identifier", "Response time"),
      units = c("none", "ms"),
      coding = "none"
    )
  )

  res <- validator(project_root = root)
  expect_length(failed_tests(res), 0)
  expect_true(
    "Data files are documented in dataset_description.json (psych-DS)" %in%
      warned_tests(res)
  )
  # a warning is not a failure
  expect_true(summary(res)$passed)
  expect_no_error(validator(project_root = root, strict = TRUE))

  # writing the metadata clears the warning
  write_dataset_description(root, name = "My study", description = "Demo", quiet = TRUE)
  expect_length(warned_tests(validator(project_root = root)), 0)

  # a name that does not follow the psych-DS convention
  write.csv(
    data.frame(a = 1),
    file.path(processed, "results_final.csv"),
    row.names = FALSE
  )
  res <- validator(project_root = root)
  expect_true(
    "Processed data file names follow the psych-DS convention" %in% warned_tests(res)
  )
  expect_match(
    res$`Details / Guidance`[res$Test == "Processed data file names follow the psych-DS convention"],
    "results_final.csv",
    fixed = TRUE
  )
})

test_that("validator treats raw data leniently and flags ambiguous codebooks", {
  root <- file.path(tempdir(), paste0("psychdsish_warn2_", sample.int(1e6, 1)))
  create_project_skeleton(root, quiet = TRUE)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)
  writeLines(c("# My study", "", "Aims."), file.path(root, "README.md"))
  raw <- file.path(root, "data", "raw")
  processed <- file.path(root, "data", "processed")

  # raw data straight from a platform: warned about, never failed
  write.csv(data.frame(a = 1), file.path(raw, "Qualtrics_export.csv"), row.names = FALSE)
  res <- validator(project_root = root)
  expect_length(failed_tests(res), 0)
  expect_setequal(
    warned_tests(res),
    c(
      "Every raw data file has a codebook",
      "Raw data file names follow the psych-DS convention"
    )
  )

  # a codebook that does not match any data file
  write.csv(data.frame(a = 1), file.path(processed, "stage-processed_data.csv"), row.names = FALSE)
  make_codebook(processed, "stage-processed", data.frame(variable = "a", description = "A", units = "none", coding = "none"))
  write.csv(data.frame(variable = "a"), file.path(processed, "codebook.csv"), row.names = FALSE)
  writeLines("{}", file.path(processed, "notes.json"))
  res <- validator(project_root = root)
  details <- res$`Details / Guidance`[
    res$Test == "Codebooks are named after the data file they describe"
  ]
  expect_match(details, "codebook.csv", fixed = TRUE)
  expect_match(details, "notes.json", fixed = TRUE)
  expect_match(details, "_codebook", fixed = TRUE)

  # a psych-DS sidecar is not ambiguous, and documents its data file
  file.remove(file.path(processed, "codebook.csv"), file.path(processed, "notes.json"))
  writeLines('{"variableMeasured": ["a"]}', file.path(processed, "stage-processed_data.json"))
  res <- validator(project_root = root)
  expect_false(
    "Codebooks are named after the data file they describe" %in% warned_tests(res)
  )
  expect_false(
    "Data files are documented in dataset_description.json (psych-DS)" %in% warned_tests(res)
  )
})
