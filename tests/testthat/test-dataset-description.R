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
