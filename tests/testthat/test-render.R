# End-to-end: create a skeleton, add data to each processing.qmd, render the
# whole project with Quarto, and validate the result.

render_project <- function(layout, studies) {
  root <- file.path(tempdir(), paste0("psychdsish_render_", sample.int(1e6, 1)))
  create_project_skeleton(
    project_root = root,
    studies = studies,
    layout = layout,
    quiet = TRUE
  )
  lay <- if (studies > 1) layout else "single"
  ids <- psychdsish:::study_names(studies)

  # processing.qmd creates data_processed and saves it; analysis.qmd reads it
  for (s in ids) {
    code_dir <- psychdsish:::layout_path("code", s, lay)
    processed <- psychdsish:::rel_path(
      code_dir,
      psychdsish:::layout_path("data/processed", s, lay)
    )
    # psych-DS file names, as the template uses
    stem <- paste0(
      if (studies > 1) paste0("study-", sub("^study_", "", s), "_"),
      "stage-processed"
    )
    processing <- file.path(root, code_dir, "processing.qmd")
    lines <- readLines(processing)
    at <- which(lines == "# Codebook")
    writeLines(
      append(
        lines,
        c(
          "```{r}",
          "data_processed <- data.frame(id = 1:3, score = c(2.5, 3, 4))",
          sprintf(
            "write.csv(data_processed, '%s/%s_data.csv', row.names = FALSE)",
            processed,
            stem
          ),
          "```",
          ""
        ),
        after = at - 1
      ),
      processing
    )
    analysis <- file.path(root, code_dir, "analysis.qmd")
    cat(
      "\n```{r}",
      sprintf("d <- read.csv('%s/%s_data.csv')", processed, stem),
      "stopifnot(nrow(d) == 3)",
      "```\n",
      file = analysis,
      sep = "\n",
      append = TRUE
    )
  }

  quarto::quarto_render(root, quiet = TRUE)
  list(root = root, lay = lay, ids = ids)
}

for (case in list(
  list(layout = "by_study", studies = 1),
  list(layout = "by_study", studies = 2),
  list(layout = "by_type", studies = 2)
)) {
  local({
    case <- case
    test_that(
      sprintf("a %s project with %d studies renders and validates", case$layout, case$studies),
      {
        skip_on_cran()
        skip_if(is.null(quarto::quarto_path()), "Quarto is not installed")

        p <- render_project(case$layout, case$studies)
        on.exit(unlink(p$root, recursive = TRUE, force = TRUE), add = TRUE)

        for (s in p$ids) {
          code_dir <- file.path(p$root, psychdsish:::layout_path("code", s, p$lay))
          expect_true(file.exists(file.path(code_dir, "processing.html")))
          expect_true(file.exists(file.path(code_dir, "analysis.html")))
          codebook <- file.path(
            p$root,
            psychdsish:::layout_path("data/processed", s, p$lay),
            paste0(
              if (length(p$ids) > 1) paste0("study-", sub("^study_", "", s), "_"),
              "stage-processed_codebook.csv"
            )
          )
          expect_equal(read.csv(codebook)$variable, c("id", "score"))
        }

        res <- validator(project_root = p$root)
        expect_equal(summary(res)$layout, p$lay)
        # rendered .html files are up to date, and codebooks are matched
        expect_equal(
          as.character(res$Status[res$Test == "Rendered .html files are up to date with their .qmd"]),
          "PASS"
        )
        expect_setequal(
          as.character(res$Test[res$Status == "FAIL"]),
          c(
            "README has been customised (no template placeholders)",
            "Codebooks are completed (no 'TO BE COMPLETED MANUALLY')"
          )
        )
      }
    )
  })
}
