# Project Title

## Overview
Add aims, data sources, and reproduction steps.

## Structure
```
code/                 # analysis and processing scripts (.qmd/.Rmd) and their rendered .html
  study_1/
  study_2/
data/
  raw/                # raw data and codebooks/data dictionaries (should be read-only, except for removal of private data)
    study_1/
    study_2/
  processed/          # cleaned datasets and codebooks/data dictionaries
    study_1/
    study_2/
  outputs/            # outputs of the processing and analyses scripts
    plots/            # plots and figures, .png/.pdf/etc.
      study_1/
      study_2/
    fitted_models/    # fitted model objects, eg from brms, lme4, lavaan, etc.
      study_1/
      study_2/
    results/          # tables and matrices, eg for descriptive statistics, formatted statistical results, correlation tables
      study_1/
      study_2/
methods/              # measures, implementations (qualtrics, lab.js, psychopy files, etc.), .docx files with items, etc.
  study_1/
  study_2/
preregistration/      # preregistration documents
  study_1/
  study_2/
reports/              # thesis, manuscript, preprints, slides, etc. (shared by all studies)
tools/                # utility scripts, e.g., project validator and code styler
CITATION.cff          # citation metadata: gives the 'Cite this repository' button on GitHub
LICENSE               # suggested: CC BY 4.0
README.md             # this file
*.Rproj               # RStudio project file: open this to work on the project in RStudio
_quarto.yml           # lists which .qmd files to render, in order, when rendering the whole project
```

This project was created with [psychdsish](https://github.com/ianhussey/psychdsish), an R package that sets up a standardised project structure loosely based on the [psych-DS](https://psych-ds.github.io/) standard.

To check that the project still follows this standard, use any one of these options:

- **RStudio:** with the project open, click *Addins > Validate psych-DS-ish project* in the toolbar. The results appear in the console and as a table in the Viewer pane.
- **Report:** render `tools/project_validator.qmd`.
- **R console:** from the project root, run:

```r
# install.packages("remotes")
# remotes::install_github("ianhussey/psychdsish")
psychdsish::validator(".")
```

This lists each check as PASS, FAIL, or SKIP, with guidance on how to fix any failures. Use `psychdsish::validator(".", strict = TRUE)` to throw an error if any check fails, e.g., to fail a GitHub Actions job.

## Reproducibility

### Workflow
Each study has its own folders and follows the same workflow. Paths below are for study 1; the other studies' folders end in `study_2/`, `study_3/`, etc.
- Raw data lives in `data/raw/study_1/` and is never modified by code.
- `code/study_1/processing.qmd` reads the raw data and writes cleaned datasets to `data/processed/study_1/`. Rendering it also creates `code/study_1/processing.html`.
- `code/study_1/analysis.qmd` reads the processed data and writes plots to `data/outputs/plots/study_1/`, fitted model objects to `data/outputs/fitted_models/study_1/`, and tables to `data/outputs/results/study_1/`. Rendering it also creates `code/study_1/analysis.html`.

### Adding a study
Increase `studies` in `tools/project_creator.qmd` and render it: this creates the new study's folders and .qmd files, and leaves existing files untouched. Then add the new study's processing and analysis files to the `render:` list in `_quarto.yml`.

### Reproduce all results
`_quarto.yml` lists the files to render and the order to render them in (processing before analysis). Render the whole project using any one of these options:

- **RStudio:** open the `.Rproj` file, then click *Build > Render Project* in the Build pane (top right).
- **R console:** with the working directory set to the project root (automatic when the `.Rproj` file is open), run `quarto::quarto_render()`.
- **Terminal:** from the project root, run `quarto render`.

Rendering stops at the first error, so analyses never run on stale or partially processed data. Each file runs with its own folder as the working directory, so paths in the code are relative to the file's folder (e.g., from `code/study_1/`, raw data is in `../../data/raw/study_1/`).

Clicking *Render* in an individual `.qmd` file renders only that file. Use it while developing, but render the whole project before sharing results.

### Adding new files
If you add another processing or analysis file (e.g., `code/study_1/processing_part_2.qmd`), add it to the `render:` list in `_quarto.yml` in the position it should run, otherwise it will not be rendered with the rest of the project.

## Codebooks
Every data file in `data/processed/study_*/` should have a codebook (data dictionary) that describes each of its variables, named after the data file (e.g., `study_1_data.csv` -> `study_1_codebook.csv`).

`code/study_1/processing.qmd` contains a chunk that creates the codebook from the processed data. It fills in each variable's type, number of missing values, and range or values, and marks the columns that only you can complete as "TO BE COMPLETED MANUALLY":

- `description`: what the variable is, e.g., the item wording, or how a score was calculated.
- `units`: e.g., years or milliseconds. Write "none" if it does not apply.
- `coding`: what the values mean, e.g., "1 = strongly disagree to 7 = strongly agree", reverse-scored items, or missing-value codes such as -99.

Open the .csv (e.g., in Excel), replace every placeholder, and save it as .csv. Re-rendering `code/study_1/processing.qmd` keeps your entries, adds new variables, and removes variables that are no longer in the data. `psychdsish::validator()` reports data files without a codebook, and codebooks that still contain placeholders.

**Using AI assistants:** an AI assistant can help draft descriptions, but only from information it can actually see. For example, ask it to read `code/study_1/processing.qmd` and describe how each variable was created. It cannot know what your items said or what your codes mean, and will guess convincingly if asked. Check every entry against your study materials (e.g., in `methods/study_1/`), and do not keep any description you cannot verify.

## License
CC BY 4.0 (see `LICENSE`).

## Suggested citation
Authors (Year). Title. URL.

`CITATION.cff` holds this citation in a machine-readable format. On GitHub, it adds a *Cite this repository* button to the repository page (right-hand sidebar) that gives APA and BibTeX citations, and Zenodo reads it when archiving a release.

To customise it, open `CITATION.cff` and replace the placeholders:

- `title`: the project title.
- `authors`: one `- family-names:` / `given-names:` block per author, in author order. Add each author's ORCID or delete the `orcid:` line.
- `date-released`: the date of the version people should cite.
- `repository-code`: the repository URL.
- `doi`: uncomment and fill in once you have one (e.g., from a Zenodo release).

Keep it consistent with the citation above. See https://citation-file-format.github.io for all available fields.

After editing, check that the file is still valid. GitHub silently drops the *Cite this repository* button if it is not. From the project root, in the R console, run:

```r
# install.packages("cffr")
cffr::cff_validate("CITATION.cff")
```

This reports whether the file is valid and, if not, which fields are wrong.
