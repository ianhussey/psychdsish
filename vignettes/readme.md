# Project Title

## Overview
Add aims, data sources, and reproduction steps.

## Structure
```
code/                 # analysis and processing scripts (.qmd/.Rmd)
  models/             # fitted model objects (.rds)
  plots/              # generated figures (.png)
reports/              # thesis, manuscript, preprints, slides, etc.
data/
  raw/                # raw data and codebooks/data dictionaries (should be read-only, except for removal of private data)
  processed/          # cleaned datasets and codebooks/data dictionaries
  outputs/            # outputs of the processing and analyses scripts
    plots/            # plots and figures, .png/.pdf/etc.
    fitted_models/    # fitted model objects, eg from brms, lme4, lavaan, etc.
    results/          # tables and matrices, eg for descriptive statistics, formatted statistical results, correlation tables
methods/              # measures, implementations (qualtrics, lab.js, psychopy files, etc.), .docx files with items, etc.
preregistration/      # preregistration documents
LICENSE               # suggested: CC BY 4.0
readme.md             # this file
```

## Reproducibility
- Place raw data in `data/raw/`.
- Write processing in `code/processing.qmd` and analyses in `code/analyses.qmd`.
- Re-run data processing with `code/processing.qmd`. This will create `code/processing.html` and files in `data/processed/` and `data/results/`.
- Re-run analyses with `code/analysis.Rmd`. This will create `code/processing.html`, plots in `code/plots/` and fitted model objects in `code/models/`.

## License
CC BY 4.0 (see `LICENSE`).
## Suggested citation
Authors (Year). Title. URL.