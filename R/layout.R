# Internal helpers for single- and multi-study project layouts, shared by
# create_project_skeleton() and validator().
#
# Folders are described by their single-study ("logical") path, e.g.
# "data/raw", and mapped to their actual path for a layout:
#   "single":   data/raw
#   "by_study": study_1/data/raw
#   "by_type":  data/raw/study_1
# `reports/` and `tools/` are always shared at the project root. In the
# by-type layout, only the per-study leaf folders get a study subfolder;
# parents such as data/ and data/outputs/ stay at the root.

# leaf folders that exist once per study
layout_study_dirs <- c(
  "code",
  "data/raw",
  "data/processed",
  "data/outputs/plots",
  "data/outputs/fitted_models",
  "data/outputs/results",
  "methods",
  "preregistration"
)

# folders shared by all studies, at the project root
layout_shared_dirs <- c("reports", "tools")

layout_labels <- c(
  single = "single study",
  by_study = "multi-study, by study (study_1/code/)",
  by_type = "multi-study, by type (code/study_1/)"
)

study_names <- function(n) {
  paste0("study_", seq_len(n))
}

# is a logical folder mapped to a per-study folder in this layout?
layout_is_per_study <- function(logical, layout) {
  switch(
    layout,
    single = FALSE,
    by_study = !logical %in% layout_shared_dirs,
    by_type = logical %in% layout_study_dirs
  )
}

# actual path of a logical folder for one study
layout_path <- function(logical, study, layout) {
  if (!layout_is_per_study(logical, layout)) {
    return(logical)
  }
  if (layout == "by_study") {
    paste(study, logical, sep = "/")
  } else {
    paste(logical, study, sep = "/")
  }
}

# actual paths of logical folders for all studies; in multi-study layouts,
# code/ at the root (by study) or anywhere under code/ (by type) is also
# allowed, for analyses that combine studies
layout_expand <- function(logicals, layout, studies, allow_root_code = TRUE) {
  unique(unlist(lapply(logicals, function(d) {
    if (!layout_is_per_study(d, layout)) {
      return(d)
    }
    paths <- vapply(studies, function(s) layout_path(d, s, layout), "")
    if (d == "code" && allow_root_code) {
      paths <- c(paths, "code")
    }
    unname(paths)
  })))
}

# short label for a set of expanded folders, e.g. "study_*/data"
layout_label <- function(logical, layout) {
  if (!layout_is_per_study(logical, layout)) {
    logical
  } else if (logical == "code") {
    # see layout_expand(): combined analyses may live in code/
    if (layout == "by_study") "study_*/code or code" else "code"
  } else if (layout == "by_study") {
    paste0("study_*/", logical)
  } else {
    paste0(logical, "/study_*")
  }
}

# all folders to create, including intermediate ones, parents first
layout_all_dirs <- function(layout, studies) {
  leaves <- c(
    unlist(lapply(studies, function(s) {
      vapply(layout_study_dirs, function(d) layout_path(d, s, layout), "")
    })),
    layout_shared_dirs
  )
  ancestors <- function(p) {
    parts <- strsplit(p, "/", fixed = TRUE)[[1]]
    vapply(
      seq_along(parts),
      function(i) paste(parts[seq_len(i)], collapse = "/"),
      ""
    )
  }
  all <- unique(unlist(lapply(leaves, ancestors)))
  depth <- lengths(strsplit(all, "/", fixed = TRUE))
  unname(all[order(depth, all)])
}

# relative path from one project folder to another, e.g.
# rel_path("code/study_1", "data/processed/study_1") ->
#   "../../data/processed/study_1"
rel_path <- function(from, to) {
  from <- strsplit(from, "/", fixed = TRUE)[[1]]
  to <- strsplit(to, "/", fixed = TRUE)[[1]]
  n <- 0
  while (n < min(length(from), length(to)) && from[n + 1] == to[n + 1]) {
    n <- n + 1
  }
  up <- rep("..", length(from) - n)
  paste(c(up, to[seq_along(to) > n]), collapse = "/")
}

# sort "study_2", "study_10", "study_1" numerically
sort_studies <- function(x) {
  x[order(as.integer(sub("^study_", "", x)))]
}

# detect the layout of an existing project from its folders
detect_layout <- function(project_root) {
  is_study <- function(x) grepl("^study_[0-9]+$", x)
  subdirs <- function(p) {
    p <- file.path(project_root, p)
    if (!dir.exists(p)) {
      return(character(0))
    }
    basename(list.dirs(p, recursive = FALSE))
  }

  root_studies <- Filter(is_study, subdirs("."))
  if (length(root_studies) > 0) {
    return(list(layout = "by_study", studies = sort_studies(root_studies)))
  }

  nested <- unique(unlist(lapply(layout_study_dirs, function(d) {
    Filter(is_study, subdirs(d))
  })))
  if (length(nested) > 0) {
    return(list(layout = "by_type", studies = sort_studies(nested)))
  }

  list(layout = "single", studies = character(0))
}

# folder tree for the Structure section of the generated README
readme_tree <- function(layout, studies) {
  pad <- function(path, comment) {
    sprintf("%-22s# %s", path, comment)
  }
  desc <- c(
    code = "analysis and processing scripts (.qmd/.Rmd) and their rendered .html",
    raw = "raw data and codebooks/data dictionaries (should be read-only, except for removal of private data)",
    processed = "cleaned datasets and codebooks/data dictionaries",
    outputs = "outputs of the processing and analyses scripts",
    plots = "plots and figures, .png/.pdf/etc.",
    fitted_models = "fitted model objects, eg from brms, lme4, lavaan, etc.",
    results = "tables and matrices, eg for descriptive statistics, formatted statistical results, correlation tables",
    methods = "measures, implementations (qualtrics, lab.js, psychopy files, etc.), .docx files with items, etc.",
    preregistration = "preregistration documents",
    reports = "thesis, manuscript, preprints, slides, etc."
  )

  if (layout == "single") {
    return(c(
      pad("code/", desc[["code"]]),
      pad("reports/", desc[["reports"]]),
      "data/",
      pad("  raw/", desc[["raw"]]),
      pad("  processed/", desc[["processed"]]),
      pad("  outputs/", desc[["outputs"]]),
      pad("    plots/", desc[["plots"]]),
      pad("    fitted_models/", desc[["fitted_models"]]),
      pad("    results/", desc[["results"]]),
      pad("methods/", desc[["methods"]]),
      pad("preregistration/", desc[["preregistration"]])
    ))
  }

  others <- if (length(studies) > 1) {
    paste0(studies[-1], "/")
  } else {
    character(0)
  }

  if (layout == "by_study") {
    return(c(
      pad(paste0(studies[1], "/"), "one folder per study, each with the same structure:"),
      pad("  code/", desc[["code"]]),
      "  data/",
      pad("    raw/", desc[["raw"]]),
      pad("    processed/", desc[["processed"]]),
      pad("    outputs/", desc[["outputs"]]),
      pad("      plots/", desc[["plots"]]),
      pad("      fitted_models/", desc[["fitted_models"]]),
      pad("      results/", desc[["results"]]),
      pad("  methods/", desc[["methods"]]),
      pad("  preregistration/", desc[["preregistration"]]),
      pad(others, "same structure as study_1/"),
      pad("reports/", paste(desc[["reports"]], "(shared by all studies)"))
    ))
  }

  # by_type: one subfolder per study inside each folder
  sub_studies <- function(indent) {
    paste0(strrep(" ", indent), studies, "/")
  }
  c(
    pad("code/", desc[["code"]]),
    sub_studies(2),
    "data/",
    pad("  raw/", desc[["raw"]]),
    sub_studies(4),
    pad("  processed/", desc[["processed"]]),
    sub_studies(4),
    pad("  outputs/", desc[["outputs"]]),
    pad("    plots/", desc[["plots"]]),
    sub_studies(6),
    pad("    fitted_models/", desc[["fitted_models"]]),
    sub_studies(6),
    pad("    results/", desc[["results"]]),
    sub_studies(6),
    pad("methods/", desc[["methods"]]),
    sub_studies(2),
    pad("preregistration/", desc[["preregistration"]]),
    sub_studies(2),
    pad("reports/", paste(desc[["reports"]], "(shared by all studies)"))
  )
}
