#' Update dataset_description.json with column names from CSV files
#'
#' This function scans for CSV files in the data directory (and subdirectories)
#' that match the psych-ds naming pattern and adds all unique column names to the
#' variableMeasured array in the dataset_description.json file.
#'
#' @param project_root Character scalar. Path to the root directory of the project.
#'   Defaults to `"../"`
#'
#' @details
#' The function performs the following steps:
#' \enumerate{
#'   \item Searches for CSV files matching the pattern `([a-z]+-[a-zA-Z0-9]+)(_[a-z]+-[a-zA-Z0-9]+)*_data\.csv` in the data directory
#'   \item Reads the header row of each matching CSV file
#'   \item Collects all unique column names across all files
#'   \item Reads the existing dataset_description.json file
#'   \item Updates the variableMeasured array with PropertyValue objects for each new column
#'   \item Writes the updated JSON back to the file
#' }
#'
#' Each variable is added in the format:
#' \preformatted{
#' {
#'   "@type": "PropertyValue",
#'   "name": "<column_name>",
#'   "description": ""
#' }
#' }
#'
#' Existing variables are preserved - only new variables are added.
#'
#' @return
#' Invisibly returns a list with:
#' \itemize{
#'   \item `csv_files_found` - character vector of CSV files that were processed
#'   \item `variables_added` - character vector of column names that were added
#'   \item `variables_existing` - character vector of variables that already existed
#' }
#'
#' @examples
#' \dontrun{
#' # Update with default settings
#' update_dataset_description_variables()
#'
#' # Update from a specific project directory
#' update_dataset_description_variables(project_root = "./my_project")
#' }
#'
#' @export
update_dataset_description_variables <- function(project_root = "../") {
  
  # Helper functions
  join <- function(...) file.path(..., fsep = .Platform$file.sep)
  
  # Check if required packages are available, if not use base R alternatives
  has_jsonlite <- requireNamespace("jsonlite", quietly = TRUE)
  
  # Paths
  data_path <- join(project_root, "data")
  json_path <- join(project_root, "dataset_description.json")
  
  # Check if dataset_description.json exists
  if (!file.exists(json_path)) {
    stop("dataset_description.json not found at: ", json_path, 
         "\nPlease run create_project_skeleton() first or ensure the file exists.")
  }
  
  # Check if data directory exists
  if (!dir.exists(data_path)) {
    warning("Data directory not found at: ", data_path)
    return(invisible(list(
      csv_files_found = character(0),
      variables_added = character(0),
      variables_existing = character(0)
    )))
  }
  
  # Find CSV files matching the pattern
  csv_pattern <- "([a-z]+-[a-zA-Z0-9]+)(_[a-z]+-[a-zA-Z0-9]+)*_data\\.csv"
  all_files <- list.files(data_path, pattern = "\\.csv$", recursive = TRUE, full.names = TRUE)
  csv_files <- all_files[grepl(csv_pattern, basename(all_files))]
  
  if (length(csv_files) == 0) {
    message("No CSV files found matching pattern: ", csv_pattern)
    return(invisible(list(
      csv_files_found = character(0),
      variables_added = character(0),
      variables_existing = character(0)
    )))
  }
  
  message("Found ", length(csv_files), " CSV file(s) matching pattern:")
  message(paste("  -", basename(csv_files), collapse = "\n"))
  
  # Extract column names from all CSV files
  all_columns <- character(0)
  
  for (csv_file in csv_files) {
    tryCatch({
      # Read just the header row
      headers <- names(read.csv(csv_file, nrows = 0, check.names = FALSE))
      all_columns <- c(all_columns, headers)
      message("Extracted ", length(headers), " column(s) from: ", basename(csv_file))
    }, error = function(e) {
      warning("Could not read CSV file: ", csv_file, " - ", e$message)
    })
  }
  
  # Get unique column names
  unique_columns <- unique(all_columns)
  
  if (length(unique_columns) == 0) {
    message("No columns found in CSV files.")
    return(invisible(list(
      csv_files_found = basename(csv_files),
      variables_added = character(0),
      variables_existing = character(0)
    )))
  }
  
  message("Found ", length(unique_columns), " unique column name(s):")
  message(paste("  -", unique_columns, collapse = "\n"))
  
  # Read existing JSON file
  if (has_jsonlite) {
    json_data <- jsonlite::fromJSON(json_path)
  } else {
    # Fallback to base R JSON parsing (basic implementation)
    json_text <- readLines(json_path, warn = FALSE)
    json_text <- paste(json_text, collapse = "")
    
    json_data <- list()
    
    # Extract name
    name_match <- regmatches(json_text, regexpr('"name"\\s*:\\s*"[^"]*"', json_text))
    if (length(name_match) > 0) {
      json_data$name <- gsub('"name"\\s*:\\s*"|"', "", name_match)
    }
    
    # Extract description  
    desc_match <- regmatches(json_text, regexpr('"description"\\s*:\\s*"[^"]*"', json_text))
    if (length(desc_match) > 0) {
      json_data$description <- gsub('"description"\\s*:\\s*"|"', "", desc_match)
    }
    
    # For simplicity, we'll assume variableMeasured is empty or simple
    json_data$variableMeasured <- list()
  }
  
  # Get existing variable names to avoid duplicates
  existing_vars <- character(0)
  
  if (!is.null(json_data$variableMeasured) && length(json_data$variableMeasured) > 0) {
    if (has_jsonlite) {
      # Handle both list and data.frame formats that jsonlite might return
      if (is.data.frame(json_data$variableMeasured)) {
        existing_vars <- json_data$variableMeasured$name
      } else if (is.list(json_data$variableMeasured)) {
        existing_vars <- sapply(json_data$variableMeasured, function(x) x$name %||% "")
      }
    }
  }
  
  # Keep existing variables and add new ones
  if (has_jsonlite && !is.null(json_data$variableMeasured)) {
    # Convert data.frame back to list format if needed
    if (is.data.frame(json_data$variableMeasured)) {
      new_variables <- apply(json_data$variableMeasured, 1, function(row) {
        list(
          `@type` = row[["@type"]],
          name = row[["name"]],
          description = row[["description"]]
        )
      }, simplify = FALSE)
    } else {
      new_variables <- json_data$variableMeasured
    }
  } else {
    new_variables <- list()
  }
  
  # Only add columns that don't already exist
  columns_to_add <- setdiff(unique_columns, existing_vars)
  
  if (length(columns_to_add) == 0) {
    message("All variables already exist in dataset_description.json")
    return(invisible(list(
      csv_files_found = basename(csv_files),
      variables_added = character(0),
      variables_existing = existing_vars
    )))
  }
  
  # Add new variables
  for (col_name in columns_to_add) {
    new_var <- list(
      `@type` = "PropertyValue",
      name = col_name,
      description = ""
    )
    new_variables[[length(new_variables) + 1]] <- new_var
  }
  
  # Update the JSON data
  json_data$variableMeasured <- new_variables
  
  # Write back to file
  if (has_jsonlite) {
    # Use jsonlite for pretty formatting
    json_text <- jsonlite::toJSON(json_data, pretty = TRUE, auto_unbox = TRUE)
  } else {
    # Create JSON manually for base R
    variables_json <- sapply(new_variables, function(var) {
      paste0('    {\n',
             '      "@type": "', var$`@type`, '",\n',
             '      "name": "', var$name, '",\n',
             '      "description": "', var$description, '"\n',
             '    }')
    })
    
    json_text <- paste0(
      '{\n',
      '  "@context": "schema.org/",\n',
      '  "@type": "Dataset",\n',
      '  "name": "', json_data$name %||% "", '",\n',
      '  "description": "', json_data$description %||% "", '",\n',
      '  "variableMeasured": [\n',
      paste(variables_json, collapse = ",\n"),
      '\n  ]\n',
      '}\n'
    )
  }
  
  writeLines(json_text, json_path)
  
  message("Updated dataset_description.json with ", length(columns_to_add), " new variable(s)")
  
  invisible(list(
    csv_files_found = basename(csv_files),
    variables_added = columns_to_add,
    variables_existing = existing_vars
  ))
}

# Helper function for null coalescing
`%||%` <- function(a, b) if (is.null(a)) b else a
