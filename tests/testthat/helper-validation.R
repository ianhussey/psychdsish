# shared helpers for the validator tests

failed_tests <- function(res) as.character(res$Test[res$Status == "FAIL"])

warned_tests <- function(res) as.character(res$Test[res$Status == "WARN"])
