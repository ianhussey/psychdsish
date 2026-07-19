# Declare column names used with non-standard evaluation (dplyr/tidy pipelines)
# so that R CMD check does not report them as undefined global variables.
globalVariables(c(
  "status",
  "test",
  "details",
  "package",
  "possibly_unused_packages"
))
