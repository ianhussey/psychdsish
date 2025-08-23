#' Detect unused dependencies in a QMD or RMD file (via Quarto render)
#'
#' Instruments the document by wrapping `library()` / `require()` and tracing
#' exported functions. Always tries Quarto first, falling back to R Markdown
#' only if Quarto render fails.
#' 
#' @import jsonlite
#' @import tibble
#' @import dplyr
#' @import stringr
#' @import quarto
#' @import rmarkdown
#' 
#' @examples
#' \dontrun{
#' res <- check_unused_dependencies_single("test.qmd")
#' res
#' }
#' 
#' @param path Path to the .qmd or .Rmd file.
#' @return A tibble with one column: possibly_unused_packages
#'   (packages attached but never called, excluding base defaults).
#'
.check_unused_dependencies_single_file <- function(path) {
  if (!file.exists(path)) stop("File not found: ", path)
  
  # temp instrumented copy
  ext <- tools::file_ext(path); if (!nzchar(ext)) ext <- "qmd"
  tmp  <- tempfile(fileext = paste0(".", ext))
  if (!file.copy(path, tmp, overwrite = TRUE)) stop("Failed to copy input to temp file.")
  
  out_json <- tempfile(fileext = ".json")
  out_json_esc <- gsub("\\\\", "/", out_json)
  
  # ---- injected setup chunk (built as lines to avoid escaping hell) ----
  setup_lines <- c(
    "```{r}",
    ".sn_counts <- new.env(parent = emptyenv())",
    "",
    ".sn_install_tracers <- function(pkg) {",
    "  if (!requireNamespace(pkg, quietly = TRUE)) return(invisible(FALSE))",
    "  if (is.null(.sn_counts[[pkg]])) .sn_counts[[pkg]] <- 0L",
    "  ns <- getNamespace(pkg)",
    "  for (sym in getNamespaceExports(pkg)) {",
    "    if (!exists(sym, envir = ns, inherits = FALSE)) next",
    "    obj <- getExportedValue(pkg, sym)",
    "    if (!is.function(obj) || is.primitive(obj)) next",
    "    base::trace(",
    "      what   = sym,",
    "      where  = ns,",
    "      tracer = substitute({ .sn_counts[[PKG]] <<- .sn_counts[[PKG]] + 1L }, list(PKG = pkg)),",
    "      print  = FALSE",
    "    )",
    "  }",
    "  invisible(TRUE)",
    "}",
    "",
    ".sn_orig_library <- base::library",
    ".sn_orig_require <- base::require",
    "",
    "# Robustly extract the package name, without regex quote stripping",
    ".sn_pkg_name <- function(package, character.only = FALSE) {",
    "  if (missing(package)) return(NA_character_)",
    "  if (isTRUE(character.only)) return(as.character(package))",
    "  nm <- substitute(package)",
    "  if (is.symbol(nm))   return(as.character(nm))   # library(dplyr)",
    "  if (is.character(nm)) return(nm)                # library('dplyr')",
    "  as.character(nm)",
    "}",
    "",
    "library <- function(package, ..., character.only = FALSE) {",
    "  mc <- match.call()",
    "  mc[[1L]] <- quote(.sn_orig_library)",
    "  res <- eval(mc, parent.frame())",
    "  pkg_chr <- .sn_pkg_name(package, character.only = character.only)",
    "  if (!inherits(res, 'try-error') && is.character(pkg_chr) && nzchar(pkg_chr)) {",
    "    try(.sn_install_tracers(pkg_chr), silent = TRUE)",
    "  }",
    "  invisible(res)",
    "}",
    "",
    "require <- function(package, ..., character.only = FALSE) {",
    "  mc <- match.call()",
    "  mc[[1L]] <- quote(.sn_orig_require)",
    "  res <- eval(mc, parent.frame())",
    "  pkg_chr <- .sn_pkg_name(package, character.only = character.only)",
    "  if (isTRUE(res) && is.character(pkg_chr) && nzchar(pkg_chr)) {",
    "    try(.sn_install_tracers(pkg_chr), silent = TRUE)",
    "  }",
    "  res",
    "}",
    "```",
    ""
  )
  setup <- paste(setup_lines, collapse = "\n")
  
  # ---- injected finale chunk ----
  finale_lines <- c(
    "```{r}",
    "attached <- sub('^package:', '', grep('^package:', search(), value = TRUE))",
    "loaded   <- loadedNamespaces()",
    "calls    <- as.list(if (length(ls(.sn_counts))) mget(ls(.sn_counts), envir = .sn_counts) else list())",
    sprintf("jsonlite::write_json(list(attached = attached, loaded = loaded, calls = calls), '%s', auto_unbox = TRUE)", out_json_esc),
    "```",
    ""
  )
  finale <- paste(finale_lines, collapse = "\n")
  
  # write instrumented copy
  orig <- readLines(tmp, warn = FALSE)
  writeLines(c(setup, orig, finale), tmp)
  
  # render (Quarto first; fallback to R Markdown)
  run_quarto <- function() {
    if (!requireNamespace("quarto", quietly = TRUE)) stop("R package 'quarto' not installed.")
    quarto::quarto_render(tmp, quiet = FALSE)
  }
  run_rmd <- function() {
    if (!requireNamespace("rmarkdown", quietly = TRUE)) stop("R package 'rmarkdown' not installed.")
    rmarkdown::render(tmp, quiet = FALSE, envir = new.env(parent = globalenv()))
  }
  
  rendered <- isTRUE(tryCatch({ run_quarto(); TRUE },
                              error = function(e) { message("Quarto render failed → trying R Markdown. ", conditionMessage(e)); FALSE }))
  if (!rendered) {
    rendered <- isTRUE(tryCatch({ run_rmd(); TRUE },
                                error = function(e) { stop("Both Quarto and R Markdown renders failed: ", conditionMessage(e)) }))
  }
  
  if (!file.exists(out_json)) stop("Instrumentation JSON not produced. Check console above.")
  
  # read results
  res <- jsonlite::read_json(out_json, simplifyVector = TRUE)
  
  attached <- sort(unique(if (!is.null(res$attached)) res$attached else character()))
  loaded   <- sort(unique(if (!is.null(res$loaded))   res$loaded   else character()))
  calls    <- res$calls
  call_pkgs   <- if (length(calls)) names(calls) else character()
  call_counts <- if (length(calls)) as.integer(unlist(calls, use.names = FALSE)) else integer()
  
  pkgs <- sort(unique(c(attached, loaded, call_pkgs)))
  mm   <- match(pkgs, call_pkgs)
  cnts <- ifelse(is.na(mm), 0L, call_counts[mm])
  
  out <- tibble::tibble(
    package  = pkgs,
    attached = package %in% attached,
    loaded   = package %in% loaded,
    calls    = cnts
  )
  
  out$status <- dplyr::case_when(
    out$attached & out$calls > 0L  ~ "used during render (calls > 0)",
    out$attached & out$calls == 0L ~ "possibly unused (attached, 0 calls)",
    !out$attached & out$loaded     ~ "loaded indirectly (dependency)",
    TRUE                           ~ "unknown"
  )
  
  out |>
    dplyr::filter(status != "loaded indirectly (dependency)") |>
    dplyr::filter(!package %in% c("base","datasets","grDevices","graphics","methods","stats","utils")) |>
    dplyr::filter(status == "possibly unused (attached, 0 calls)") |>
    dplyr::select(possibly_unused_packages = package)
}
