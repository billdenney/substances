#' Read a registry table shipped as CSV
#'
#' The reference data is CSV so that every value is reviewable in a diff and
#' carries its own citation. See `vignette` sources in `data-raw/` for how the
#' shipped tables are built.
#'
#' @param table One of `"substances"`, `"synonyms"`, `"parameters"`,
#'   `"conversions"`, `"sources"`.
#' @param path Directory to read from; defaults to the installed `extdata`.
#' @return A data frame.
#' @export
substance_read_csv <- function(table, path = NULL) {
  table <- match.arg(table, registry_tables)
  if (is.null(path)) {
    path <- system.file("extdata", package = "substances", mustWork = TRUE)
  }
  file <- file.path(path, registry_files[[table]])
  if (!file.exists(file)) {
    stop("registry file not found: ", file, call. = FALSE)
  }
  out <- utils::read.csv(file, stringsAsFactors = FALSE, na.strings = c("NA", ""))
  if (!nrow(out)) {
    stop("registry file is empty: ", file,
         "\n  An empty registry would make every conversion fail with a ",
         "confusing message, so this is an error rather than a warning.",
         call. = FALSE)
  }
  out
}

#' Build the conversion system shipped with the package
#'
#' @param name Name to register it under.
#' @param path Directory holding the CSVs.
#' @return A `substance_system`, invisibly.
#' @export
substance_load_default <- function(name = "substances", path = NULL) {
  tables <- lapply(stats::setNames(registry_tables, registry_tables),
                   substance_read_csv, path = path)
  substance_system(name,
                   substances = tables$substances,
                   synonyms = tables$synonyms,
                   parameters = tables$parameters,
                   conversions = tables$conversions,
                   sources = tables$sources)
}
