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

#' Build a conversion system from a directory of CSVs
#'
#' Reads whichever of the registry tables are present and registers them as a
#' system. The package's own registry is one use of this, not the only one: a
#' project keeping its substances in version control can ship a directory with
#' as little as `substance_parameters.csv` in it and load it the same way.
#'
#' @param name Name to register the system under.
#' @param path Directory holding the CSVs; defaults to the installed `extdata`.
#' @param ... Passed to [substance_system()], so `inherit`, `parameter_units`
#'   and `overwrite` work here too.
#' @return A `substance_system`, invisibly.
#'
#' @examples
#' # the shipped registry, rebuilt under another name
#' substance_load_default("a_copy")
#' @export
substance_load_default <- function(name = "substances", path = NULL, ...) {
  if (is.null(path)) {
    path <- system.file("extdata", package = "substances", mustWork = TRUE)
  }
  present <- registry_tables[file.exists(file.path(path, registry_files))]
  if (!length(present)) {
    stop("no registry CSV found in ", path,
         "\n  Expected at least one of: ",
         paste(unlist(registry_files), collapse = ", "), call. = FALSE)
  }
  tables <- lapply(stats::setNames(present, present), substance_read_csv,
                   path = path)
  do.call(substance_system, c(list(name), tables, list(...)))
}
