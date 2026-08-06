substances_env <- new.env(parent = emptyenv())

registry_tables <- c("substances", "synonyms", "parameters", "conversions", "sources")

registry_files <- list(
  substances  = "substances.csv",
  synonyms    = "substance_synonyms.csv",
  parameters  = "substance_parameters.csv",
  conversions = "substance_conversions.csv",
  sources     = "sources.csv"
)

registry_columns <- list(
  substances  = c("substance_id", "name", "formula", "cas", "inchikey", "pubchem_cid"),
  synonyms    = c("substance_id", "synonym", "context", "source_id"),
  parameters  = c("substance_id", "parameter", "value", "unit", "status",
                  "source_id", "note"),
  conversions = c("substance_id", "from_unit", "to_unit", "kind", "slope",
                  "intercept", "status", "source_id", "note"),
  sources     = c("source_id", "citation", "url", "accessed")
)

#' Parameters that bridge otherwise incommensurable dimensions
#'
#' Each is a quantity of the substance itself, so a single value covers every
#' unit pair it can bridge: one `molar_mass` converts mg/dL to mmol/L, g to mol
#' and ug/mL to nmol/L alike.
#'
#' @format A named character vector of the units each parameter must have.
#' @export
substance_parameter_units <- c(
  molar_mass = "g/mol",   # mass <-> amount
  density    = "g/mL",    # mass <-> volume
  valence    = "eq/mol",  # amount <-> charge
  activity   = "mol/IU"   # WHO biological activity <-> amount
)

#' Create a conversion system
#'
#' A system is an isolated set of substances and their conversions. Because the
#' substance lives in R data rather than in the udunits database, several
#' systems can coexist in one session without colliding, and a [substance]
#' vector records the system it was created under so vectors from different
#' systems cannot be combined.
#'
#' @param name Name of the system. Must be unique within the session.
#' @param substances,synonyms,parameters,conversions,sources Data frames with
#'   the registry columns. Missing tables are created empty.
#' @param inherit Name of a system to inherit entries from, or `NULL`. Entries
#'   in this system take precedence.
#'
#' @return A `substance_system` object, invisibly registered under `name`.
#' @export
substance_system <- function(name, substances = NULL, synonyms = NULL,
                             parameters = NULL, conversions = NULL,
                             sources = NULL, inherit = NULL) {
  stopifnot(is.character(name), length(name) == 1L, nzchar(name))
  tables <- list(substances = substances, synonyms = synonyms,
                 parameters = parameters, conversions = conversions,
                 sources = sources)
  for (tbl in registry_tables)
    tables[[tbl]] <- coerce_registry_table(tables[[tbl]], tbl)

  if (!is.null(inherit)) {
    parent <- get_system(inherit)
    # local rows come first, so dropping later duplicates makes them win
    for (tbl in registry_tables)
      tables[[tbl]] <- dedupe_registry(rbind(tables[[tbl]], parent[[tbl]]), tbl)
  }

  system <- structure(c(list(name = name), tables), class = "substance_system")
  validate_system(system)
  systems <- get("systems", envir = substances_env)
  systems[[name]] <- system
  assign("systems", systems, envir = substances_env)
  invisible(system)
}

coerce_registry_table <- function(x, tbl) {
  cols <- registry_columns[[tbl]]
  if (is.null(x)) {
    x <- as.data.frame(
      stats::setNames(rep(list(character(0)), length(cols)), cols),
      stringsAsFactors = FALSE)
  }
  x <- as.data.frame(x, stringsAsFactors = FALSE)
  missing_cols <- setdiff(cols, names(x))
  for (nm in missing_cols) x[[nm]] <- NA_character_
  x <- x[, cols, drop = FALSE]
  # numeric columns stay numeric; everything else is character
  for (nm in intersect(c("value", "slope", "intercept"), cols))
    x[[nm]] <- as.numeric(x[[nm]])
  for (nm in setdiff(cols, c("value", "slope", "intercept")))
    x[[nm]] <- as.character(x[[nm]])
  if ("status" %in% cols) x$status[is.na(x$status)] <- "ok"
  x
}

## Natural key per table, used to let an inheriting system override its parent.
registry_keys <- list(
  substances  = "substance_id",
  synonyms    = "synonym",
  parameters  = c("substance_id", "parameter"),
  conversions = c("substance_id", "from_unit", "to_unit"),
  sources     = "source_id"
)

dedupe_registry <- function(x, tbl) {
  key <- do.call(paste, c(lapply(registry_keys[[tbl]], function(k) tolower(x[[k]])),
                          list(sep = "\r")))
  x[!duplicated(key), , drop = FALSE]
}

validate_system <- function(system) {
  p <- system$parameters
  unknown <- setdiff(unique(p$parameter), names(substance_parameter_units))
  if (length(unknown))
    stop("unknown parameter(s) in system '", system$name, "': ",
         paste(unknown, collapse = ", "), call. = FALSE)

  dup <- duplicated(p[, c("substance_id", "parameter")])
  if (any(dup))
    stop("duplicate (substance_id, parameter) in system '", system$name, "': ",
         paste(unique(paste(p$substance_id[dup], p$parameter[dup])),
               collapse = ", "), call. = FALSE)

  ids <- system$substances$substance_id
  for (tbl in c("synonyms", "parameters", "conversions")) {
    orphan <- setdiff(system[[tbl]]$substance_id, ids)
    if (length(orphan))
      stop("substance_id(s) in `", tbl, "` with no entry in `substances`: ",
           paste(orphan, collapse = ", "), call. = FALSE)
  }
  invisible(system)
}

get_system <- function(system = NULL) {
  if (inherits(system, "substance_system")) return(system)
  systems <- get0("systems", envir = substances_env, ifnotfound = list())
  if (is.null(system)) system <- substance_default_system()
  if (!system %in% names(systems))
    stop("no conversion system named '", system, "'. Available: ",
         paste(names(systems), collapse = ", "), call. = FALSE)
  systems[[system]]
}

#' Get or set the default conversion system
#'
#' @param name Name of a registered system.
#' @return The name of the default system.
#' @export
substance_default_system <- function() {
  get0("default_system", envir = substances_env, ifnotfound = "substances")
}

#' @rdname substance_default_system
#' @export
substance_set_default_system <- function(name) {
  get_system(name)  # errors if unknown
  old <- substance_default_system()
  assign("default_system", name, envir = substances_env)
  invisible(old)
}

#' List registered conversion systems
#'
#' @return A character vector of system names.
#' @export
substance_systems <- function() {
  names(get0("systems", envir = substances_env, ifnotfound = list()))
}

#' Resolve a substance name or synonym to its identifier
#'
#' Matching is case-insensitive and ignores surrounding whitespace. Unmatched
#' names return `NA`; it is the caller's job to decide whether that is an error.
#'
#' @param x Character vector of names, synonyms or identifiers.
#' @param system A system name or object.
#' @return A character vector of `substance_id`, `NA` where unmatched.
#' @export
substance_resolve <- function(x, system = NULL) {
  system <- get_system(system)
  key <- tolower(trimws(as.character(x)))
  lookup <- c(
    stats::setNames(system$substances$substance_id,
                    tolower(system$substances$substance_id)),
    stats::setNames(system$substances$substance_id,
                    tolower(system$substances$name)),
    stats::setNames(system$synonyms$substance_id,
                    tolower(system$synonyms$synonym)))
  lookup <- lookup[!duplicated(names(lookup))]
  out <- unname(lookup[key])
  out[is.na(key)] <- NA_character_
  out
}

#' Look up a substance's bridge parameters
#'
#' @param substance_id A single substance identifier.
#' @param system A system name or object.
#' @return A named list of [units::units] quantities, one per available
#'   parameter. Parameters whose `status` is not `"ok"` are omitted; see
#'   [substance_info()] to inspect them.
#' @export
substance_parameters <- function(substance_id, system = NULL) {
  system <- get_system(system)
  p <- system$parameters
  p <- p[p$substance_id %in% substance_id & p$status %in% "ok" & !is.na(p$value), ,
         drop = FALSE]
  if (!nrow(p)) return(list())
  stats::setNames(
    lapply(seq_len(nrow(p)), function(i)
      units::set_units(p$value[i], p$unit[i], mode = "standard")),
    p$parameter)
}

#' Report everything the registry knows about a substance
#'
#' @param x A substance name, synonym or identifier.
#' @param system A system name or object.
#' @return A list with the identity row, parameters (including non-`ok` ones)
#'   and explicit conversions, each joined to its source citation.
#' @export
substance_info <- function(x, system = NULL) {
  system <- get_system(system)
  id <- substance_resolve(x, system)
  if (is.na(id)) stop("unknown substance: ", x, call. = FALSE)
  cite <- function(d) {
    d$citation <- system$sources$citation[match(d$source_id, system$sources$source_id)]
    d
  }
  structure(list(
    substance   = system$substances[system$substances$substance_id == id, , drop = FALSE],
    parameters  = cite(system$parameters[system$parameters$substance_id == id, , drop = FALSE]),
    conversions = cite(system$conversions[system$conversions$substance_id == id, , drop = FALSE])
  ), class = "substance_info")
}

#' @export
print.substance_system <- function(x, ...) {
  cat("<substance_system '", x$name, "'>\n", sep = "")
  cat("  substances:  ", nrow(x$substances), "\n", sep = "")
  cat("  parameters:  ", nrow(x$parameters), "\n", sep = "")
  cat("  conversions: ", nrow(x$conversions), "\n", sep = "")
  cat("  sources:     ", nrow(x$sources), "\n", sep = "")
  invisible(x)
}

#' @export
print.substance_info <- function(x, ...) {
  cat("<substance ", x$substance$substance_id, ">  ", x$substance$name, "\n", sep = "")
  if (!is.na(x$substance$formula) && nzchar(x$substance$formula))
    cat("  formula: ", x$substance$formula, "\n", sep = "")
  if (nrow(x$parameters)) {
    cat("  parameters:\n")
    for (i in seq_len(nrow(x$parameters))) {
      cat(sprintf("    %-11s %-12s %-9s %s\n",
                  x$parameters$parameter[i],
                  format(x$parameters$value[i]),
                  x$parameters$unit[i],
                  paste0("[", x$parameters$status[i], "] ",
                         ifelse(is.na(x$parameters$citation[i]), "",
                                x$parameters$citation[i]))))
      # the note is where reference conditions and caveats live, so a value is
      # not really reviewable without it
      if (!is.na(x$parameters$note[i]) && nzchar(x$parameters$note[i]))
        cat(strwrap(x$parameters$note[i], width = 78, prefix = "      ",
                    initial = "      "), sep = "\n")
    }
  }
  if (nrow(x$conversions)) {
    cat("  conversions:\n")
    for (i in seq_len(nrow(x$conversions)))
      cat(sprintf("    %s -> %s (%s) %s\n",
                  x$conversions$from_unit[i], x$conversions$to_unit[i],
                  x$conversions$kind[i],
                  ifelse(is.na(x$conversions$citation[i]), "",
                         x$conversions$citation[i])))
  }
  invisible(x)
}
