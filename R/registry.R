substances_env <- new.env(parent = emptyenv())

registry_files <- list(
  substances  = "substances.csv",
  synonyms    = "substance_synonyms.csv",
  parameters  = "substance_parameters.csv",
  conversions = "substance_conversions.csv",
  sources     = "sources.csv"
)

## Derived, so a table cannot be added to one list and forgotten in the other.
registry_tables <- names(registry_files)

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
#' These are the kinds the shipped registry uses, not a closed list. Nothing in
#' the conversion machinery knows what a molar mass is — it multiplies by
#' whatever quantities a substance carries and asks `units` which combination
#' works. A system may therefore declare kinds of its own through the
#' `parameter_units` argument of [substance_system()]: enzyme specific activity
#' in `U/mg`, a turnover number in `1/s`, a partition coefficient, anything that
#' is a property of the substance with units attached.
#'
#' @format A named character vector: the units each built-in parameter must be
#'   convertible to.
#' @export
substance_parameter_units <- c(
  molar_mass   = "g/mol",   # mass <-> amount
  density      = "g/mL",    # mass <-> volume, for a solid or liquid
  molar_volume = "L/mol",   # amount <-> volume, for a gas
  valence      = "eq/mol",  # amount <-> charge
  activity     = "mol/IU"   # WHO biological activity <-> amount
)

#' Create a conversion system
#'
#' A system is an isolated set of substances and their conversions. Because the
#' substance lives in R data rather than in the udunits database, several
#' systems can coexist in one session without colliding, and a [substances]
#' vector records the system it was created under so vectors from different
#' systems cannot be combined.
#'
#' This is the extension point. A package with its own substances needs only a
#' data frame of parameters; the identity table is optional and is derived from
#' whatever the other tables declare, so adding a substance can be as short as
#' one row. Inherit from `"substances"` to keep the shipped registry and add to
#' it, or omit `inherit` for a registry that contains only your own entries.
#'
#' @param name Name of the system. Must be unique within the session.
#' @param substances,synonyms,parameters,conversions,sources Data frames with
#'   the registry columns. Missing tables are created empty. If `substances` is
#'   omitted, an identity row is created for every `substance_id` the other
#'   tables mention.
#' @param parameter_units A named character vector declaring parameter kinds
#'   beyond the built-in ones, as `kind = "units it must be convertible to"`.
#'   Use this for properties the shipped registry does not cover — enzyme
#'   specific activity, a turnover number, a partition coefficient. The
#'   conversion machinery is indifferent to what a kind means; it only needs the
#'   units to be right.
#' @param inherit Name of a system to inherit entries from, or `NULL`. Entries
#'   in this system take precedence, and so do the parameter kinds it declares.
#' @param overwrite Replace an already-registered system of the same name. A
#'   `substances` vector records only its system's name, so replacing a system
#'   changes what every existing vector of that system means; registering over
#'   an existing name is an error unless this is `TRUE`.
#'
#' @return A `substance_system` object, invisibly registered under `name`.
#'
#' @examples
#' # a downstream package's whole registration, from one data frame
#' substance_system("example_pkg", inherit = "substances", parameters = data.frame(
#'   substance_id = "widgetol",
#'   parameter    = "molar_mass",
#'   value        = 100,
#'   unit         = "g/mol",
#'   source_id    = "internal-spec"))
#'
#' set_units(set_substances(1, "widgetol", "mg/dL", system = "example_pkg"),
#'           "mmol/L")
#'
#' # a parameter kind of your own: enzyme mass to catalytic activity
#' substance_system("example_enzymes",
#'   parameter_units = c(specific_activity = "U/mg"),
#'   parameters = data.frame(
#'     substance_id = "alkaline_phosphatase",
#'     parameter    = "specific_activity",
#'     value        = 1000,
#'     unit         = "U/mg",
#'     source_id    = "supplier-certificate"))
#'
#' set_units(set_substances(1, "alkaline_phosphatase", "ug",
#'                          system = "example_enzymes"), "U")
#' @export
substance_system <- function(name, substances = NULL, synonyms = NULL,
                             parameters = NULL, conversions = NULL,
                             sources = NULL, parameter_units = NULL,
                             inherit = NULL, overwrite = FALSE) {
  stopifnot(is.character(name), length(name) == 1L, nzchar(name))
  systems <- get("systems", envir = substances_env)
  if (name %in% names(systems) && !isTRUE(overwrite)) {
    stop("a conversion system named '", name, "' is already registered.",
         "\n  Re-registering it would change what every existing `substances` ",
         "vector of that system means, since a vector records only the name.",
         "\n  Pass overwrite = TRUE if that is what you intend.", call. = FALSE)
  }

  tables <- mget(registry_tables, environment())
  for (tbl in registry_tables) {
    tables[[tbl]] <- coerce_registry_table(tables[[tbl]], tbl)
  }

  kinds <- declared_kinds(parameter_units)   # this system's own declarations

  # An identity table is bookkeeping, not information, when the caller has only
  # a handful of substances to declare. Derive it rather than demand it.
  if (!nrow(tables$substances)) {
    declared <- unique(unlist(lapply(
      tables[c("synonyms", "parameters", "conversions")], `[[`, "substance_id")))
    declared <- declared[!is.na(declared)]
    if (length(declared)) {
      tables$substances <- coerce_registry_table(
        data.frame(substance_id = declared, name = declared,
                   stringsAsFactors = FALSE), "substances")
    }
  }

  if (!is.null(inherit)) {
    parent <- get_system(inherit)
    # local rows come first, so dropping later duplicates makes them win
    for (tbl in registry_tables) {
      tables[[tbl]] <- dedupe_registry(rbind(tables[[tbl]], parent[[tbl]]), tbl)
    }
    ## Kinds inherit by the same rule as the tables: local wins over parent.
    ## Letting the built-ins win here would silently discard a parent's
    ## redefinition and leave the child unbuildable.
    kinds <- c(kinds, parent$parameter_units)
  }
  ## Built-ins fill in whatever neither declared, so they are last.
  kinds <- c(kinds, substance_parameter_units)
  kinds <- kinds[!duplicated(names(kinds))]

  system <- structure(c(list(name = name), tables,
                        list(parameter_units = kinds)),
                      class = "substance_system")
  validate_system(system)
  ## The name lookup is derived, not authored, so build it once here rather
  ## than rebuilding it on every substance_resolve() call.
  keys <- lookup_keys(system)
  system$lookup <- stats::setNames(keys$id, keys$key)[!duplicated(keys$key)]
  systems[[name]] <- system
  assign("systems", systems, envir = substances_env)
  invisible(system)
}

## The kinds a system declares for itself, validated. Merging with a parent's
## and with the built-ins is the caller's job, so precedence stays in one place.
declared_kinds <- function(parameter_units) {
  if (!length(parameter_units)) {
    return(character(0))
  }
  nms <- names(parameter_units)
  if (is.null(nms) || anyNA(nms) || !all(nzchar(nms))) {
    stop("`parameter_units` must be a named character vector, as ",
         "c(kind = \"units\")", call. = FALSE)
  }
  stats::setNames(as.character(parameter_units), nms)
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
  for (nm in missing_cols) {
    x[[nm]] <- NA_character_
  }
  x <- x[, cols, drop = FALSE]
  # numeric columns stay numeric; everything else is character
  numeric_cols <- c("value", "slope", "intercept")
  for (nm in intersect(numeric_cols, cols)) {
    x[[nm]] <- as.numeric(x[[nm]])
  }
  for (nm in setdiff(cols, numeric_cols)) {
    x[[nm]] <- as.character(x[[nm]])
  }
  if ("status" %in% cols) {
    x$status[is.na(x$status)] <- "ok"
  }
  x
}

## Conversion kinds apply_explicit() knows how to evaluate. `factor` is `affine`
## with no offset; both are checked here so an unusable row is rejected when the
## registry is built rather than when someone converts.
conversion_kinds <- c("affine", "factor")

## Natural key per table, used to let an inheriting system override its parent.
registry_keys <- list(
  substances    = "substance_id",
  synonyms      = "synonym",
  parameters    = c("substance_id", "parameter"),
  conversions   = c("substance_id", "from_unit", "to_unit"),
  sources       = "source_id"
)

dedupe_registry <- function(x, tbl) {
  key <- as.data.frame(lapply(x[registry_keys[[tbl]]], tolower),
                       stringsAsFactors = FALSE)
  x[!duplicated(key), , drop = FALSE]
}

## Every rule here is a property of "a registry", not of the shipped data, so it
## has to run at registration: a downstream system built from data frames or
## CSVs gets exactly the same checks the bundled one does. What is left to the
## test suite is editorial policy about the bundled data -- that its citations
## are complete, that its withheld values explain themselves -- which a
## downstream registry is entitled to decide for itself.
validate_system <- function(system) {
  p <- system$parameters
  cv <- system$conversions
  kinds <- system$parameter_units
  where <- paste0(" in system '", system$name, "'")

  unknown <- setdiff(unique(p$parameter), names(kinds))
  if (length(unknown)) {
    stop("unknown parameter kind(s)", where, ": ",
         paste(unknown, collapse = ", "),
         "\n  Known kinds: ", paste(names(kinds), collapse = ", "),
         "\n  Declare a new one with the `parameter_units` argument of ",
         "substance_system().", call. = FALSE)
  }

  ## A parameter with the wrong units is not a bridge, it is a silently wrong
  ## answer, so check the dimension here rather than at conversion time. Only
  ## the distinct (unit, kind) pairs need checking, not every row.
  pairs <- unique(data.frame(unit = p$unit, kind = p$parameter,
                             stringsAsFactors = FALSE))
  for (i in seq_len(nrow(pairs))) {
    required <- kinds[[pairs$kind[i]]]
    if (!are_convertible(pairs$unit[i], required)) {
      bad <- p$substance_id[p$unit == pairs$unit[i] & p$parameter == pairs$kind[i]]
      stop("parameter '", pairs$kind[i], "' for '", bad[1L], "' has units ",
           pairs$unit[i], ", which are not convertible to ", required,
           " as that kind requires.", call. = FALSE)
    }
  }

  ## A unit string udunits cannot parse makes a conversion row unreachable
  ## rather than wrong, which is worse: it never matches and never complains.
  bad_unit <- unique(c(cv$from_unit, cv$to_unit))
  bad_unit <- bad_unit[!is.na(bad_unit)]
  bad_unit <- bad_unit[!vapply(bad_unit, unit_is_defined, logical(1))]
  if (length(bad_unit)) {
    stop("conversion unit(s)", where, " not recognised by udunits: ",
         paste0("\"", bad_unit, "\"", collapse = ", "), call. = FALSE)
  }

  usable <- cv$status %in% "ok"
  bad_kind <- setdiff(unique(cv$kind[usable]), conversion_kinds)
  if (length(bad_kind)) {
    stop("unsupported conversion kind(s)", where, ": ",
         paste(bad_kind, collapse = ", "),
         "\n  Supported: ", paste(conversion_kinds, collapse = ", "),
         call. = FALSE)
  }
  if (any(usable & is.na(cv$slope))) {
    stop("conversion(s)", where, " marked \"ok\" with no slope: ",
         paste(cv$substance_id[usable & is.na(cv$slope)], collapse = ", "),
         "\n  Every value they touch would become NA.", call. = FALSE)
  }

  dup <- duplicated(p[, c("substance_id", "parameter")])
  if (any(dup)) {
    stop("duplicate (substance_id, parameter)", where, ": ",
         paste(unique(paste(p$substance_id[dup], p$parameter[dup])),
               collapse = ", "), call. = FALSE)
  }

  ids <- system$substances$substance_id
  if (anyDuplicated(ids)) {
    stop("duplicate substance_id", where, ": ",
         paste(unique(ids[duplicated(ids)]), collapse = ", "), call. = FALSE)
  }
  for (tbl in c("synonyms", "parameters", "conversions")) {
    orphan <- setdiff(system[[tbl]]$substance_id, ids)
    if (length(orphan)) {
      stop("substance_id(s) in `", tbl, "` with no entry in `substances`: ",
           paste(orphan, collapse = ", "), call. = FALSE)
    }
  }

  ## One name must mean one substance, or substance_resolve() silently picks
  ## whichever row came first.
  keys <- lookup_keys(system)
  clash <- tapply(keys$id, keys$key, function(z) length(unique(z)))
  if (any(clash > 1L)) {
    stop("name(s)", where, " resolving to more than one substance: ",
         paste(names(clash)[clash > 1L], collapse = ", "), call. = FALSE)
  }
  invisible(system)
}

get_system <- function(system = NULL) {
  if (inherits(system, "substance_system")) {
    return(system)
  }
  systems <- get0("systems", envir = substances_env, ifnotfound = list())
  if (is.null(system)) {
    system <- substance_default_system()
  }
  if (!system %in% names(systems)) {
    stop("no conversion system named '", system, "'. Available: ",
         paste(names(systems), collapse = ", "), call. = FALSE)
  }
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

## Every string that names a substance, paired with the id it names. Identity
## ids and names come before synonyms so a synonym cannot shadow a real name.
lookup_keys <- function(system) {
  data.frame(
    key = tolower(c(system$substances$substance_id, system$substances$name,
                    system$synonyms$synonym)),
    id = c(system$substances$substance_id, system$substances$substance_id,
           system$synonyms$substance_id),
    stringsAsFactors = FALSE)
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
  lookup <- system$lookup
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
  ## One substance at a time: the return value is keyed by parameter name, so a
  ## vector of ids would silently collapse two substances' molar masses into one
  ## `molar_mass` entry and hand back whichever came first.
  if (length(substance_id) != 1L) {
    stop("`substance_id` must name a single substance, not ",
         length(substance_id), ".", call. = FALSE)
  }
  system <- get_system(system)
  p <- system$parameters
  p <- p[p$substance_id %in% substance_id & p$status %in% "ok" & !is.na(p$value), ,
         drop = FALSE]
  if (!nrow(p)) {
    return(list())
  }
  stats::setNames(
    lapply(seq_len(nrow(p)), function(i) {
      units::set_units(p$value[i], p$unit[i], mode = "standard")
    }),
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
  if (is.na(id)) {
    stop("unknown substance: ", x, call. = FALSE)
  }
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
  if (!is.na(x$substance$formula) && nzchar(x$substance$formula)) {
    cat("  formula: ", x$substance$formula, "\n", sep = "")
  }
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
      if (!is.na(x$parameters$note[i]) && nzchar(x$parameters$note[i])) {
        cat(strwrap(x$parameters$note[i], width = 78, prefix = "      ",
                    initial = "      "), sep = "\n")
      }
    }
  }
  if (nrow(x$conversions)) {
    cat("  conversions:\n")
    for (i in seq_len(nrow(x$conversions))) {
      cat(sprintf("    %s -> %s (%s) %s\n",
                  x$conversions$from_unit[i], x$conversions$to_unit[i],
                  x$conversions$kind[i],
                  ifelse(is.na(x$conversions$citation[i]), "",
                         x$conversions$citation[i])))
    }
  }
  invisible(x)
}
