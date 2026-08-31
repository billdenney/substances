#' Substance quantities with a different unit per element
#'
#' The long-format shape that laboratory data actually arrives in: one column of
#' values, one of unit strings, one of analyte names, all varying by row. This
#' is the entry point rather than an afterthought -- its purpose is to be
#' converted to a homogeneous [substances] vector, which is where arithmetic
#' happens.
#'
#' This mirrors [units::mixed_units] in intent. It differs in representation:
#' `mixed_units` is a list of length-1 `units` objects, whereas this keeps the
#' unit as a character attribute, which stays a flat vector for the long columns
#' this class exists to serve. It does not inherit from `units`, because there
#' is no one unit for it to inherit -- which is also why it defines no
#' arithmetic: [units::set_units()] converts it to a single unit, and the
#' `substances` vector that comes back is where the arithmetic happens.
#'
#' @param x A numeric vector.
#' @param unit A character vector of units, recycled to the length of `x`.
#' @param substance A character vector of substance names, synonyms or
#'   identifiers, recycled to the length of `x`.
#' @param system A conversion system name or object.
#'
#' @return A `mixed_substances` vector.
#'
#' @examples
#' m <- mixed_substances(c(100, 5.5, 140),
#'                       c("mg/dL", "mmol/L", "mg/dL"),
#'                       c("glucose", "glucose", "sodium"))
#' m
#' set_units(m, "mmol/L")
#' @export
mixed_substances <- function(x = double(), unit = character(),
                             substance = NA_character_, system = NULL) {
  system_obj <- get_system(system)
  x <- as.double(x)
  unit <- recycle_to(as.character(unit), length(x), "unit")
  substance <- recycle_to(as.character(substance), length(x), "substance")

  check_units_defined(unit)
  new_mixed_substances(x, resolve_or_stop(substance, system_obj), unit,
                       system_obj$name)
}

## Only so that set_units.mixed_substances()'s signature fits in 80 columns
## while keeping the default `units` itself uses.
mixed_set_units_mode <- function() units::units_options("set_units_mode")

new_mixed_substances <- function(x, substance_id, unit, system_name) {
  structure(as.double(x), substance = substance_id, unit = unit,
            system = system_name, class = "mixed_substances")
}

#' @export
substances.mixed_substances <- function(x) attr(x, "substance")

## Per-element unit strings, the counterpart of the single `units` attribute a
## homogeneous `substances` vector carries.
#' @export
substance_units.mixed_substances <- function(x) attr(x, "unit")

#' @export
format.mixed_substances <- function(x, ...) {
  stats::setNames(format_substance_like(x, ...), names(x))
}

#' @export
print.mixed_substances <- function(x, ...) {
  cat("<mixed_substances[", length(x), "]>\n", sep = "")
  print(stats::setNames(format_substance_like(x, ...), names(x)), quote = FALSE)
  invisible(x)
}

#' @export
as.double.mixed_substances <- function(x, ...) as.vector(unclass(x), "double")

#' @export
`[.mixed_substances` <- function(x, ...) {
  new_mixed_substances(unclass(x)[...], substances(x)[...],
                       substance_units(x)[...], substance_system_of(x))
}

#' @export
`[[.mixed_substances` <- `[.mixed_substances`

#' @export
rep.mixed_substances <- function(x, ...) {
  new_mixed_substances(rep(unclass(x), ...), rep(substances(x), ...),
                       rep(substance_units(x), ...), substance_system_of(x))
}

#' @export
c.mixed_substances <- function(..., recursive = FALSE) {
  args <- lapply(list(...), as_mixed_substances)
  systems <- unique(vapply(args, substance_system_of, character(1)))
  if (length(systems) > 1L) {
    stop("cannot combine `mixed_substances` from different systems: ",
         paste0("'", systems, "'", collapse = " and "), call. = FALSE)
  }
  new_mixed_substances(unlist(lapply(args, unclass)),
                       unlist(lapply(args, substances)),
                       unlist(lapply(args, substance_units)), systems)
}

## A homogeneous vector is a mixed one whose units all happen to agree, so
## widening in this direction loses nothing and lets c() mix the two classes.
as_mixed_substances <- function(x) {
  if (inherits(x, "mixed_substances")) {
    return(x)
  }
  if (inherits(x, "substances")) {
    return(new_mixed_substances(bare_values(x), substances(x),
                                rep(as.character(units(x)), length(x)),
                                substance_system_of(x)))
  }
  stop("cannot combine a `mixed_substances` vector with a ",
       paste(class(x), collapse = "/"), ".", call. = FALSE)
}

#' @export
mixed_units.substances <- function(x, values, ...) {
  stopifnot(missing(values))
  as_mixed_substances(x)
}

#' Convert a mixed_substances vector to a single unit
#'
#' @param x A [mixed_substances] vector.
#' @param value The target unit.
#' @param ... Unused.
#' @param mode As in [units::set_units()].
#' @return A homogeneous [substances] vector.
#' @export
set_units.mixed_substances <- function(x, value, ...,
                                       mode = mixed_set_units_mode()) {
  if (missing(value)) {
    stop("a target unit is required", call. = FALSE)
  } else if (mode == "symbols") {
    value <- substitute(value)
    if (is.name(value) || is.call(value)) {
      value <- format(value)
    }
  }
  to_sym <- as_symbolic_units(value)
  system <- substance_system_of(x)
  values <- as.vector(unclass(x), "double")
  ids <- substances(x)
  units_chr <- substance_units(x)

  out <- numeric(length(values))
  for (u in unique(units_chr)) {
    rows <- units_chr == u
    part <- new_substances(values[rows], ids[rows], as_symbolic_units(u),
                           system)
    out[rows] <- bare_values(convert_substance(part, to_sym))
  }
  new_substances(out, ids, to_sym, system)
}
