#' Substance quantities with a different unit per element
#'
#' The long-format shape that laboratory data actually arrives in: one column of
#' values, one of unit strings, one of analyte names, all varying by row. This
#' is the entry point rather than an afterthought -- its purpose is to be
#' converted to a homogeneous [substance] vector, which is where arithmetic
#' happens.
#'
#' This mirrors [units::mixed_units] in intent. It differs in representation:
#' `mixed_units` is a list of length-1 `units` objects, whereas this keeps the
#' unit as a character field, which stays a flat vector for the long columns
#' this class exists to serve.
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
  unit <- vctrs::vec_recycle(as.character(unit), length(x), x_arg = "unit")
  substance <- vctrs::vec_recycle(as.character(substance), length(x),
                                  x_arg = "substance")

  bad_unit <- unique(unit[!vapply(unit, unit_is_defined, logical(1))])
  if (length(bad_unit))
    stop("unit(s) not recognised by udunits: ",
         paste0("\"", bad_unit, "\"", collapse = ", "),
         "\n  Normalising unit strings is out of scope for this package; see ",
         "units::install_unit() for genuinely missing symbols.", call. = FALSE)

  id <- substance_resolve(substance, system_obj)
  unknown <- unique(substance[is.na(id) & !is.na(substance)])
  if (length(unknown))
    stop("unknown substance(s) in system '", system_obj$name, "': ",
         paste0("\"", unknown, "\"", collapse = ", "), call. = FALSE)

  vctrs::new_rcrd(list(value = x, substance = id, unit = unit),
                  system = system_obj$name, class = "mixed_substances")
}

#' @export
format.mixed_substances <- function(x, ...) {
  sub <- vctrs::field(x, "substance")
  sub[is.na(sub)] <- "?"
  paste0(format(vctrs::field(x, "value"), ...), " [",
         vctrs::field(x, "unit"), "] ", sub)
}

#' @export
vec_ptype_abbr.mixed_substances <- function(x, ...) "mxsub"

#' @export
vec_ptype_full.mixed_substances <- function(x, ...) "mixed_substances"

#' @export
substance_of.mixed_substances <- function(x) vctrs::field(x, "substance")

## Per-element unit strings, the counterpart of the single `unit` attribute a
## homogeneous `substance` carries.
#' @export
substance_unit.mixed_substances <- function(x) vctrs::field(x, "unit")

## as.double(), not as.numeric(): as.numeric() dispatches through as.double(),
## so an as.numeric method is never reached and the vctrs default errors.
#' @export
as.double.mixed_substances <- function(x, ...) vctrs::field(x, "value")

#' Convert a mixed_substances vector to a single unit
#'
#' @param x A [mixed_substances] vector.
#' @param value The target unit.
#' @param ... Unused.
#' @param mode As in [units::set_units()].
#' @return A homogeneous [substance] vector.
#' @export
set_units.mixed_substances <- function(x, value, ...,
                                       mode = units::units_options("set_units_mode")) {
  if (missing(value)) stop("a target unit is required", call. = FALSE)
  else if (mode == "symbols") {
    value <- substitute(value)
    if (is.name(value) || is.call(value)) value <- format(value)
  }
  to_sym <- as_symbolic_units(value)
  system <- substance_system_of(x)
  values <- vctrs::field(x, "value")
  ids <- vctrs::field(x, "substance")
  units_chr <- vctrs::field(x, "unit")

  out <- numeric(length(values))
  for (u in unique(units_chr)) {
    rows <- units_chr == u
    part <- new_substance(values[rows], ids[rows], as_symbolic_units(u), system)
    out[rows] <- vctrs::field(convert_substance(part, to_sym), "value")
  }
  new_substance(out, ids, to_sym, system)
}

#' @export
#' @method vec_ptype2 mixed_substances
vec_ptype2.mixed_substances <- function(x, y, ...)
  UseMethod("vec_ptype2.mixed_substances", y)

#' @export
#' @method vec_ptype2.mixed_substances mixed_substances
vec_ptype2.mixed_substances.mixed_substances <- function(x, y, ...) {
  if (!identical(substance_system_of(x), substance_system_of(y)))
    stop("cannot combine `mixed_substances` from different systems", call. = FALSE)
  x
}

#' @export
#' @method vec_cast mixed_substances
vec_cast.mixed_substances <- function(x, to, ...)
  UseMethod("vec_cast.mixed_substances", x)

#' @export
#' @method vec_cast.mixed_substances mixed_substances
vec_cast.mixed_substances.mixed_substances <- function(x, to, ...) x
