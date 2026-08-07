#' Substance-aware quantities
#'
#' A vector of measurements that each carry the substance they measure. The
#' substance is a *field*, so it survives subsetting, concatenation, reordering
#' and data-frame operations; the unit is a single attribute for the whole
#' vector, exactly as in [units::units].
#'
#' A different substance per element is the ordinary case, not an edge case:
#' long-format laboratory data has one analyte per row. Use
#' [mixed_substances()] when the *unit* also varies per element.
#'
#' @param x A numeric vector.
#' @param unit A unit, as a string, a [units::units] object or `symbolic_units`.
#' @param substance A character vector of substance names, synonyms or
#'   identifiers, recycled to the length of `x`. `NA` means the substance is
#'   unknown, which permits dimensional conversion but not parametric.
#' @param system A conversion system name or object; defaults to
#'   [substance_default_system()].
#'
#' @return A `substance` vector.
#'
#' @examples
#' substance(c(100, 140), "mg/dL", "glucose")
#' substance(c(100, 140), "mg/dL", c("glucose", "sodium"))
#' @export
substance <- function(x = double(), unit = units::unitless,
                      substance = NA_character_, system = NULL) {
  system_obj <- get_system(system)
  x <- as.double(x)
  substance <- vctrs::vec_recycle(as.character(substance), length(x),
                                  x_arg = "substance")
  id <- resolve_or_stop(substance, system_obj)
  new_substance(x, id, as_symbolic_units(unit), system_obj$name)
}

## Resolve names to ids, refusing any that the system does not know. Every
## constructor goes through this so the same mistake reads the same way
## whichever door it came through.
resolve_or_stop <- function(x, system) {
  id <- substance_resolve(x, system)
  unknown <- unique(x[is.na(id) & !is.na(x)])
  if (length(unknown)) {
    stop("unknown substance(s) in system '", system$name, "': ",
         paste0("\"", unknown, "\"", collapse = ", "),
         "\n  See substance_systems() and substance_resolve().", call. = FALSE)
  }
  id
}

#' @rdname substance
#' @param value,substance_id,unit_sym,system_name Low-level constructor
#'   arguments; no checking is done.
#' @export
new_substance <- function(value = double(), substance_id = character(),
                          unit_sym = units::unitless,
                          system_name = substance_default_system()) {
  vctrs::new_rcrd(list(value = value, substance = substance_id),
                  unit = unit_sym, system = system_name, class = "substance")
}

## Reject unit strings udunits does not know, with a message that says whose
## job normalising them is. Only the distinct strings need checking: a long
## column repeats a handful of units thousands of times.
check_units_defined <- function(unit) {
  candidates <- unique(unit[!is.na(unit)])
  bad <- candidates[!vapply(candidates, unit_is_defined, logical(1))]
  if (length(bad)) {
    stop("unit(s) not recognised by udunits: ",
         paste0("\"", bad, "\"", collapse = ", "),
         "\n  Normalising unit strings is out of scope for this package; see ",
         "units::install_unit() for genuinely missing symbols.", call. = FALSE)
  }
  invisible(unit)
}

as_symbolic_units <- function(unit) {
  if (inherits(unit, "symbolic_units")) {
    return(unit)
  }
  if (inherits(unit, "units")) {
    return(units(unit))
  }
  units(units::as_units(as.character(unit)))
}

#' Accessors for substance vectors
#'
#' @param x A `substance` vector.
#' @param value Replacement value.
#' @return `substance_of()` a character vector of substance identifiers;
#'   `substance_unit()` a `symbolic_units`; `drop_substance()` a
#'   [units::units] vector; `substance_system_of()` the system name.
#' @examples
#' x <- substance(c(100, 140), "mg/dL", c("glucose", "sodium"))
#' substance_of(x)
#' drop_substance(x)
#' @export
substance_of <- function(x) UseMethod("substance_of")

#' @export
substance_of.substance <- function(x) vctrs::field(x, "substance")

#' @export
substance_of.default <- function(x) {
  stop("no `substance_of()` method for class ",
       paste(class(x), collapse = "/"), call. = FALSE)
}

#' @rdname substance_of
#' @export
`substance_of<-` <- function(x, value) {
  stopifnot(inherits(x, "substance"))
  value <- vctrs::vec_recycle(as.character(value), length(x))
  vctrs::field(x, "substance") <-
    resolve_or_stop(value, get_system(substance_system_of(x)))
  x
}

#' @rdname substance_of
#' @export
substance_unit <- function(x) UseMethod("substance_unit")

#' @export
substance_unit.substance <- function(x) attr(x, "unit")

#' @export
substance_unit.default <- function(x) {
  stop("no `substance_unit()` method for class ",
       paste(class(x), collapse = "/"), call. = FALSE)
}

#' @rdname substance_of
#' @export
substance_system_of <- function(x) attr(x, "system")

#' @rdname substance_of
#' @export
drop_substance <- function(x) {
  stopifnot(inherits(x, "substance"))
  out <- vctrs::field(x, "value")
  units(out) <- substance_unit(x)
  out
}

## Accepts either a substance vector or anything as_symbolic_units() understands,
## so callers can compare a vector's unit against a plain string.
unit_label <- function(x) {
  if (inherits(x, "substance")) {
    sym <- substance_unit(x)
  } else {
    sym <- as_symbolic_units(x)
  }
  as.character(sym)
}

## unit_label() over a character vector of unit strings.
unit_labels <- function(x) {
  vapply(x, function(z) {
    tryCatch(unit_label(z), error = function(e) NA_character_)
  }, character(1), USE.NAMES = FALSE)
}

## Shared by both classes: substance_unit() is the generic that abstracts where
## the unit comes from, so one body covers a single unit and a per-element one.
format_substance_like <- function(x, ...) {
  sub <- vctrs::field(x, "substance")
  sub[is.na(sub)] <- "?"
  paste0(format(vctrs::field(x, "value"), ...), " [",
         as.character(substance_unit(x)), "] ", sub)
}

#' @export
format.substance <- function(x, ...) format_substance_like(x, ...)

#' @export
vec_ptype_abbr.substance <- function(x, ...) "subst"

#' @export
vec_ptype_full.substance <- function(x, ...) paste0("substance<", unit_label(x), ">")

#' @export
obj_print_header.substance <- function(x, ...) {
  cat("<", vctrs::vec_ptype_full(x), "[", length(x), "]>\n", sep = "")
  invisible(x)
}

#' @export
as.character.substance <- function(x, ...) format(x, ...)

## as.numeric() dispatches through as.double(), so an as.numeric.substance
## method would never be reached; defining only as.double() keeps both working.
#' @export
as.double.substance <- function(x, ...) vctrs::field(x, "value")

#' @export
units.substance <- function(x) substance_unit(x)

## ---- combination rules -----------------------------------------------------
## Combining is allowed only when the unit and the system match. Different
## substances within a vector are fine -- that is the point -- but silently
## reinterpreting one unit as another, or mixing registries, is not.
##
## Ordering and equality use the vctrs record defaults, which compare the value
## field first and the substance second; there is no method here for either.

#' @export
#' @method vec_ptype2 substance
vec_ptype2.substance <- function(x, y, ...) UseMethod("vec_ptype2.substance", y)

#' @export
#' @method vec_ptype2.substance default
vec_ptype2.substance.default <- function(x, y, ..., x_arg = "", y_arg = "") {
  vctrs::stop_incompatible_type(x, y, x_arg = x_arg, y_arg = y_arg)
}

#' @export
#' @method vec_ptype2.substance substance
vec_ptype2.substance.substance <- function(x, y, ...) {
  if (!identical(unit_label(x), unit_label(y))) {
    stop("cannot combine `substance` vectors with different units: ",
         unit_label(x), " and ", unit_label(y),
         "\n  Convert one with set_units() first.", call. = FALSE)
  }
  if (!identical(substance_system_of(x), substance_system_of(y))) {
    stop("cannot combine `substance` vectors from different systems: '",
         substance_system_of(x), "' and '", substance_system_of(y), "'.",
         call. = FALSE)
  }
  x
}

#' @export
#' @method vec_cast substance
vec_cast.substance <- function(x, to, ...) UseMethod("vec_cast.substance", x)

#' @export
#' @method vec_cast.substance default
vec_cast.substance.default <- function(x, to, ..., x_arg = "", to_arg = "") {
  vctrs::stop_incompatible_cast(x, to, x_arg = x_arg, to_arg = to_arg)
}

#' @export
#' @method vec_cast.substance substance
vec_cast.substance.substance <- function(x, to, ...) x
