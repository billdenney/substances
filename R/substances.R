#' Substance-aware quantities
#'
#' A vector of measurements that each carry the substance they measure. The
#' object is a [units::units] vector with one extra attribute -- the substance
#' of every element -- so everything `units` can do it can do, and the methods
#' in this package keep the substance aligned with the values.
#'
#' A different substance per element is the ordinary case, not an edge case:
#' long-format laboratory data has one analyte per row. Use
#' [mixed_substances()] when the *unit* also varies per element.
#'
#' @param x A numeric vector, a [units::units] vector, or a `substances` vector.
#' @param value A character vector of substance names, synonyms or identifiers,
#'   recycled to the length of `x`. `NA` means the substance is unknown, which
#'   permits dimensional conversion but not parametric.
#' @param unit A unit, as a string, a [units::units] object or `symbolic_units`.
#'   Applied before the substance. If `x` has no unit yet this labels it; if it
#'   already has one, this converts to `unit`.
#' @param system A conversion system name or object; defaults to
#'   [substance_default_system()], or to `x`'s own system if it has one.
#'
#' @return A `substances` vector, of class `c("substances", "units")`.
#'
#' @examples
#' set_substances(c(100, 140), "glucose", "mg/dL")
#' set_substances(c(100, 140), c("glucose", "sodium"), "mg/dL")
#'
#' x <- units::set_units(c(100, 140), "mg/dL")
#' substances(x) <- c("glucose", "sodium")
#' x
#' @export
set_substances <- function(x = double(), value = NA_character_, unit = NULL,
                           system = NULL) {
  if (is.null(system)) {
    system <- attr(x, "system")
  }
  system_obj <- get_system(system)

  ## A bare number has no unit to convert from, so `unit` labels it; a quantity
  ## does, so `unit` converts it -- and converting has to happen after the new
  ## substance is attached, because which factor applies depends on it.
  label_first <- !is.null(unit) && !inherits(x, "units")
  if (label_first) {
    x <- units::set_units(x, unit, mode = "standard")
  }
  if (!inherits(x, "units")) {
    x <- units::set_units(x, units::unitless, mode = "standard")
  }

  value <- recycle_to(as.character(value), length(x), "value")
  x <- new_substances(x, resolve_or_stop(value, system_obj), units(x),
                      system_obj$name)

  if (!is.null(unit) && !label_first) {
    x <- units::set_units(x, unit, mode = "standard")
  }
  x
}

#' @rdname set_substances
#' @export
`substances<-` <- function(x, value) set_substances(x, value)

#' @rdname set_substances
#' @export
substances <- function(x) UseMethod("substances")

#' @export
substances.substances <- function(x) {
  s <- attr(x, "substance")
  ## An operation that copied the attributes but not the values -- rbind() on a
  ## data frame is the one that does this -- leaves a vector whose substances no
  ## longer line up with its values. Every path into this package reads the
  ## substance through here, so checking once makes that corruption loud rather
  ## than letting it pick molar masses at random.
  if (length(s) != length(x)) {
    stop("this `substances` vector holds ", length(x), " values but ",
         length(s), " substances.\n  Something copied its attributes ",
         "without carrying the substances along, so which value is which ",
         "analyte is no longer known.", call. = FALSE)
  }
  s
}

#' @export
substances.default <- function(x) {
  stop("no `substances()` method for class ", paste(class(x), collapse = "/"),
       call. = FALSE)
}

#' @rdname set_substances
#' @export
substance_system_of <- function(x) attr(x, "system")

#' @rdname set_substances
#' @param substance_id,unit_sym,system_name Low-level constructor arguments; no
#'   checking is done.
#' @export
new_substances <- function(x = double(), substance_id = character(),
                           unit_sym = units::unitless,
                           system_name = substance_default_system()) {
  ## storage.mode(), not as.double(), so a named or dimensioned vector keeps
  ## its names and dim the way a `units` vector would.
  x <- bare_values(x)
  storage.mode(x) <- "double"
  structure(x, units = unit_sym, substance = substance_id,
            system = system_name, class = c("substances", "units"))
}

## Reattach our two attributes to whatever a units method handed back. The
## class vector is rebuilt rather than prepended to because `units` methods
## return a plain `units` object (see units:::.as.units).
reclass <- function(x, substance, system) {
  structure(x, substance = substance, system = system,
            class = c("substances", "units"))
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

## Recycle only from length 1, never from a shorter multiple: quietly repeating
## c("glucose", "sodium") across six values would label three of them wrongly.
recycle_to <- function(x, n, arg) {
  if (length(x) == n) {
    return(x)
  }
  if (length(x) == 1L) {
    return(rep(x, n))
  }
  stop("`", arg, "` has length ", length(x), ", which cannot be recycled to ",
       n, ".", call. = FALSE)
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

## Accepts either a substance vector or anything as_symbolic_units()
## understands, so callers can compare a vector's unit against a string.
unit_label <- function(x) {
  if (inherits(x, "units")) {
    sym <- units(x)
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

## ---- display ---------------------------------------------------------------

## The values with everything this package added stripped off, keeping the
## names and dim that base R would have kept.
bare_values <- function(x) {
  out <- unclass(x)
  attr(out, "units") <- NULL
  attr(out, "substance") <- NULL
  attr(out, "system") <- NULL
  out
}

#' The unit of every element, as character
#'
#' One string per element for a [mixed_substances] vector, one string for the
#' whole of a [substances] vector -- which is also what [units::units()] returns
#' for the latter, in `symbolic_units` form. Having it as a generic is what lets
#' one `format()` body serve both classes.
#'
#' @param x A [substances] or [mixed_substances] vector.
#' @return A character vector.
#' @examples
#' substance_units(set_substances(c(1, 2), "glucose", "mg/dL"))
#' substance_units(mixed_substances(c(1, 2), c("mg/dL", "mmol/L"), "glucose"))
#' @export
substance_units <- function(x) UseMethod("substance_units")

#' @export
substance_units.substances <- function(x) as.character(units(x))

#' @export
substance_units.default <- function(x) {
  stop("no `substance_units()` method for class ",
       paste(class(x), collapse = "/"), call. = FALSE)
}

## An unknown substance prints as "?" rather than NA, so it reads as a gap in
## the labelling rather than a missing measurement.
displayed_substances <- function(x) {
  sub <- substances(x)
  sub[is.na(sub)] <- "?"
  sub
}

## Value and substance, without the unit -- for contexts that show the unit
## somewhere else, such as a print header or a tibble column header.
labelled_values <- function(x, ...) {
  paste(format(unname(bare_values(x)), ...), displayed_substances(x))
}

format_substance_like <- function(x, ...) {
  paste0(format(unname(bare_values(x)), ...), " [", substance_units(x), "] ",
         displayed_substances(x))
}

#' @export
format.substances <- function(x, ...) {
  stats::setNames(format_substance_like(x, ...), names(x))
}

#' @export
print.substances <- function(x, ...) {
  ## Build the body before printing the header, so a corrupt vector reports
  ## that rather than a header followed by an error.
  body <- stats::setNames(labelled_values(x, ...), names(x))
  cat("<substances[", length(x), "]> ", as.character(units(x)), "\n", sep = "")
  print(body, quote = FALSE)
  invisible(x)
}

#' @export
str.substances <- function(object, ...) {
  cat(" Substances: [", as.character(units(object)), "] ",
      paste(unique(substances(object)), collapse = ", "), "\n", sep = "")
  utils::str(bare_values(object), ...)
}

#' @export
as.character.substances <- function(x, ...) format(x, ...)

## as.numeric() dispatches through as.double(), so an as.numeric.substances
## method would never be reached; defining only as.double() keeps both working.
#' @export
as.double.substances <- function(x, ...) as.vector(bare_values(x), "double")
