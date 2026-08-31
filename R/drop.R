#' Drop the substance, or the units, from a substance vector
#'
#' `drop_substances()` returns the plain [units::units] quantity, keeping the
#' unit and discarding which analyte each element measured.
#' [units::drop_units()] goes further and returns a bare numeric vector: a
#' substance with no unit is not something this package represents, so dropping
#' the unit drops the substance with it.
#'
#' @param x A [substances] vector.
#' @return `drop_substances()` a `units` vector; `drop_units()` a numeric one.
#' @examples
#' x <- set_substances(c(100, 140), c("glucose", "sodium"), "mg/dL")
#' drop_substances(x)
#' units::drop_units(x)
#' @export
drop_substances <- function(x) UseMethod("drop_substances")

#' @export
drop_substances.substances <- function(x) {
  attr(x, "substance") <- NULL
  attr(x, "system") <- NULL
  class(x) <- "units"
  x
}

#' @export
drop_substances.default <- function(x) {
  stop("no `drop_substances()` method for class ",
       paste(class(x), collapse = "/"), call. = FALSE)
}

#' @export
drop_units.substances <- function(x) {
  ## drop_units.units() removes only "units" from the class vector, so this has
  ## to take "substances" off itself or the result claims to be something it no
  ## longer is.
  x <- drop_substances(x)
  units::drop_units(x)
}
