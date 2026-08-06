## Arithmetic on substance vectors.
##
## The governing rule is that a quantity of one substance is not a quantity of
## another. Addition and comparison require the substances to match elementwise;
## division by the same substance cancels it; multiplication by a substance-free
## quantity carries it through. Anything else is an error, deliberately: this is
## easy to loosen later and painful to tighten.

same_substance_or_stop <- function(x, y, op) {
  sx <- vctrs::field(x, "substance")
  sy <- vctrs::field(y, "substance")
  if (anyNA(sx) || anyNA(sy))
    stop("cannot use `", op, "` when the substance is unknown", call. = FALSE)
  n <- max(length(sx), length(sy))
  sx <- rep_len(sx, n); sy <- rep_len(sy, n)
  bad <- sx != sy
  if (any(bad))
    stop("cannot use `", op, "` on different substances: ",
         paste(unique(paste(sx[bad], "and", sy[bad])), collapse = "; "),
         "\n  Convert one to the other first; cross-substance conversion is not ",
         "implemented yet.", call. = FALSE)
  invisible(TRUE)
}

## Bring `y` into `x`'s unit, substance-aware, with a clearer error than the
## conversion machinery gives on its own.
align_units <- function(x, y, op) {
  if (identical(unit_label(x), unit_label(y))) return(y)
  tryCatch(convert_substance(y, substance_unit(x)),
           error = function(e)
             stop("cannot use `", op, "` on ", unit_label(x), " and ",
                  unit_label(y), ": ", conditionMessage(e), call. = FALSE))
}

#' @export
#' @method vec_arith substance
vec_arith.substance <- function(op, x, y, ...) UseMethod("vec_arith.substance", y)

#' @export
#' @method vec_arith.substance default
vec_arith.substance.default <- function(op, x, y, ...) {
  # R refuses to dispatch `substance <op> units` at all, so this branch is only
  # reached when vec_arith() is called directly; point at the way that works.
  if (inherits(y, "units"))
    stop("use substance_scale() to combine a `substance` with a `units` ",
         "quantity; `", op, "` cannot dispatch between the two classes.",
         call. = FALSE)
  vctrs::stop_incompatible_op(op, x, y)
}

#' @export
#' @method vec_arith.substance MISSING
vec_arith.substance.MISSING <- function(op, x, y, ...) {
  switch(op,
    `-` = new_substance(-vctrs::field(x, "value"), vctrs::field(x, "substance"),
                        substance_unit(x), substance_system_of(x)),
    `+` = x,
    vctrs::stop_incompatible_op(op, x, y))
}

#' @export
#' @method vec_arith.substance substance
vec_arith.substance.substance <- function(op, x, y, ...) {
  if (!identical(substance_system_of(x), substance_system_of(y)))
    stop("cannot use `", op, "` on `substance` vectors from different systems: '",
         substance_system_of(x), "' and '", substance_system_of(y), "'.",
         call. = FALSE)
  switch(op,
    `+` = ,
    `-` = {
      same_substance_or_stop(x, y, op)
      y <- align_units(x, y, op)
      new_substance(vctrs::vec_arith_base(op, vctrs::field(x, "value"),
                                          vctrs::field(y, "value")),
                    vctrs::vec_recycle_common(vctrs::field(x, "substance"),
                                              vctrs::field(y, "substance"))[[1L]],
                    substance_unit(x), substance_system_of(x))
    },
    `/` = {
      # the substance cancels, leaving an ordinary units quantity
      same_substance_or_stop(x, y, op)
      drop_substance(x) / drop_substance(y)
    },
    `*` = stop("cannot multiply two `substance` vectors: the result would be a ",
               "quantity of substance squared, which has no meaning here.",
               call. = FALSE),
    vctrs::stop_incompatible_op(op, x, y))
}

#' @export
#' @method vec_arith.substance numeric
vec_arith.substance.numeric <- function(op, x, y, ...) {
  switch(op,
    `*` = ,
    `/` = new_substance(vctrs::vec_arith_base(op, vctrs::field(x, "value"), y),
                        vctrs::field(x, "substance"), substance_unit(x),
                        substance_system_of(x)),
    `^` = stop("cannot raise a `substance` to a power: the unit would change ",
               "but the substance parameters would not follow.", call. = FALSE),
    stop("cannot use `", op, "` on a `substance` and a bare number: a number ",
         "has no unit.\n  Use set_units() to give it one.", call. = FALSE))
}

#' @export
#' @method vec_arith.numeric substance
vec_arith.numeric.substance <- function(op, x, y, ...) {
  switch(op,
    `*` = new_substance(x * vctrs::field(y, "value"), vctrs::field(y, "substance"),
                        substance_unit(y), substance_system_of(y)),
    stop("cannot use `", op, "` on a bare number and a `substance`.",
         call. = FALSE))
}

#' Multiply a substance quantity by a units quantity
#'
#' Concentration times volume is an amount, still of the same substance. This
#' needs its own function because `*` cannot be made to work: R refuses to
#' dispatch a binary operator when both operands carry methods from different
#' classes, so `x * units::set_units(3, "L")` fails with "Incompatible methods"
#' before any method of ours is reached. No S3 arrangement avoids that -- the
#' only thing that does is inheriting from `units`, which silently returns a
#' plain `units` object with the substance dropped, and is precisely what this
#' class exists to prevent.
#'
#' Multiplying by a plain number uses `*` as usual; only `units` quantities
#' need this function. For division, multiply by the reciprocal.
#'
#' @param x A [substance] vector.
#' @param by A [units::units] quantity, or a plain number.
#'
#' @return A `substance` vector with the combined unit.
#'
#' @examples
#' conc <- substance(2, "mmol/L", "glucose")
#' substance_scale(conc, units::set_units(3, "L"))       # -> mmol of glucose
#' substance_scale(conc, 1 / units::set_units(3, "L"))   # divide by a volume
#' @export
substance_scale <- function(x, by) {
  stopifnot(inherits(x, "substance"))
  if (!inherits(by, "units")) return(x * as.numeric(by))
  combined <- unit_quantity(x) * by
  new_substance(vctrs::field(x, "value") * as.numeric(combined),
                vctrs::field(x, "substance"),
                units(combined), substance_system_of(x))
}

#' @export
sum.substance <- function(..., na.rm = FALSE) {
  x <- vctrs::vec_c(...)
  ids <- unique(stats::na.omit(vctrs::field(x, "substance")))
  if (length(ids) != 1L)
    stop("cannot sum a `substance` vector holding ", length(ids),
         " substances; split by substance first.", call. = FALSE)
  new_substance(sum(vctrs::field(x, "value"), na.rm = na.rm), ids,
                substance_unit(x), substance_system_of(x))
}

#' @export
mean.substance <- function(x, ..., na.rm = FALSE) {
  ids <- unique(stats::na.omit(vctrs::field(x, "substance")))
  if (length(ids) != 1L)
    stop("cannot average a `substance` vector holding ", length(ids),
         " substances; split by substance first.", call. = FALSE)
  new_substance(mean(vctrs::field(x, "value"), na.rm = na.rm), ids,
                substance_unit(x), substance_system_of(x))
}
