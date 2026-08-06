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
  if (anyNA(sx) || anyNA(sy)) {
    stop("cannot use `", op, "` when the substance is unknown", call. = FALSE)
  }
  n <- max(length(sx), length(sy))
  sx <- rep_len(sx, n)
    sy <- rep_len(sy, n)
  bad <- sx != sy
  if (any(bad)) {
    stop("cannot use `", op, "` on different substances: ",
         paste(unique(paste(sx[bad], "and", sy[bad])), collapse = "; "),
         call. = FALSE)
  }
  invisible(TRUE)
}

## Bring `y` into `x`'s unit, substance-aware, with a clearer error than the
## conversion machinery gives on its own.
align_units <- function(x, y, op) {
  if (identical(unit_label(x), unit_label(y))) {
    return(y)
  }
  tryCatch(convert_substance(y, substance_unit(x)),
           error = function(e) {
             stop("cannot use `", op, "` on ", unit_label(x), " and ",
                  unit_label(y), ": ", conditionMessage(e), call. = FALSE)
           })
}

#' @export
#' @method vec_arith substance
vec_arith.substance <- function(op, x, y, ...) UseMethod("vec_arith.substance", y)

#' @export
#' @method vec_arith.substance default
vec_arith.substance.default <- function(op, x, y, ...) {
  vctrs::stop_incompatible_op(op, x, y)
}

## Unary operators only. vctrs dispatches here exactly when `y` is its MISSING
## sentinel, which it passes only from the unary forms of `Ops`; a binary `x - y`
## reaches vec_arith.substance.substance or .numeric instead. So `op` needs no
## arity check, only a whitelist -- `!x` also arrives here, with op = "!", and
## falls through to the error.
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
  if (!identical(substance_system_of(x), substance_system_of(y))) {
    stop("cannot use `", op, "` on `substance` vectors from different systems: '",
         substance_system_of(x), "' and '", substance_system_of(y), "'.",
         call. = FALSE)
  }
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

## ---- interoperating with plain `units` quantities --------------------------
##
## Concentration times volume is an amount, still of the same substance. Without
## help, R refuses to evaluate it: `substance` and `units` carry operator
## methods from different classes, and R will not choose between them, so
## `x * units::set_units(3, "L")` fails with "Incompatible methods" before any
## method of ours runs.
##
## chooseOpsMethod() (R >= 4.3.0) is the supported way to break that tie. It is
## called with `x` always bound to our object and `y` to the other operand;
## `reverse` says which side ours was on. We claim the tie only for `units`,
## the one conflict we can service -- any other clash keeps R's default
## behaviour rather than being silently captured by us.

#' @export
chooseOpsMethod.substance <- function(x, y, mx, my, cl, reverse) inherits(y, "units")

#' @export
#' @method vec_arith.substance units
vec_arith.substance.units <- function(op, x, y, ...) {
  substance_arith_units(op, x, y, reverse = FALSE)
}

## The reverse direction, `units * substance`, dispatches vec_arith() on the
## units object, so it needs a vec_arith.units to route from. vctrs ships these
## for the base classes (numeric, Date, difftime, ...) but not for units, and it
## belongs here rather than upstream: hosting it in units would oblige that
## package to depend on vctrs for this one method and nothing else. The default
## below defers to vctrs' own error, so units gains no behaviour it did not
## already have -- units-to-units arithmetic never reaches vec_arith at all.

#' @export
#' @method vec_arith units
vec_arith.units <- function(op, x, y, ...) UseMethod("vec_arith.units", y)

#' @export
#' @method vec_arith.units default
vec_arith.units.default <- function(op, x, y, ...) {
  vctrs::stop_incompatible_op(op, x, y)
}

#' @export
#' @method vec_arith.units substance
vec_arith.units.substance <- function(op, x, y, ...) {
  substance_arith_units(op, y, x, reverse = TRUE)
}

## `s` is the substance operand and `q` the units one; `reverse` says whether
## `q` came first in the expression.
substance_arith_units <- function(op, s, q, reverse) {
  if (!op %in% c("*", "/")) {
    stop("cannot use `", op, "` on a `substance` and a bare `units` quantity: ",
         "only `*` and `/` are defined, because a quantity with no substance ",
         "cannot be added to or compared with one that has a substance.",
         "\n  Give the other operand a substance, or use drop_substance().",
         call. = FALSE)
  }
  sq <- drop_substance(s)
  combined <- if (reverse) get(op, envir = baseenv())(q, sq)
              else get(op, envir = baseenv())(sq, q)
  new_substance(as.numeric(combined),
                vctrs::vec_recycle(vctrs::field(s, "substance"),
                                   length(combined)),
                units(combined), substance_system_of(s))
}

#' @export
sum.substance <- function(..., na.rm = FALSE) {
  x <- vctrs::vec_c(...)
  ids <- unique(stats::na.omit(vctrs::field(x, "substance")))
  if (length(ids) != 1L) {
    stop("cannot sum a `substance` vector holding ", length(ids),
         " substances; split by substance first.", call. = FALSE)
  }
  new_substance(sum(vctrs::field(x, "value"), na.rm = na.rm), ids,
                substance_unit(x), substance_system_of(x))
}

#' @export
mean.substance <- function(x, ..., na.rm = FALSE) {
  ids <- unique(stats::na.omit(vctrs::field(x, "substance")))
  if (length(ids) != 1L) {
    stop("cannot average a `substance` vector holding ", length(ids),
         " substances; split by substance first.", call. = FALSE)
  }
  new_substance(mean(vctrs::field(x, "value"), na.rm = na.rm), ids,
                substance_unit(x), substance_system_of(x))
}
