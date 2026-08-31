## Arithmetic and comparison on substance vectors.
##
## The governing rule is that a quantity of one substance is not a quantity of
## another. Addition and comparison require the substances to match elementwise;
## division by the same substance cancels it; multiplication by a substance-free
## quantity carries it through. Anything else is an error, deliberately: this is
## easy to loosen later and painful to tighten.
##
## `units` does the unit algebra, reached through NextMethod(); this method
## decides what the result is a quantity *of*.

#' Arithmetic and comparison for substance vectors
#'
#' @param e1,e2 A [substances] vector, a [units::units] quantity or a number.
#' @return A `substances` vector, a `units` quantity where the substance
#'   cancels, or a logical vector for comparisons.
#' @name groupGeneric.substances
#' @examples
#' glc <- set_substances(c(100, 140), "glucose", "mg/dL")
#' glc + set_substances(10, "glucose", "mg/dL")
#' glc * 2
#' glc > set_substances(5, "glucose", "mmol/L")
#' glc * units::set_units(3, "L")
#' @export
Ops.substances <- function(e1, e2) {
  if (missing(e2)) {
    if (!.Generic %in% c("+", "-")) {
      stop("cannot use unary `", .Generic, "` on a `substances` vector.",
           call. = FALSE)
    }
    return(reclass(NextMethod(), substances(e1), substance_system_of(e1)))
  }

  ## Both operand orders reach this method, so the guards name the operands by
  ## role. The operations that care which side each was written on -- division,
  ## which does not commute -- keep using e1 and e2.
  both <- inherits(e1, "substances") && inherits(e2, "substances")
  if (inherits(e1, "substances")) {
    ours <- e1
    other <- e2
  } else {
    ours <- e2
    other <- e1
  }
  if (both) {
    same_system_or_stop(e1, e2, .Generic)
  }
  system <- substance_system_of(ours)

  if (.Generic %in% c("==", "!=", "<", ">", "<=", ">=")) {
    require_both(other, .Generic, both)
    same_substance_or_stop(e1, e2, .Generic)
    e2 <- align_units(e1, e2, .Generic)
    return(NextMethod())
  }

  if (.Generic %in% c("+", "-")) {
    require_both(other, .Generic, both)
    same_substance_or_stop(e1, e2, .Generic)
    e2 <- align_units(e1, e2, .Generic)
    out <- NextMethod()
    return(reclass(out, recycle_to(substances(e1), length(out), "substance"),
                   system))
  }

  if (both) {
    if (.Generic == "/") {
      ## The substance cancels, leaving an ordinary units quantity.
      same_substance_or_stop(e1, e2, .Generic)
      return(drop_substances(e1) / drop_substances(e2))
    }
    stop("cannot use `", .Generic, "` on two `substances` vectors: the result ",
         "would be a quantity of substance squared, which has no meaning here.",
         call. = FALSE)
  }

  if (.Generic %in% c("*", "/")) {
    ## Concentration times volume is an amount, still of the same substance.
    scalable_or_stop(other, .Generic)
    out <- NextMethod()
    return(reclass(out, recycle_to(substances(ours), length(out), "substance"),
                   system))
  }

  if (.Generic == "^") {
    stop("cannot raise a `substances` vector to a power: the unit would ",
         "change but the substance parameters would not follow.",
         call. = FALSE)
  }
  stop("cannot use `", .Generic, "` on a `substances` vector.", call. = FALSE)
}

## Adding or comparing across the substance boundary is refused rather than
## quietly treating the other operand as "the same substance".
require_both <- function(other, op, both) {
  if (both) {
    return(invisible(TRUE))
  }
  if (inherits(other, "units")) {
    stop("cannot use `", op, "` on a `substances` vector and a bare `units` ",
         "quantity: a quantity with no substance cannot be added to or ",
         "compared with one that has a substance.",
         "\n  Give the other operand a substance with set_substances(), or ",
         "use drop_substances().", call. = FALSE)
  }
  stop("cannot use `", op, "` on a `substances` vector and a bare number: a ",
       "number has no unit.\n  Use units::set_units() to give it one.",
       call. = FALSE)
}

## Scaling is defined against a number or a plain `units` quantity. Anything
## else -- a mixed_substances vector is the one that gets here -- has no single
## unit for `units` to do the algebra with, so it would produce a result whose
## unit attribute meant nothing.
scalable_or_stop <- function(x, op) {
  ## is.object(), not just is.numeric(): a mixed_substances vector *is* a
  ## numeric one underneath, so only a classless number qualifies.
  if (inherits(x, "units") || (is.numeric(x) && !is.object(x))) {
    return(invisible(TRUE))
  }
  stop("cannot use `", op, "` on a `substances` vector and a ",
       paste(class(x), collapse = "/"), ".", call. = FALSE)
}

same_system_or_stop <- function(e1, e2, op) {
  if (!identical(substance_system_of(e1), substance_system_of(e2))) {
    stop("cannot use `", op, "` on `substances` vectors from different ",
         "systems: '", substance_system_of(e1), "' and '",
         substance_system_of(e2), "'.", call. = FALSE)
  }
  invisible(TRUE)
}

same_substance_or_stop <- function(e1, e2, op) {
  sx <- substances(e1)
  sy <- substances(e2)
  if (anyNA(sx) || anyNA(sy)) {
    stop("cannot use `", op, "` when the substance is unknown", call. = FALSE)
  }
  ## Recycle by the same rule the arithmetic that follows will use, so the
  ## guard cannot accept a pair of lengths the operation then rejects.
  n <- max(length(sx), length(sy))
  sx <- recycle_to(sx, n, "substance")
  sy <- recycle_to(sy, n, "substance")
  bad <- sx != sy
  if (any(bad)) {
    stop("cannot use `", op, "` on different substances: ",
         paste(unique(paste(sx[bad], "and", sy[bad])), collapse = "; "),
         call. = FALSE)
  }
  invisible(TRUE)
}

## Bring `e2` into `e1`'s unit, substance-aware, with a clearer error than the
## conversion machinery gives on its own. `units` would align them too, but only
## dimensionally, so mg/dL + mmol/L would fail there.
align_units <- function(e1, e2, op) {
  if (identical(unit_label(e1), unit_label(e2))) {
    return(e2)
  }
  tryCatch(convert_substance(e2, units(e1)),
           error = function(e) {
             stop("cannot use `", op, "` on ", unit_label(e1), " and ",
                  unit_label(e2), ": ", conditionMessage(e), call. = FALSE)
           })
}

## ---- interoperating with plain `units` quantities --------------------------
##
## Concentration times volume is a normal thing to want, and R will not evaluate
## it unaided: `substances` and `units` carry operator methods from different
## classes, and R refuses to choose between them. Inheriting from `units` does
## not settle it, because the conflict is between the two *methods*, not the two
## classes -- R warns "Incompatible methods" and falls back to the internal
## default, which returns the values with e1's unit unchanged. That is a wrong
## answer rather than an error: 2 mmol/L * 3 L comes back as 6 mmol/L.
##
## chooseOpsMethod() (R >= 4.3.0) is the supported way to break the tie. It is
## called with `x` always bound to our object and `y` to the other operand;
## returning TRUE selects our method. We claim the tie only for `units`, the one
## conflict we can service -- any other clash keeps R's default behaviour rather
## than being silently captured by us.

#' @rdname groupGeneric.substances
#' @param x,y,mx,my,cl,reverse As in [base::chooseOpsMethod()].
#' @export
chooseOpsMethod.substances <- function(x, y, mx, my, cl, reverse) {
  inherits(y, "units")
}
