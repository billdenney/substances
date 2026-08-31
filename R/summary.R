## Reductions and elementwise maths. Both delegate to `units` and then decide
## what the answer is a quantity of: a reduction collapses many elements into
## one, so it needs them all to be the same substance; an elementwise map keeps
## the length, so each element keeps its own.

#' Summaries of a substance vector
#'
#' `sum()`, `min()`, `max()`, `range()`, `mean()`, `median()`, `quantile()` and
#' `weighted.mean()` collapse many elements into one, which only means something
#' if every element is the same substance. An unknown substance is refused
#' rather than dropped: it is not evidence that the rest are alike.
#'
#' @param x,... [substances] vectors.
#' @param na.rm Passed to the underlying function.
#' @return A `substances` vector of length one, or of `length(probs)` for
#'   `quantile()`.
#' @name Summary.substances
#' @examples
#' x <- set_substances(c(100, 140, 90), "glucose", "mg/dL")
#' sum(x)
#' mean(x)
#' range(x)
#' @export
Summary.substances <- function(..., na.rm = FALSE) {
  args <- list(...)
  if (length(args) > 1L) {
    x <- do.call(c, args)
  } else {
    x <- args[[1L]]
  }
  id <- one_substance_or_stop(x, .Generic)
  out <- do.call(.Generic, c(list(drop_substances(x)), na.rm = na.rm))
  reclass(out, rep(id, length(out)), substance_system_of(x))
}

#' @rdname Summary.substances
#' @export
mean.substances <- function(x, ...) substance_reduce(x, mean, "mean", ...)

#' @rdname Summary.substances
#' @export
median.substances <- function(x, ...) {
  substance_reduce(x, stats::median, "median", ...)
}

#' @rdname Summary.substances
#' @export
quantile.substances <- function(x, ...) {
  substance_reduce(x, stats::quantile, "quantile", ...)
}

#' @rdname Summary.substances
#' @param w Weights, as in [stats::weighted.mean()].
#' @export
weighted.mean.substances <- function(x, w, ...) {
  substance_reduce(x, stats::weighted.mean, "weighted mean", w = w, ...)
}

substance_reduce <- function(x, f, verb, ...) {
  id <- one_substance_or_stop(x, verb)
  out <- f(drop_substances(x), ...)
  reclass(out, rep(id, length(out)), substance_system_of(x))
}

#' Elementwise maths on a substance vector
#'
#' Whatever [units::units] permits, applied elementwise, with each element
#' keeping its own substance.
#'
#' @param x A [substances] vector.
#' @param ... Passed to the underlying function.
#' @return A `substances` vector, or a plain numeric one where `units` returns
#'   a number (`sign()`).
#' @examples
#' abs(set_substances(c(-1, 2), "glucose", "mmol/L"))
#' cumsum(set_substances(c(1, 2, 3), "glucose", "mmol/L"))
#' @export
Math.substances <- function(x, ...) {
  out <- NextMethod()
  ## `units` answers some of these with a bare number (sign) and refuses others
  ## with a warning, dropping the unit; where it kept a quantity, the result is
  ## still elementwise and still of the same substances.
  if (!inherits(out, "units") || length(out) != length(x)) {
    return(out)
  }
  reclass(out, substances(x), substance_system_of(x))
}
