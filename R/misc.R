## Keeping the substance aligned with the values.
##
## Every method here has the same shape: let the `units` method do the work
## through NextMethod(), then put the substance back, indexed the same way the
## values were. The substance is subset with the caller's own `...`, exactly as
## errors::`[.errors` does, so a matrix or an array indexes correctly without a
## separate code path.

#' Extract or replace parts of a substance vector
#'
#' @param x A [substances] vector.
#' @param ... Index arguments, as in [base::Extract].
#' @param value Replacement value.
#' @return A `substances` vector.
#' @name Extract.substances
#' @examples
#' x <- set_substances(c(100, 140, 5), c("glucose", "sodium", "urea"), "mg/dL")
#' x[2:3]
#' x[[1]]
#' x[1] <- set_substances(120, "glucose", "mg/dL")
#' x
#' @export
`[.substances` <- function(x, ...) {
  s <- substances(x)
  dim(s) <- dim(x)
  reclass(NextMethod(), as.character(s[...]), substance_system_of(x))
}

#' @rdname Extract.substances
#' @export
`[[.substances` <- `[.substances`

#' @rdname Extract.substances
#' @export
`[<-.substances` <- function(x, ..., value) {
  s <- substances(x)
  dim(s) <- dim(x)
  s[...] <- substances_of_replacement(value, x, s[...])
  ## Assigning a quantity in another unit has to convert before the values are
  ## written, and which factor applies depends on the incoming substance, so it
  ## is done here rather than left to `units`.
  if (inherits(value, "substances")) {
    value <- convert_substance(value, units(x))
  }
  dim(s) <- NULL
  reclass(NextMethod(), as.character(s), substance_system_of(x))
}

#' @rdname Extract.substances
#' @export
`[[<-.substances` <- `[<-.substances`

## What substance the replacement carries. A bare number or a plain units
## quantity has none, and guessing would be how the wrong molar mass gets in,
## so it has to be said explicitly. Bare NA is the exception: blanking a result
## says nothing about which analyte the row was, so the substance stays.
substances_of_replacement <- function(value, x, current) {
  if (identical(value, NA)) {
    return(current)
  }
  if (inherits(value, "substances")) {
    if (!identical(substance_system_of(value), substance_system_of(x))) {
      stop("cannot assign a `substances` vector from system '",
           substance_system_of(value), "' into one from '",
           substance_system_of(x), "'.", call. = FALSE)
    }
    return(substances(value))
  }
  stop("cannot assign a ", paste(class(value), collapse = "/"),
       " into a `substances` vector: it does not say which substance it is.",
       "\n  Wrap it with set_substances() first.", call. = FALSE)
}

#' Combine substance vectors
#'
#' Arguments after the first are converted to the first one's unit, the
#' substance-aware way, exactly as [base::c()] on [units::units] vectors
#' converts to the first one's unit dimensionally.
#'
#' @param ... [substances] vectors.
#' @param recursive Ignored, for compatibility with [base::c()].
#' @return A `substances` vector.
#' @examples
#' c(set_substances(100, "glucose", "mg/dL"),
#'   set_substances(10, "glucose", "mmol/L"))
#' @export
c.substances <- function(..., recursive = FALSE) {
  args <- list(...)
  args[vapply(args, is.null, logical(1))] <- NULL
  bad <- !vapply(args, inherits, logical(1), "substances")
  if (any(bad)) {
    stop("cannot combine a `substances` vector with a ",
         paste(class(args[which(bad)[1L]][[1L]]), collapse = "/"),
         ": it does not say which substance it is.", call. = FALSE)
  }
  system <- substance_system_of(args[[1L]])
  differing <- vapply(args, substance_system_of, character(1)) != system
  if (any(differing)) {
    stop("cannot combine `substances` vectors from different systems: '",
         system, "' and '",
         substance_system_of(args[[which(differing)[1L]]]), "'.", call. = FALSE)
  }

  unit <- units(args[[1L]])
  args <- lapply(args, convert_substance, unit)
  new_substances(unlist(lapply(args, bare_values)),
                 unlist(lapply(args, substances)), unit, system)
}

#' Replicate a substance vector
#'
#' @param x A [substances] vector.
#' @param ... Passed to [base::rep()].
#' @return A `substances` vector.
#' @examples
#' rep(set_substances(100, "glucose", "mg/dL"), 3)
#' @export
rep.substances <- function(x, ...) {
  reclass(NextMethod(), rep(substances(x), ...), substance_system_of(x))
}

## Two elements are the same only if both the value and the substance agree;
## 5 mmol/L of glucose is not 5 mmol/L of urea. Comparing them as a data frame
## gets that for free and stays exact for doubles.
as_comparable <- function(x) {
  data.frame(value = as.vector(bare_values(x), "double"),
             substance = substances(x), stringsAsFactors = FALSE)
}

#' Duplicated and unique substance vectors
#'
#' Two elements match only when both the value and the substance match.
#'
#' @param x A [substances] vector.
#' @param incomparables Passed to the data-frame method.
#' @param ... Passed on.
#' @return As the corresponding base function.
#' @name duplicated.substances
#' @examples
#' x <- set_substances(c(5, 5, 5), c("glucose", "glucose", "urea"), "mmol/L")
#' duplicated(x)
#' unique(x)
#' @export
duplicated.substances <- function(x, incomparables = FALSE, ...) {
  duplicated(as_comparable(x), incomparables, ...)
}

#' @rdname duplicated.substances
#' @export
anyDuplicated.substances <- function(x, incomparables = FALSE, ...) {
  anyDuplicated(as_comparable(x), incomparables, ...)
}

#' @rdname duplicated.substances
#' @export
unique.substances <- function(x, incomparables = FALSE, ...) {
  x[!duplicated(x, incomparables, ...)]
}

#' Coerce a substance vector to a data frame or a list
#'
#' @param x A [substances] vector.
#' @param row.names,optional As in [base::as.data.frame()].
#' @param ... Passed on.
#' @return A data frame with one `substances` column, or a list of length-1
#'   `substances` vectors.
#' @name as.data.frame.substances
#' @examples
#' x <- set_substances(c(100, 140), c("glucose", "sodium"), "mg/dL")
#' as.data.frame(x)
#' as.list(x)
#' @export
as.data.frame.substances <- function(x, row.names = NULL, optional = FALSE,
                                     ...) {
  name <- deparse(substitute(x))
  df <- as.data.frame(bare_values(x), row.names = row.names,
                      optional = optional, ...)
  df[[1L]] <- x
  if (!optional && ncol(df) == 1L) {
    colnames(df) <- name
  }
  df
}

#' @rdname as.data.frame.substances
#' @export
as.list.substances <- function(x, ...) lapply(seq_along(x), function(i) x[i])

#' Lagged differences of a substance vector
#'
#' @param x A [substances] vector.
#' @param ... Passed to [base::diff()].
#' @return A `substances` vector one element shorter per lag.
#' @examples
#' diff(set_substances(c(100, 120, 90), "glucose", "mg/dL"))
#' @export
diff.substances <- function(x, ...) {
  ## diff() is a difference between neighbours, so the substance of the result
  ## is the substance of the pair -- which only exists when they agree.
  one_substance_or_stop(x, "difference")
  out <- NextMethod()
  reclass(out, rep(substances(x)[1L], length(out)), substance_system_of(x))
}

## Collapsing several elements into one only means something if they are all the
## same substance. NA is refused rather than dropped, matching `+`: an unknown
## substance is not evidence that the rest are alike.
one_substance_or_stop <- function(x, verb) {
  ids <- substances(x)
  if (anyNA(ids)) {
    stop("cannot take the ", verb, " of a `substances` vector when the ",
         "substance is unknown", call. = FALSE)
  }
  distinct <- unique(ids)
  if (length(distinct) != 1L) {
    stop("cannot take the ", verb, " of a `substances` vector holding ",
         length(distinct), " substances; split by substance first.",
         call. = FALSE)
  }
  distinct
}
