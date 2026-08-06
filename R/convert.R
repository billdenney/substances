## Find the product of small integer powers of a substance's parameters that
## makes `from` and `to` commensurable.
##
## Returns a list with the numeric factor for one unit of `from`, and the powers
## used, or NULL if no combination works. Errors if two distinct combinations
## give materially different answers, rather than silently picking one.
bridge_factor <- function(from, to, params, max_power = 1L, tolerance = 1e-9) {
  if (!length(params)) return(NULL)
  grid <- as.matrix(expand.grid(
    rep(list(seq(-max_power, max_power)), length(params)),
    KEEP.OUT.ATTRS = FALSE))
  grid <- grid[rowSums(abs(grid)) > 0, , drop = FALSE]   # 0 is the dimensional case
  grid <- grid[order(rowSums(abs(grid))), , drop = FALSE] # fewest parameters first

  from_q <- units::as_units(from)
  hits <- list()
  for (i in seq_len(nrow(grid))) {
    p <- grid[i, ]
    q <- from_q
    for (j in seq_along(params)) if (p[j] != 0) q <- q * params[[j]]^p[j]
    factor <- tryCatch({
      if (units::ud_are_convertible(units::deparse_unit(q), to))
        as.numeric(units::set_units(q, to, mode = "standard")) else NULL
    }, error = function(e) NULL)
    if (!is.null(factor))
      hits[[length(hits) + 1L]] <- list(
        factor = factor, powers = stats::setNames(p, names(params)))
  }
  if (!length(hits)) return(NULL)

  factors <- vapply(hits, `[[`, numeric(1), "factor")
  spread <- max(factors) - min(factors)
  if (spread > tolerance * max(abs(factors))) {
    described <- vapply(hits, function(h)
      paste0(paste(names(h$powers)[h$powers != 0],
                   h$powers[h$powers != 0], sep = "^", collapse = " * "),
             " = ", format(h$factor)), character(1))
    stop("ambiguous conversion from ", from, " to ", to,
         ": more than one combination of substance parameters applies and they ",
         "disagree.\n  ", paste(described, collapse = "\n  "), call. = FALSE)
  }
  hits[[1L]]
}

## Explicit (affine / factor) conversion for one substance and unit pair.
find_explicit <- function(substance_id, from, to, system) {
  if (is.na(substance_id)) return(NULL)
  cv <- system$conversions
  if (!nrow(cv)) return(NULL)
  cv <- cv[cv$substance_id == substance_id, , drop = FALSE]
  if (!nrow(cv)) return(NULL)
  norm <- function(u) vapply(u, function(z)
    tryCatch(unit_label(z), error = function(e) NA_character_), character(1),
    USE.NAMES = FALSE)
  cv$from_norm <- norm(cv$from_unit)
  cv$to_norm <- norm(cv$to_unit)
  from <- unit_label(from); to <- unit_label(to)

  fwd <- which(cv$from_norm == from & cv$to_norm == to)
  if (length(fwd)) return(c(as.list(cv[fwd[1L], ]), list(reverse = FALSE)))
  rev <- which(cv$from_norm == to & cv$to_norm == from)
  if (length(rev)) return(c(as.list(cv[rev[1L], ]), list(reverse = TRUE)))
  NULL
}

apply_explicit <- function(conv, values) {
  if (!identical(conv$status, "ok"))
    stop("the conversion ", conv$from_unit, " -> ", conv$to_unit, " for '",
         conv$substance_id, "' is marked \"", conv$status, "\"",
         if (!is.na(conv$note) && nzchar(conv$note)) paste0(": ", conv$note),
         "\n  Refusing to apply it.", call. = FALSE)
  slope <- conv$slope
  intercept <- if (is.na(conv$intercept)) 0 else conv$intercept
  switch(conv$kind,
    affine = ,
    factor = if (isTRUE(conv$reverse)) (values - intercept) / slope
             else values * slope + intercept,
    stop("unsupported conversion kind: ", conv$kind, call. = FALSE))
}

convert_substance <- function(x, to) {
  from <- unit_label(x)
  to_sym <- as_symbolic_units(to)
  to <- as.character(to_sym)
  if (identical(from, to)) return(x)

  system <- get_system(substance_system_of(x))
  values <- vctrs::field(x, "value")
  ids <- vctrs::field(x, "substance")
  dimensional <- tryCatch(units::ud_are_convertible(from, to),
                          error = function(e) FALSE)
  unresolved <- character(0)

  for (id in unique(ids)) {
    rows <- if (is.na(id)) is.na(ids) else !is.na(ids) & ids == id

    # An explicit conversion takes precedence over a dimensional one: %
    # and mmol/mol are both dimensionless, so HbA1c would otherwise convert
    # by a factor of 10 instead of by its master equation.
    conv <- find_explicit(id, from, to, system)
    if (!is.null(conv)) {
      values[rows] <- apply_explicit(conv, values[rows])
      next
    }
    if (dimensional) {
      values[rows] <- as.numeric(
        units::set_units(units::set_units(values[rows], from, mode = "standard"),
                         to, mode = "standard"))
      next
    }
    if (is.na(id)) {
      unresolved <- c(unresolved, "<unknown substance>")
      next
    }
    bridge <- bridge_factor(from, to, substance_parameters(id, system))
    if (is.null(bridge)) {
      unresolved <- c(unresolved, id)
      next
    }
    values[rows] <- values[rows] * bridge$factor
  }

  if (length(unresolved))
    stop("cannot convert ", from, " to ", to, " for: ",
         paste(unique(unresolved), collapse = ", "),
         "\n  `units` cannot relate these dimensions, and the registry has no ",
         "parameter or explicit conversion that bridges them.",
         "\n  See substance_info() for what is registered.", call. = FALSE)

  new_substance(values, ids, to_sym, substance_system_of(x))
}

#' Convert a substance vector to another unit
#'
#' Conversion is attempted in three stages: an explicit conversion registered
#' for that substance and unit pair; then an ordinary dimensional conversion by
#' [units::set_units()]; then a bridge built from the substance's parameters
#' (molar mass, density, valence, activity). Explicit conversions are tried
#' first because some are between dimensionally-equivalent units.
#'
#' Elements of different substances are converted independently, so a vector
#' holding several analytes converts in one call.
#'
#' @param x A [substance] vector.
#' @param value A unit, as in [units::set_units()].
#' @param ... Passed to [units::as_units()].
#' @param mode If `"symbols"` (the default, following `units`) `value` is taken
#'   unevaluated; if `"standard"` it is evaluated.
#'
#' @return A `substance` vector in the new unit.
#'
#' @examples
#' x <- substance(c(100, 140), "mg/dL", c("glucose", "sodium"))
#' set_units(x, "mmol/L")
#' @importFrom units set_units
#' @export
set_units.substance <- function(x, value, ...,
                                mode = units::units_options("set_units_mode")) {
  if (missing(value)) value <- units::unitless()
  else if (mode == "symbols") {
    value <- substitute(value)
    if (is.name(value) || is.call(value)) value <- format(value)
  }
  convert_substance(x, value)
}

#' @export
`units<-.substance` <- function(x, value) convert_substance(x, value)

#' Can these units be converted for this substance?
#'
#' @param from,to Units, as strings.
#' @param substance A substance name, synonym or identifier, or `NA`.
#' @param system A conversion system name or object.
#' @return `TRUE` or `FALSE`.
#' @examples
#' substance_convertible("mg/dL", "mmol/L", "glucose")
#' substance_convertible("mg/dL", "mmol/L", NA)
#' @export
substance_convertible <- function(from, to, substance = NA_character_,
                                  system = NULL) {
  system <- get_system(system)
  from <- unit_label(from); to <- unit_label(to)
  if (identical(from, to)) return(TRUE)
  id <- substance_resolve(substance, system)
  if (!is.null(find_explicit(id, from, to, system))) return(TRUE)
  if (isTRUE(tryCatch(units::ud_are_convertible(from, to),
                      error = function(e) FALSE))) return(TRUE)
  if (is.na(id)) return(FALSE)
  !is.null(tryCatch(bridge_factor(from, to, substance_parameters(id, system)),
                    error = function(e) NULL))
}
