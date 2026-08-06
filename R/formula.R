#' Molar mass computed from a molecular formula
#'
#' Molar masses of ordinary molecules are not independent facts to be looked up
#' and cited one by one -- they are arithmetic on the atomic weights. Computing
#' them keeps every value re-derivable by a reviewer and reduces the citable
#' inputs to one table of atomic weights.
#'
#' Handles flat formulae such as `"C6H12O6"` and a trailing ionic charge such as
#' `"Mg+2"` (the electron mass is neglected, as it is in standard practice).
#' Parenthesised groups and hydrates are not supported and raise an error rather
#' than being silently mis-parsed.
#'
#' @param formula A character vector of molecular formulae.
#' @param system A conversion system name or object, used for the atomic weights
#'   of the elements.
#'
#' @return A [units::units] vector in g/mol.
#'
#' @examples
#' molar_mass_from_formula(c("C6H12O6", "H2O", "C27H46O"))
#' @export
molar_mass_from_formula <- function(formula, system = NULL) {
  system <- get_system(system)
  out <- vapply(formula, molar_mass_one, numeric(1), system = system,
                USE.NAMES = FALSE)
  units::set_units(out, "g/mol", mode = "standard")
}

molar_mass_one <- function(formula, system) {
  if (is.na(formula) || !nzchar(formula)) {
    return(NA_real_)
  }
  body <- sub("[+-][0-9]*$", "", formula)   # drop trailing ionic charge
  if (grepl("[()\u00b7.]", body)) {
    stop("cannot parse formula \"", formula,
         "\": parenthesised groups and hydrates are not supported.",
         call. = FALSE)
  }

  m <- gregexpr("([A-Z][a-z]?)([0-9]*)", body)
  parts <- regmatches(body, m)[[1L]]
  if (!length(parts) || !identical(paste(parts, collapse = ""), body)) {
    stop("cannot parse formula \"", formula, "\"", call. = FALSE)
  }

  symbols <- sub("[0-9]*$", "", parts)
  counts <- sub("^[A-Za-z]+", "", parts)
  counts <- ifelse(nzchar(counts), as.numeric(counts), 1)

  weights <- vapply(symbols, function(s) {
    id <- substance_resolve(s, system)
    if (is.na(id)) {
      return(NA_real_)
    }
    p <- substance_parameters(id, system)
    if (is.null(p$molar_mass)) {
      return(NA_real_)
    }
    as.numeric(units::set_units(p$molar_mass, "g/mol", mode = "standard"))
  }, numeric(1), USE.NAMES = FALSE)

  if (anyNA(weights)) {
    stop("no atomic weight registered for element(s) ",
         paste(unique(symbols[is.na(weights)]), collapse = ", "),
         " in formula \"", formula, "\"", call. = FALSE)
  }

  sum(weights * counts)
}
