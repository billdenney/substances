#' @keywords internal
#' @aliases substances-package
#'
#' @details
#' The `units` package converts between commensurable units. It cannot convert
#' mg/dL to mmol/L, because that needs the analyte's molar mass, and a unit
#' string has nowhere to put one.
#'
#' The substance cannot live in the unit string either. Defining `mol_glucose`
#' in udunits relative to `mol` makes glucose and sodium mutually convertible,
#' so `1 mol_glucose + 1 mol_sodium` silently returns a number; defining them as
#' base units instead keeps them apart but makes `mol` to `g` conversion
#' impossible. Isolation and conversion are mutually exclusive inside udunits.
#' So here the substance is an attribute of the vector, carried alongside the
#' unit, and the registry supplies the quantities that bridge dimensions `units`
#' cannot relate on its own.
#'
#' @section Object model:
#' A [substances] vector is a [units::units] vector with one extra attribute:
#' the substance of every element. Inheriting from `units` means every `units`
#' method works, and the methods in this package -- `[`, `c()`, `rep()`, `Ops`,
#' `Summary` and the rest -- keep the substance aligned with the values.
#' [substances()] checks that alignment on every read, so an operation this
#' package has not anticipated fails loudly instead of matching values to
#' analytes at random.
#'
#' `vctrs` and `pillar` are supported but not required. Their methods are
#' registered at load time when those packages are present, which is what lets
#' `vec_slice()`, `vec_c()` and `dplyr::bind_rows()` carry the substance too.
#'
#' @section R version:
#' R >= 4.3.0 is required for [chooseOpsMethod()], which is what lets a
#' `substances` vector interoperate with a plain [units::units] quantity.
#' Without it R refuses to choose between the two classes' operator methods:
#' `x * units::set_units(3, "L")` warns "Incompatible methods" and returns the
#' values with `x`'s unit unchanged -- a wrong answer rather than an error.
#'
#' @section Out of scope:
#' Normalising unit *strings* (`ng/ml` versus `ng/mL`, `IU/L` versus `U/L`) is
#' per-source data cleaning and belongs in the consuming package. So are
#' conversions `units` already performs: `mg/dL` to `g/L` is dimensional, and so
#' is `U/L` to `ukat/L` once `U` is defined as `umol/min`. This package handles
#' only conversions that need a property of the substance.
"_PACKAGE"

## `units()` and `units<-()` are base R generics, so only these three need
## importing from units. The stats and utils generics have to be imported too,
## or registering methods for them fails at namespace load -- which only shows
## up under R CMD check, since load_all() has them attached already.
#' @importFrom units set_units mixed_units drop_units
#' @importFrom stats median quantile weighted.mean
#' @importFrom utils str
NULL

## Re-exported so `library(substances)` alone is enough to convert; the
## substance methods are useless without the generic.
#' @export
units::set_units
