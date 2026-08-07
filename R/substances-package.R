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
#' So here the substance is a field of the vector, and the registry supplies the
#' quantities that bridge dimensions `units` cannot relate on its own.
#'
#' @section R version:
#' R >= 4.3.0 is required for [chooseOpsMethod()], which is what lets a
#' `substance` interoperate with a plain [units::units] quantity. Without it R
#' refuses to choose between the two classes' operator methods and
#' `x * units::set_units(3, "L")` fails with "Incompatible methods". The only
#' S3 arrangement that works without it is inheriting from `units`, which
#' silently drops the substance.
#'
#' @section Out of scope:
#' Normalising unit *strings* (`ng/ml` versus `ng/mL`, `IU/L` versus `U/L`) is
#' per-source data cleaning and belongs in the consuming package. So are
#' conversions `units` already performs: `mg/dL` to `g/L` is dimensional, and so
#' is `U/L` to `ukat/L` once `U` is defined as `umol/min`. This package handles
#' only conversions that need a property of the substance.
"_PACKAGE"

## vctrs generics are re-exported so methods dispatch without attaching vctrs.
#' @importFrom vctrs vec_ptype2 vec_cast vec_arith vec_ptype_abbr vec_ptype_full
#' @importFrom vctrs vec_arith_base obj_print_header vec_arith.numeric
## `units()` and `units<-()` are base R generics; only set_units() comes from units.
#' @importFrom units set_units
NULL

## Re-exported so `library(substances)` alone is enough to convert; the
## substance methods are useless without the generic.
#' @export
units::set_units
