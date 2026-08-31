## Optional support for vctrs and pillar.
##
## Nothing here is needed to use the package: the base S3 methods keep the
## substance aligned through `[`, `c()`, `rep()` and data-frame subsetting on
## their own. What these add is the tidyverse's own path -- vec_slice(),
## vec_c(), bind_rows() and the recycling rules -- which bypasses `[` and would
## otherwise reach the substance attribute without carrying it along.
##
## Neither package is a dependency. The methods are registered in .onLoad() if
## the package is loaded, and by a load hook if it is loaded later, so
## substances imposes no install cost on someone who does not use them. This is
## the same arrangement as quantities.

## vctrs proxying and restoration -------------------------------------
##
## The proxy is a two-column data frame, so every vctrs operation that slices,
## orders or recycles moves the value and its substance together. Restoring
## reads the unit and system from `to`, which is the prototype vctrs carried
## through the operation.

vec_proxy.substances <- function(x, ...) {
  vctrs::new_data_frame(list(value = as.vector(bare_values(x), "double"),
                             substance = substances(x)), n = length(x))
}

vec_restore.substances <- function(x, to, ...) {
  new_substances(x$value, x$substance, units(to), substance_system_of(to))
}

## vctrs coercion -----------------------------------------------------
##
## The common type of two substance vectors is the first one's unit, matching
## both c.substances() and vec_ptype2.units.units(); casting then converts, the
## substance-aware way.
##
## Whether two units are reconcilable is not decidable here: mg/dL and mmol/L
## are, but only for a substance with a molar mass, and vctrs calls this with
## prototypes, which are zero-length and so carry no substance. Only the system
## can be checked; the cast below raises the conversion error.

vec_ptype2.substances.substances <- function(x, y, ...,
                                             x_arg = "", y_arg = "") {
  if (!identical(substance_system_of(x), substance_system_of(y))) {
    vctrs::stop_incompatible_type(x, y, x_arg = x_arg, y_arg = y_arg,
                                  details = "Different substance systems.")
  }
  x[0L]
}

vec_cast.substances.substances <- function(x, to, ...) {
  convert_substance(x, units(to))
}

vec_ptype_abbr.substances <- function(x, ...) "subst"

vec_ptype_full.substances <- function(x, ...) {
  paste0("substances<", unit_label(x), ">")
}

## pillar ------------------------------------------------------------

type_sum.substances <- function(x) paste0("subst<", unit_label(x), ">")

pillar_shaft.substances <- function(x, ...) {
  ## type_sum() already puts the unit in the column header, so the cells carry
  ## the value and the substance only.
  pillar::new_pillar_shaft_simple(labelled_values(x), align = "right")
}

# nocov start
register_all_s3_methods <- function() {
  register_s3_method("vctrs::vec_proxy", "substances")
  register_s3_method("vctrs::vec_restore", "substances")
  register_s3_method("vctrs::vec_ptype2", "substances.substances")
  register_s3_method("vctrs::vec_cast", "substances.substances")
  register_s3_method("vctrs::vec_ptype_abbr", "substances")
  register_s3_method("vctrs::vec_ptype_full", "substances")

  register_s3_method("pillar::type_sum", "substances")
  register_s3_method("pillar::pillar_shaft", "substances")
}

register_s3_method <- function(generic, class, fun = NULL) {
  pieces <- strsplit(generic, "::")[[1L]]
  package <- pieces[[1L]]
  generic <- pieces[[2L]]

  if (is.null(fun)) {
    fun <- get(paste0(generic, ".", class), envir = parent.frame())
  }
  if (package %in% loadedNamespaces()) {
    registerS3method(generic, class, fun, envir = asNamespace(package))
  }

  ## Always set the hook too, in case the package is loaded after this one, or
  ## unloaded and reloaded.
  setHook(packageEvent(package, "onLoad"), function(...) {
    registerS3method(generic, class, fun, envir = asNamespace(package))
  })
}
# nocov end
