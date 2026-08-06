## Unit symbols that clinical and chemical data use but udunits does not define.
## These are substance-INdependent: the symbol means the same thing whatever is
## being measured, so they belong in udunits rather than in the registry.
##
## `U` and `IU` are deliberately different dimensions. `U` is the enzyme unit,
## defined as 1 umol/min, which makes U/L <-> ukat/L an ordinary dimensional
## conversion. `IU` is the WHO biological-standard unit, whose relationship to
## mass or amount is fixed by an assay standard and differs per substance, so it
## is a base dimension bridged by an `activity` parameter. Clinical data writes
## both as "IU"; disambiguating the string is the caller's job, not ours.
substances_extra_units <- data.frame(
  symbol = c("U", "IU", "eq"),
  def = c("umol/min", "", ""),
  name = c("enzyme unit", "international unit (WHO biological standard)",
           "equivalent"),
  stringsAsFactors = FALSE
)

unit_is_defined <- function(symbol) {
  tryCatch({
    units::as_units(symbol)
    TRUE
  }, error = function(e) FALSE)
}

install_extra_units <- function() {
  installed <- character(0)
  for (i in seq_len(nrow(substances_extra_units))) {
    sym <- substances_extra_units$symbol[i]
    if (unit_is_defined(sym)) next
    def <- substances_extra_units$def[i]
    if (nzchar(def)) {
      units::install_unit(sym, def, substances_extra_units$name[i])
    } else {
      units::install_unit(sym, name = substances_extra_units$name[i])
    }
    installed <- c(installed, sym)
  }
  installed
}

# nocov start
# Load hooks run before the coverage tracer is attached and after it detaches,
# so they cannot be exercised from the test suite. install_extra_units() and
# unit_is_defined() carry the logic and are tested directly.
.onLoad <- function(libname, pkgname) {
  # udunits is global process state, so record what we added and only remove
  # those on unload -- never a symbol the user or another package defined.
  assign("installed_units", install_extra_units(), envir = substances_env)
  assign("systems", list(), envir = substances_env)
  assign("default_system", "substances", envir = substances_env)
  substance_load_default("substances",
                         path = system.file("extdata", package = pkgname))
  invisible(NULL)
}

.onUnload <- function(libpath) {
  for (sym in get0("installed_units", envir = substances_env,
                   ifnotfound = character(0)))
    try(units::remove_unit(sym), silent = TRUE)
  invisible(NULL)
}
# nocov end
