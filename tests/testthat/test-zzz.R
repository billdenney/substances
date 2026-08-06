test_that("the extra unit symbols are installed at load", {
  for (sym in substances_extra_units$symbol)
    expect_true(unit_is_defined(sym), info = sym)
})

test_that("U is the enzyme unit, so U/L to ukat/L is purely dimensional", {
  # this is why enzyme activity is NOT substance-parametric and is out of scope
  expect_equal(as.numeric(units::set_units(units::set_units(1, "U/L"), "nkat/L")),
               16.66667, tolerance = 1e-4)
  expect_equal(as.numeric(units::set_units(units::set_units(1, "U/L"), "ukat/L")),
               0.01666667, tolerance = 1e-6)
})

test_that("IU is a separate dimension from U", {
  # the WHO biological-standard unit is not the enzyme unit, even though
  # clinical data writes both as "IU"
  expect_false(units::ud_are_convertible("IU", "U"))
  expect_false(units::ud_are_convertible("IU", "mol"))
})

test_that("eq is a base dimension, bridged only by valence", {
  expect_false(units::ud_are_convertible("eq", "mol"))
  expect_true(units::ud_are_convertible("meq", "eq"))   # prefixes still work
})

test_that("unit_is_defined() distinguishes real units from strings", {
  expect_true(unit_is_defined("mg/dL"))
  expect_true(unit_is_defined("mmol/L"))
  expect_false(unit_is_defined("frac of 1"))
  expect_false(unit_is_defined("Hb Fract."))
})

test_that("install_extra_units() is idempotent", {
  # it runs at load; running it again must not error or re-install
  expect_equal(install_extra_units(), character(0))
  for (sym in substances_extra_units$symbol)
    expect_true(unit_is_defined(sym), info = sym)
})

test_that("install_extra_units() only claims symbols it actually added", {
  # so .onUnload never removes a symbol the user or another package defined
  on.exit(try(units::remove_unit("zzq"), silent = TRUE), add = TRUE)
  units::install_unit("zzq", name = "user-defined test unit")

  old <- substances_extra_units
  on.exit(assign("substances_extra_units", old,
                 envir = asNamespace("substances")), add = TRUE)
  unlockBinding("substances_extra_units", asNamespace("substances"))
  assign("substances_extra_units",
         data.frame(symbol = c("zzq", "zzr"), def = c("", ""),
                    name = c("already there", "new"),
                    stringsAsFactors = FALSE),
         envir = asNamespace("substances"))

  added <- install_extra_units()
  expect_equal(added, "zzr")          # not "zzq", which already existed
  units::remove_unit("zzr")
})

test_that("units with a definition install as well as base units", {
  on.exit({
    try(units::remove_unit("zzdef"), silent = TRUE)
    try(units::remove_unit("zzbase"), silent = TRUE)
  }, add = TRUE)

  old <- substances_extra_units
  on.exit(assign("substances_extra_units", old,
                 envir = asNamespace("substances")), add = TRUE)
  unlockBinding("substances_extra_units", asNamespace("substances"))
  assign("substances_extra_units",
         data.frame(symbol = c("zzdef", "zzbase"),
                    def = c("3 umol/min", ""),
                    name = c("defined unit", "base unit"),
                    stringsAsFactors = FALSE),
         envir = asNamespace("substances"))

  expect_setequal(install_extra_units(), c("zzdef", "zzbase"))
  # the definition was honoured, not ignored
  expect_equal(as.numeric(units::set_units(units::set_units(1, "zzdef"),
                                           "umol/min")), 3)
  # and the base unit is its own dimension
  expect_false(units::ud_are_convertible("zzbase", "mol"))
})
