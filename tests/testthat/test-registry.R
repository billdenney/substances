test_that("systems are isolated from one another", {
  substance_system("project_a",
    substances = data.frame(substance_id = "widgetol", name = "Widgetol"),
    parameters = data.frame(substance_id = "widgetol", parameter = "molar_mass",
                            value = 100, unit = "g/mol", status = "ok",
                            source_id = NA, note = NA))

  x <- substance(1, "mg/dL", "widgetol", system = "project_a")
  expect_equal(as.numeric(set_units(x, "mmol/L")), 0.1, tolerance = 1e-9)

  # the custom substance does not leak into the default system
  expect_true(is.na(substance_resolve("widgetol")))
  expect_error(substance(1, "mg/dL", "widgetol"), "unknown substance")

  # nor does the default leak into the custom one
  expect_true(is.na(substance_resolve("glucose", system = "project_a")))
})

test_that("a system can inherit from another and override it", {
  substance_system("project_b", inherit = "substances",
    substances = data.frame(substance_id = "glucose", name = "Glucose"),
    parameters = data.frame(substance_id = "glucose", parameter = "molar_mass",
                            value = 999, unit = "g/mol", status = "ok",
                            source_id = NA, note = NA))
  # the local entry wins
  p <- substance_parameters("glucose", system = "project_b")
  expect_equal(as.numeric(p$molar_mass), 999)
  # inherited substances are still reachable
  expect_equal(substance_resolve("sodium", system = "project_b"), "sodium")
  # the default system is untouched
  expect_equal(as.numeric(substance_parameters("glucose")$molar_mass),
               180.156, tolerance = 1e-6)
})

test_that("an unknown system is an error", {
  expect_error(get_system("nope"), "no conversion system named", fixed = TRUE)
})

test_that("registry validation rejects malformed tables", {
  expect_error(
    substance_system("bad_param",
      substances = data.frame(substance_id = "x", name = "X"),
      parameters = data.frame(substance_id = "x", parameter = "wibble",
                              value = 1, unit = "g/mol")),
    "unknown parameter", fixed = TRUE)

  expect_error(
    substance_system("bad_dup",
      substances = data.frame(substance_id = "x", name = "X"),
      parameters = data.frame(substance_id = c("x", "x"),
                              parameter = c("molar_mass", "molar_mass"),
                              value = c(1, 2), unit = "g/mol")),
    "duplicate", fixed = TRUE)

  expect_error(
    substance_system("bad_orphan",
      substances = data.frame(substance_id = "x", name = "X"),
      parameters = data.frame(substance_id = "y", parameter = "molar_mass",
                              value = 1, unit = "g/mol")),
    "no entry in `substances`", fixed = TRUE)
})

test_that("substance_parameters() omits non-ok entries", {
  expect_length(substance_parameters("lipoprotein_a"), 0L)
  expect_named(substance_parameters("sodium"), c("molar_mass", "valence"),
               ignore.order = TRUE)
})

test_that("substance_info() reports values with their citations", {
  info <- substance_info("LDL Cholesterol")
  expect_equal(info$substance$substance_id, "cholesterol")
  expect_true(all(!is.na(info$parameters$citation)))
  expect_error(substance_info("unobtainium"), "unknown substance")
})

test_that("the default system can be changed and restored", {
  substance_system("temp_default",
                   substances = data.frame(substance_id = "glucose",
                                           name = "Glucose"))
  old <- substance_set_default_system("temp_default")
  on.exit(substance_set_default_system(old))
  expect_equal(substance_default_system(), "temp_default")
  expect_true(is.na(substance_resolve("sodium")))
})

test_that("the extra unit symbols are installed and behave as intended", {
  expect_true(unit_is_defined("U"))
  expect_true(unit_is_defined("IU"))
  expect_true(unit_is_defined("eq"))
  # U is umol/min, which makes enzyme activity purely dimensional
  expect_equal(as.numeric(units::set_units(units::set_units(1, "U/L"), "nkat/L")),
               16.6667, tolerance = 1e-4)
  # IU is a separate dimension: it is not the enzyme unit
  expect_false(units::ud_are_convertible("IU", "U"))
})
