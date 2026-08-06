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

test_that("printing a system reports its table sizes", {
  out <- capture.output(print(get_system("substances")))
  expect_match(out[1], "substance_system 'substances'", fixed = TRUE)
  expect_match(paste(out, collapse = "\n"), "substances:", fixed = TRUE)
  expect_match(paste(out, collapse = "\n"), "sources:", fixed = TRUE)
})

test_that("substance_info() prints parameters with their citations", {
  out <- paste(capture.output(print(substance_info("glucose"))), collapse = "\n")
  expect_match(out, "glucose", fixed = TRUE)
  expect_match(out, "C6H12O6", fixed = TRUE)      # formula line
  expect_match(out, "molar_mass", fixed = TRUE)
  expect_match(out, "CIAAW", fixed = TRUE)        # the citation
})

test_that("substance_info() prints explicit conversions", {
  out <- paste(capture.output(print(substance_info("HbA1c"))), collapse = "\n")
  expect_match(out, "conversions:", fixed = TRUE)
  expect_match(out, "affine", fixed = TRUE)
  expect_match(out, "NGSP", fixed = TRUE)
})

test_that("substance_info() prints a substance with no formula", {
  out <- paste(capture.output(print(substance_info("insulin"))), collapse = "\n")
  expect_match(out, "activity", fixed = TRUE)
  expect_false(grepl("formula:", out, fixed = TRUE))
})

test_that("substance_systems() lists what has been registered", {
  substance_system("listed_system",
                   substances = data.frame(substance_id = "x", name = "X"))
  expect_true("listed_system" %in% substance_systems())
  expect_true("substances" %in% substance_systems())
})

test_that("a system object can be passed instead of its name", {
  sys_obj <- get_system("substances")
  expect_identical(get_system(sys_obj), sys_obj)
  expect_equal(substance_resolve("glucose", system = sys_obj), "glucose")
})

test_that("setting an unknown default system is refused", {
  expect_error(substance_set_default_system("nope"),
               "no conversion system named", fixed = TRUE)
})
