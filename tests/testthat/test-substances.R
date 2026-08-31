test_that("a substance vector holds a different substance per element", {
  x <- set_substances(c(100, 140), c("glucose", "sodium"), "mg/dL")
  expect_s3_class(x, "substances")
  expect_equal(length(x), 2L)
  expect_equal(substances(x), c("glucose", "sodium"))
  expect_equal(as.numeric(x), c(100, 140))
  expect_equal(unit_label(x), "mg/dL")
})

test_that("substance names are resolved through synonyms, case-insensitively", {
  x <- set_substances(c(1, 2, 3),
                 c("LDL Cholesterol", "HDL-C", "cholesterol"), "mg/dL")
  expect_equal(substances(x), rep("cholesterol", 3))
  expect_equal(substance_resolve("  GLUCOSE "), "glucose")
  expect_equal(substance_resolve("Na"), "sodium")
})

test_that("an unknown substance is an error, not a silent NA", {
  expect_error(set_substances(1, "unobtainium", "mg/dL"),
               "unknown substance", fixed = TRUE)
})

test_that("the substance survives the whole method surface", {
  # this is the property the plain-attribute representation does not have
  x <- set_substances(c(1, 2, 3), c("glucose", "sodium", "glucose"), "mg/dL")

  expect_equal(substances(x[2:3]), c("sodium", "glucose"))
  expect_equal(substances(c(x, x)), rep(c("glucose", "sodium", "glucose"), 2))
  expect_equal(substances(rev(x)), c("glucose", "sodium", "glucose"))
  expect_equal(substances(rep(x, 2)), rep(c("glucose", "sodium", "glucose"), 2))
  expect_equal(substances(x[c(TRUE, FALSE, TRUE)]), c("glucose", "glucose"))

  d <- data.frame(id = 1:3)
  d$x <- x
  expect_equal(substances(d[d$id > 1, ]$x), c("sodium", "glucose"))
  expect_equal(substances(d[order(-d$id), ]$x), c("glucose", "sodium", "glucose"))
})

test_that("combining converts to the first vector's unit", {
  # this is what c() on plain units vectors does, and the conversion available
  # here is the substance-aware one
  x <- set_substances(1, "glucose", "mg/dL")
  y <- set_substances(1, "glucose", "mmol/L")
  z <- c(x, y)
  expect_equal(unit_label(z), "mg/dL")
  expect_equal(as.numeric(z), c(1, 18.0156), tolerance = 1e-4)

  # and it refuses when no conversion exists rather than dropping the unit
  expect_error(c(set_substances(1, "hba1c", "mmol/L"),
                 set_substances(1, "hba1c", "mg/dL")),
               "cannot convert", fixed = TRUE)
})

test_that("combining different systems is an error", {
  x <- set_substances(1, "glucose", "mg/dL")
  substance_system("isolated",
                   substances = data.frame(substance_id = "glucose",
                                           name = "Glucose"))
  z <- set_substances(1, "glucose", "mg/dL", system = "isolated")
  expect_error(c(x, z), "different systems", fixed = TRUE)
})

test_that("accessors round-trip", {
  x <- set_substances(c(100, 140), c("glucose", "sodium"), "mg/dL")
  expect_s3_class(drop_substances(x), "units")
  expect_equal(as.numeric(drop_substances(x)), c(100, 140))
  expect_equal(substance_system_of(x), "substances")

  substances(x) <- c("sodium", "glucose")
  expect_equal(substances(x), c("sodium", "glucose"))
  expect_error(substances(x) <- "unobtainium", "unknown substance")
})

test_that("NA substance is allowed and printed as unknown", {
  x <- set_substances(c(1, 2), c("glucose", NA), "mg/dL")
  expect_equal(substances(x), c("glucose", NA))
  expect_match(format(x)[2], "?", fixed = TRUE)
})

test_that("a corrupt vector is refused rather than guessed at", {
  # nothing in the package produces this, but an operation that copied the
  # attributes without the values would, and matching values to analytes at
  # random is the one failure mode this package cannot have
  x <- set_substances(c(1, 2, 3), "glucose", "mg/dL")
  attr(x, "substance") <- c("glucose", "sodium")
  expect_error(substances(x), "3 values but 2 substances", fixed = TRUE)
  expect_error(print(x), "3 values but 2 substances", fixed = TRUE)
})

test_that("units() returns the symbolic units, as for a units vector", {
  x <- set_substances(1, "glucose", "mg/dL")
  expect_s3_class(units(x), "symbolic_units")
  expect_equal(as.character(units(x)), "mg/dL")
  expect_equal(units(x), units(x))
})

test_that("as.numeric() works, which needs an as.double() method", {
  # as.numeric() dispatches through as.double(), so an as.numeric.substances
  # method would never be reached
  x <- set_substances(c(1, 2), "glucose", "mg/dL")
  expect_equal(as.numeric(x), c(1, 2))
  expect_equal(as.double(x), c(1, 2))
})

test_that("as.character() shows value, unit and substance", {
  x <- set_substances(1, "glucose", "mg/dL")
  expect_match(as.character(x), "mg/dL", fixed = TRUE)
  expect_match(as.character(x), "glucose", fixed = TRUE)
})

test_that("printing shows the unit in the header and the substance per row", {
  x <- set_substances(c(1, 2), c("glucose", "sodium"), "mg/dL")
  expect_output(print(x), "<substances[2]> mg/dL", fixed = TRUE)
  expect_output(print(x), "glucose", fixed = TRUE)
  expect_output(print(x), "sodium", fixed = TRUE)
  expect_output(str(x), "Substances: [mg/dL] glucose, sodium", fixed = TRUE)
})

test_that("the unit may be given as a units object or symbolic_units", {
  from_chr <- set_substances(1, "glucose", "mg/dL")
  from_units <- set_substances(1, "glucose", units::as_units("mg/dL"))
  from_sym <- set_substances(1, "glucose", units(units::as_units("mg/dL")))
  expect_equal(unit_label(from_units), unit_label(from_chr))
  expect_equal(unit_label(from_sym), unit_label(from_chr))
})

test_that("a zero-length substance vector is well formed", {
  x <- set_substances()
  expect_length(x, 0L)
  expect_equal(substances(x), character(0))
  expect_length(c(x, x), 0L)
})

test_that("combining with an unrelated class is an error", {
  x <- set_substances(1, "glucose", "mg/dL")
  expect_error(c(x, 1), "does not say which substance it is", fixed = TRUE)
  expect_error(c(x, "a"), "does not say which substance it is", fixed = TRUE)
  expect_error(c(x, units::set_units(1, "mg/dL")),
               "does not say which substance it is", fixed = TRUE)
})
