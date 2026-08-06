test_that("same-substance addition and subtraction work", {
  x <- substance(c(1, 2), "mmol/L", "glucose")
  y <- substance(c(3, 4), "mmol/L", "glucose")
  expect_equal(as.numeric(x + y), c(4, 6))
  expect_equal(as.numeric(y - x), c(2, 2))
  expect_equal(substance_of(x + y), c("glucose", "glucose"))
})

test_that("adding different substances is an error", {
  x <- substance(1, "mmol/L", "glucose")
  y <- substance(1, "mmol/L", "sodium")
  expect_error(x + y, "different substances", fixed = TRUE)
})

test_that("addition converts the right-hand side, substance-aware", {
  x <- substance(1, "mmol/L", "glucose")
  y <- substance(18.0156, "mg/dL", "glucose")   # = 1 mmol/L
  expect_equal(as.numeric(x + y), 2, tolerance = 1e-4)
  expect_equal(unit_label(x + y), "mmol/L")
})

test_that("an unknown substance cannot be added", {
  x <- substance(1, "mmol/L", NA)
  expect_error(x + x, "substance is unknown", fixed = TRUE)
})

test_that("multiplication by a bare number scales, and keeps the substance", {
  x <- substance(c(1, 2), "mmol/L", c("glucose", "sodium"))
  expect_equal(as.numeric(x * 2), c(2, 4))
  expect_equal(as.numeric(2 * x), c(2, 4))
  expect_equal(as.numeric(x / 2), c(0.5, 1))
  expect_equal(substance_of(x * 2), c("glucose", "sodium"))
})

test_that("adding a bare number is an error", {
  expect_error(substance(1, "mmol/L", "glucose") + 1, "has no unit", fixed = TRUE)
})

test_that("substance_scale() carries the substance through a units quantity", {
  # concentration times volume is an amount, still of the same substance
  x <- substance(2, "mmol/L", "glucose")
  y <- substance_scale(x, units::set_units(3, "L"))
  expect_s3_class(y, "substance")
  expect_equal(substance_of(y), "glucose")
  expect_equal(as.numeric(units::set_units(drop_substance(y), "mmol")), 6,
               tolerance = 1e-9)

  expect_equal(as.numeric(substance_scale(x, 3)), 6)
})

test_that("`*` between a substance and a units quantity cannot dispatch", {
  # Pinned deliberately. R refuses to choose between the two classes' operator
  # methods, so this fails before any method of ours runs; substance_scale() is
  # the supported path. If a future R or vctrs changes this, this test tells us.
  x <- substance(2, "mmol/L", "glucose")
  expect_error(suppressWarnings(x * units::set_units(3, "L")))
})

test_that("dividing by the same substance cancels it", {
  x <- substance(6, "mmol/L", "glucose")
  y <- substance(2, "mmol/L", "glucose")
  z <- x / y
  expect_false(inherits(z, "substance"))
  expect_s3_class(z, "units")
  expect_equal(as.numeric(z), 3)
})

test_that("multiplying two substances is refused", {
  x <- substance(1, "mmol/L", "glucose")
  expect_error(x * x, "substance squared", fixed = TRUE)
})

test_that("unary minus works", {
  x <- substance(c(1, -2), "mmol/L", "glucose")
  expect_equal(as.numeric(-x), c(-1, 2))
  expect_equal(substance_of(-x), c("glucose", "glucose"))
})

test_that("sum and mean require a single substance", {
  x <- substance(c(1, 2, 3), "mmol/L", "glucose")
  expect_equal(as.numeric(sum(x)), 6)
  expect_equal(as.numeric(mean(x)), 2)
  expect_equal(substance_of(sum(x)), "glucose")

  y <- substance(c(1, 2), "mmol/L", c("glucose", "sodium"))
  expect_error(sum(y), "2 substances", fixed = TRUE)
  expect_error(mean(y), "2 substances", fixed = TRUE)
})

test_that("arithmetic across systems is refused", {
  substance_system("arith_iso",
                   substances = data.frame(substance_id = "glucose",
                                           name = "Glucose"))
  x <- substance(1, "mmol/L", "glucose")
  z <- substance(1, "mmol/L", "glucose", system = "arith_iso")
  expect_error(x + z, "different systems", fixed = TRUE)
})
