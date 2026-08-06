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

test_that("multiplying by a units quantity carries the substance through", {
  # concentration times volume is an amount, still of the same substance
  x <- substance(2, "mmol/L", "glucose")
  y <- x * units::set_units(3, "L")
  expect_s3_class(y, "substance")
  expect_equal(substance_of(y), "glucose")
  expect_equal(as.numeric(units::set_units(drop_substance(y), "mmol")), 6,
               tolerance = 1e-9)
})

test_that("multiplication works with the units quantity on either side", {
  x <- substance(2, "mmol/L", "glucose")
  vol <- units::set_units(3, "L")
  left <- x * vol
  right <- vol * x
  expect_s3_class(right, "substance")
  expect_equal(substance_of(right), "glucose")
  expect_equal(as.numeric(units::set_units(drop_substance(right), "mmol")),
               as.numeric(units::set_units(drop_substance(left), "mmol")),
               tolerance = 1e-9)
})

test_that("division by a units quantity works in both directions", {
  x <- substance(6, "mmol/L", "glucose")
  vol <- units::set_units(3, "L")

  a <- x / vol
  expect_s3_class(a, "substance")
  expect_equal(substance_of(a), "glucose")
  expect_equal(as.numeric(a), 2)

  b <- vol / x
  expect_s3_class(b, "substance")
  expect_equal(substance_of(b), "glucose")
  expect_equal(as.numeric(b), 0.5)
})

test_that("a units quantity recycles against a longer substance vector", {
  x <- substance(c(1, 2, 3), "mmol/L", c("glucose", "sodium", "glucose"))
  y <- x * units::set_units(2, "L")
  expect_length(y, 3L)
  expect_equal(substance_of(y), c("glucose", "sodium", "glucose"))
})

test_that("only `*` and `/` are defined against a bare units quantity", {
  x <- substance(2, "mmol/L", "glucose")
  expect_error(x + units::set_units(3, "mmol/L"), "only `*` and `/`",
               fixed = TRUE)
  expect_error(units::set_units(3, "mmol/L") - x, "only `*` and `/`",
               fixed = TRUE)
})

test_that("chooseOpsMethod is claimed only for units, not every conflict", {
  x <- substance(2, "mmol/L", "glucose")
  expect_true(chooseOpsMethod(x, units::set_units(1, "L")))
  expect_false(chooseOpsMethod(x, as.difftime(1, units = "secs")))
  expect_false(chooseOpsMethod(x, Sys.Date()))
})

test_that("adding vec_arith.units leaves ordinary units arithmetic intact", {
  # We register a method on another package's class, so pin the behaviour that
  # must not change: units-to-units arithmetic never reaches vec_arith at all.
  L <- units::set_units(3, "L")
  expect_equal(as.numeric(L * units::set_units(2, "m")), 6)
  expect_equal(as.numeric(L + units::set_units(2, "L")), 5)
  expect_equal(as.numeric(L - units::set_units(1, "L")), 2)
  expect_equal(as.numeric(L * 2), 6)
  expect_equal(as.numeric(2 * L), 6)
  expect_equal(as.numeric(L / units::set_units(3, "L")), 1)
  expect_equal(as.numeric(-L), -3)
  expect_true(L > units::set_units(1, "L"))
  expect_equal(as.numeric(units::set_units(units::set_units(1, "km"), "m")), 1000)
  expect_equal(as.character(units(units::set_units(1, "m") /
                                    units::set_units(2, "s"))), "m/s")
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
