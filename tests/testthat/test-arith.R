test_that("same-substance addition and subtraction work", {
  x <- set_substances(c(1, 2), "glucose", "mmol/L")
  y <- set_substances(c(3, 4), "glucose", "mmol/L")
  expect_equal(as.numeric(x + y), c(4, 6))
  expect_equal(as.numeric(y - x), c(2, 2))
  expect_equal(substances(x + y), c("glucose", "glucose"))
})

test_that("adding different substances is an error", {
  x <- set_substances(1, "glucose", "mmol/L")
  y <- set_substances(1, "sodium", "mmol/L")
  expect_error(x + y, "different substances", fixed = TRUE)
})

test_that("addition converts the right-hand side, substance-aware", {
  x <- set_substances(1, "glucose", "mmol/L")
  y <- set_substances(18.0156, "glucose", "mg/dL")   # = 1 mmol/L
  expect_equal(as.numeric(x + y), 2, tolerance = 1e-4)
  expect_equal(unit_label(x + y), "mmol/L")
})

test_that("an unknown substance cannot be added", {
  x <- set_substances(1, NA, "mmol/L")
  expect_error(x + x, "substance is unknown", fixed = TRUE)
})

test_that("multiplication by a bare number scales, and keeps the substance", {
  x <- set_substances(c(1, 2), c("glucose", "sodium"), "mmol/L")
  expect_equal(as.numeric(x * 2), c(2, 4))
  expect_equal(as.numeric(2 * x), c(2, 4))
  expect_equal(as.numeric(x / 2), c(0.5, 1))
  expect_equal(substances(x * 2), c("glucose", "sodium"))
})

test_that("adding a bare number is an error", {
  expect_error(set_substances(1, "glucose", "mmol/L") + 1, "has no unit", fixed = TRUE)
})

test_that("multiplying by a units quantity carries the substance through", {
  # concentration times volume is an amount, still of the same substance
  x <- set_substances(2, "glucose", "mmol/L")
  y <- x * units::set_units(3, "L")
  expect_s3_class(y, "substances")
  expect_equal(substances(y), "glucose")
  expect_equal(as.numeric(units::set_units(drop_substances(y), "mmol")), 6,
               tolerance = 1e-9)
})

test_that("multiplication works with the units quantity on either side", {
  x <- set_substances(2, "glucose", "mmol/L")
  vol <- units::set_units(3, "L")
  left <- x * vol
  right <- vol * x
  expect_s3_class(right, "substances")
  expect_equal(substances(right), "glucose")
  expect_equal(as.numeric(units::set_units(drop_substances(right), "mmol")),
               as.numeric(units::set_units(drop_substances(left), "mmol")),
               tolerance = 1e-9)
})

test_that("division by a units quantity works in both directions", {
  x <- set_substances(6, "glucose", "mmol/L")
  vol <- units::set_units(3, "L")

  a <- x / vol
  expect_s3_class(a, "substances")
  expect_equal(substances(a), "glucose")
  expect_equal(as.numeric(a), 2)

  b <- vol / x
  expect_s3_class(b, "substances")
  expect_equal(substances(b), "glucose")
  expect_equal(as.numeric(b), 0.5)
})

test_that("a units quantity recycles against a longer substance vector", {
  x <- set_substances(c(1, 2, 3), c("glucose", "sodium", "glucose"), "mmol/L")
  y <- x * units::set_units(2, "L")
  expect_length(y, 3L)
  expect_equal(substances(y), c("glucose", "sodium", "glucose"))
})

test_that("only `*` and `/` are defined against a bare units quantity", {
  x <- set_substances(2, "glucose", "mmol/L")
  expect_error(x + units::set_units(3, "mmol/L"),
               "cannot be added to or compared with", fixed = TRUE)
  expect_error(units::set_units(3, "mmol/L") - x,
               "cannot be added to or compared with", fixed = TRUE)
  expect_error(x > units::set_units(3, "mmol/L"),
               "cannot be added to or compared with", fixed = TRUE)
})

test_that("chooseOpsMethod is claimed only for units, not every conflict", {
  x <- set_substances(2, "glucose", "mmol/L")
  expect_true(chooseOpsMethod(x, units::set_units(1, "L")))
  expect_false(chooseOpsMethod(x, as.difftime(1, units = "secs")))
  expect_false(chooseOpsMethod(x, Sys.Date()))
})

test_that("loading substances leaves ordinary units arithmetic intact", {
  # chooseOpsMethod.substances() is consulted whenever R has to break an
  # operator tie, so pin the behaviour that must not change: arithmetic between
  # two plain units quantities never involves this package at all.
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
  x <- set_substances(6, "glucose", "mmol/L")
  y <- set_substances(2, "glucose", "mmol/L")
  z <- x / y
  expect_false(inherits(z, "substances"))
  expect_s3_class(z, "units")
  expect_equal(as.numeric(z), 3)
})

test_that("multiplying two substances is refused", {
  x <- set_substances(1, "glucose", "mmol/L")
  expect_error(x * x, "substance squared", fixed = TRUE)
})

test_that("unary minus works", {
  x <- set_substances(c(1, -2), "glucose", "mmol/L")
  expect_equal(as.numeric(-x), c(-1, 2))
  expect_equal(substances(-x), c("glucose", "glucose"))
})

test_that("sum and mean require a single substance", {
  x <- set_substances(c(1, 2, 3), "glucose", "mmol/L")
  expect_equal(as.numeric(sum(x)), 6)
  expect_equal(as.numeric(mean(x)), 2)
  expect_equal(substances(sum(x)), "glucose")

  y <- set_substances(c(1, 2), c("glucose", "sodium"), "mmol/L")
  expect_error(sum(y), "2 substances", fixed = TRUE)
  expect_error(mean(y), "2 substances", fixed = TRUE)
})

test_that("arithmetic across systems is refused", {
  substance_system("arith_iso",
                   substances = data.frame(substance_id = "glucose",
                                           name = "Glucose"))
  x <- set_substances(1, "glucose", "mmol/L")
  z <- set_substances(1, "glucose", "mmol/L", system = "arith_iso")
  expect_error(x + z, "different systems", fixed = TRUE)
})

test_that("exponentiation is refused, because the parameters would not follow", {
  x <- set_substances(2, "glucose", "mmol/L")
  expect_error(x^2, "cannot raise a `substances` vector to a power",
               fixed = TRUE)
})

test_that("unary plus returns the vector unchanged", {
  x <- set_substances(c(1, -2), "glucose", "mmol/L")
  expect_equal(as.numeric(+x), c(1, -2))
  expect_equal(substances(+x), c("glucose", "glucose"))
})

test_that("undefined operator combinations are refused", {
  x <- set_substances(2, "glucose", "mmol/L")
  expect_error(x > 1, "a number has no unit", fixed = TRUE)
  expect_error(1 - x, "a number has no unit", fixed = TRUE)
  expect_error(!x, "cannot use unary `!`", fixed = TRUE)
  expect_error(x %% x, "cannot use `%%`", fixed = TRUE)

  # a mixed_substances vector has no single unit for the algebra to use
  m <- mixed_substances(c(1, 2), c("mg/dL", "mmol/L"), "glucose")
  expect_error(x * m, "and a mixed_substances", fixed = TRUE)
  expect_error(x + m, "cannot use `+`", fixed = TRUE)
})

test_that("dividing different substances is refused", {
  x <- set_substances(1, "glucose", "mmol/L")
  y <- set_substances(1, "sodium", "mmol/L")
  expect_error(x / y, "different substances", fixed = TRUE)
})

test_that("subtraction converts the right-hand side too", {
  x <- set_substances(2, "glucose", "mmol/L")
  y <- set_substances(18.0156, "glucose", "mg/dL")   # = 1 mmol/L
  expect_equal(as.numeric(x - y), 1, tolerance = 1e-4)
})

test_that("addition reports a unit that cannot be reconciled", {
  x <- set_substances(1, "hba1c", "mmol/L")
  y <- set_substances(1, "hba1c", "mg/dL")
  expect_error(x + y, "cannot use `+` on", fixed = TRUE)
})

test_that("the unary branch is reached only by unary operators", {
  # `Ops` signals the unary forms by leaving e2 missing, and the branch that
  # negates x depends on that, so pin it: a binary minus must not land there
  x <- set_substances(c(1, -2), "glucose", "mmol/L")
  expect_equal(as.numeric(-x), c(-1, 2))          # unary reaches it
  expect_equal(as.numeric(+x), c(1, -2))

  # binary minus goes elsewhere: if it reached the unary branch it would negate
  # x and ignore y entirely
  y <- set_substances(c(1, 1), "glucose", "mmol/L")
  expect_equal(as.numeric(x - y), c(0, -3))
  # even the refused binary form stays binary: it reports the bare number
  # rather than silently returning -x
  expect_error(x - 1, "has no unit", fixed = TRUE)

  # and a unary operator we do not define still errors
  expect_error(!x, "cannot use unary `!`", fixed = TRUE)
})

test_that("sum and mean refuse an unknown substance, as `+` does", {
  # these used to na.omit the substance, so sum() succeeded where + errored
  x <- set_substances(c(1, 2), c("glucose", NA), "mmol/L")
  expect_error(sum(x), "substance is unknown", fixed = TRUE)
  expect_error(mean(x), "substance is unknown", fixed = TRUE)
  expect_error(x + x, "substance is unknown", fixed = TRUE)
})
