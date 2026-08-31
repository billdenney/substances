test_that("mixed_substances holds a unit and a substance per element", {
  m <- mixed_substances(c(100, 5.5, 140),
                        c("mg/dL", "mmol/L", "mg/dL"),
                        c("glucose", "glucose", "sodium"))
  expect_s3_class(m, "mixed_substances")
  expect_length(m, 3L)
  expect_equal(as.numeric(m), c(100, 5.5, 140))
  expect_equal(substances(m), c("glucose", "glucose", "sodium"))
  expect_equal(substance_units(m), c("mg/dL", "mmol/L", "mg/dL"))
})

test_that("as.numeric() works, which needs an as.double() method", {
  # as.numeric() dispatches through as.double(), so an as.numeric method alone
  # would never be reached
  m <- mixed_substances(c(1, 2), c("mg/dL", "mmol/L"), "glucose")
  expect_equal(as.numeric(m), c(1, 2))
  expect_equal(as.double(m), c(1, 2))
})

test_that("converting to one unit yields a homogeneous substance vector", {
  m <- mixed_substances(c(100, 5.5, 140),
                        c("mg/dL", "mmol/L", "mg/dL"),
                        c("glucose", "glucose", "sodium"))
  y <- set_units(m, "mmol/L")
  expect_s3_class(y, "substances")
  expect_equal(as.numeric(y), c(5.5507, 5.5, 60.897), tolerance = 1e-4)
  expect_equal(substances(y), c("glucose", "glucose", "sodium"))
  expect_equal(unit_label(y), "mmol/L")
})

test_that("conversion handles a unit that needs no substance", {
  m <- mixed_substances(c(1, 1), c("g/dL", "g/L"), c("albumin", NA))
  y <- set_units(m, "g/L")
  expect_equal(as.numeric(y), c(10, 1))
})

test_that("a target unit is required", {
  m <- mixed_substances(1, "mg/dL", "glucose")
  expect_error(set_units(m), "a target unit is required", fixed = TRUE)
})

test_that("set_units() accepts a unit held in a variable", {
  m <- mixed_substances(1, "mg/dL", "glucose")
  target <- "mmol/L"
  expect_equal(as.numeric(set_units(m, target, mode = "standard")),
               0.05550745, tolerance = 1e-6)
})

test_that("unit strings udunits does not know are rejected", {
  expect_error(mixed_substances(1, "frac of 1", "glucose"),
               "not recognised by udunits", fixed = TRUE)
  expect_error(mixed_substances(1, "Hb Fract.", "glucose"),
               "not recognised by udunits", fixed = TRUE)
})

test_that("unknown substances are rejected", {
  expect_error(mixed_substances(1, "mg/dL", "unobtainium"),
               "unknown substance", fixed = TRUE)
})

test_that("format and print show unit and substance per element", {
  m <- mixed_substances(c(1, 2), c("mg/dL", "mmol/L"), c("glucose", NA))
  f <- format(m)
  expect_match(f[1], "mg/dL", fixed = TRUE)
  expect_match(f[1], "glucose", fixed = TRUE)
  expect_match(f[2], "?", fixed = TRUE)          # unknown substance
  expect_output(print(m), "mixed_substances")
})

test_that("subsetting and combining preserve unit and substance per element", {
  m <- mixed_substances(c(1, 2, 3), c("mg/dL", "mmol/L", "g/L"),
                        c("glucose", "sodium", "albumin"))
  expect_equal(substance_units(m[2:3]), c("mmol/L", "g/L"))
  expect_equal(substances(m[2:3]), c("sodium", "albumin"))
  expect_equal(substances(c(m, m)), rep(c("glucose", "sodium", "albumin"), 2))
  expect_equal(substance_units(rev(m)), c("g/L", "mmol/L", "mg/dL"))
})

test_that("combining with something that is not a quantity is refused", {
  m <- mixed_substances(c(1, 2), c("mg/dL", "mmol/L"), "glucose")
  expect_error(c(m, 1), "cannot combine a `mixed_substances` vector with a",
               fixed = TRUE)
  expect_error(c(m, units::set_units(1, "mg/dL")),
               "cannot combine a `mixed_substances` vector with a", fixed = TRUE)
})

test_that("rep() and [[ keep the unit and substance per element", {
  m <- mixed_substances(c(1, 2), c("mg/dL", "mmol/L"), c("glucose", "sodium"))
  expect_equal(substance_units(rep(m, 2)),
               rep(c("mg/dL", "mmol/L"), 2))
  expect_equal(substances(rep(m, times = 2)),
               rep(c("glucose", "sodium"), 2))
  expect_equal(substances(m[[2]]), "sodium")
  expect_equal(substance_units(m[[2]]), "mmol/L")
})

test_that("combining across systems is refused", {
  substance_system("mixed_iso",
                   substances = data.frame(substance_id = "glucose",
                                           name = "Glucose"))
  a <- mixed_substances(1, "mg/dL", "glucose")
  b <- mixed_substances(1, "mg/dL", "glucose", system = "mixed_iso")
  expect_error(c(a, b), "different systems", fixed = TRUE)
})

test_that("arguments recycle to the length of x", {
  m <- mixed_substances(c(1, 2, 3), "mg/dL", "glucose")
  expect_equal(substance_units(m), rep("mg/dL", 3))
  expect_equal(substances(m), rep("glucose", 3))
  expect_error(mixed_substances(c(1, 2, 3), c("mg/dL", "mmol/L"), "glucose"))
})

test_that("the accessors have no method for unrelated classes", {
  expect_error(substance_units(1:3), "no `substance_units()` method",
               fixed = TRUE)
  expect_error(substances(1:3), "no `substances()` method", fixed = TRUE)
  expect_error(drop_substances(1:3), "no `drop_substances()` method",
               fixed = TRUE)
})

test_that("the default symbols mode accepts a bare unit expression", {
  m <- mixed_substances(c(100, 5.5), c("mg/dL", "mmol/L"), "glucose")
  expect_equal(as.numeric(set_units(m, mmol/L)), c(5.5507, 5.5),
               tolerance = 1e-4)
})

test_that("unit validation is per distinct string, not per element", {
  # a long column repeats a handful of units thousands of times; validating
  # each row made construction the slowest step in the package
  units_rep <- rep(c("mg/dL", "mmol/L"), each = 5000)
  elapsed <- system.time(
    mixed_substances(seq_along(units_rep), units_rep, "glucose"))[["elapsed"]]
  expect_lt(elapsed, 1)

  # and the message still names every distinct bad string, once
  err <- tryCatch(mixed_substances(1:4, c("mg/dL", "frac of 1", "Hb Fract.",
                                          "frac of 1"), "glucose"),
                  error = conditionMessage)
  expect_match(err, "frac of 1", fixed = TRUE)
  expect_match(err, "Hb Fract.", fixed = TRUE)
  expect_equal(lengths(regmatches(err, gregexpr("frac of 1", err, fixed = TRUE))),
               1L)
})
