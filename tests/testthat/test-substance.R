test_that("a substance vector holds a different substance per element", {
  x <- substance(c(100, 140), "mg/dL", c("glucose", "sodium"))
  expect_s3_class(x, "substance")
  expect_equal(length(x), 2L)
  expect_equal(substance_of(x), c("glucose", "sodium"))
  expect_equal(as.numeric(x), c(100, 140))
  expect_equal(unit_label(x), "mg/dL")
})

test_that("substance names are resolved through synonyms, case-insensitively", {
  x <- substance(c(1, 2, 3), "mg/dL",
                 c("LDL Cholesterol", "HDL-C", "cholesterol"))
  expect_equal(substance_of(x), rep("cholesterol", 3))
  expect_equal(substance_resolve("  GLUCOSE "), "glucose")
  expect_equal(substance_resolve("Na"), "sodium")
})

test_that("an unknown substance is an error, not a silent NA", {
  expect_error(substance(1, "mg/dL", "unobtainium"),
               "unknown substance", fixed = TRUE)
})

test_that("the substance survives the whole method surface", {
  # this is the property the plain-attribute representation does not have
  x <- substance(c(1, 2, 3), "mg/dL", c("glucose", "sodium", "glucose"))

  expect_equal(substance_of(x[2:3]), c("sodium", "glucose"))
  expect_equal(substance_of(c(x, x)), rep(c("glucose", "sodium", "glucose"), 2))
  expect_equal(substance_of(rev(x)), c("glucose", "sodium", "glucose"))
  expect_equal(substance_of(rep(x, 2)), rep(c("glucose", "sodium", "glucose"), 2))
  expect_equal(substance_of(x[c(TRUE, FALSE, TRUE)]), c("glucose", "glucose"))

  d <- data.frame(id = 1:3)
  d$x <- x
  expect_equal(substance_of(d[d$id > 1, ]$x), c("sodium", "glucose"))
  expect_equal(substance_of(d[order(-d$id), ]$x), c("glucose", "sodium", "glucose"))
})

test_that("combining different units or systems is an error", {
  x <- substance(1, "mg/dL", "glucose")
  y <- substance(1, "mmol/L", "glucose")
  expect_error(c(x, y), "different units", fixed = TRUE)

  substance_system("isolated",
                   substances = data.frame(substance_id = "glucose",
                                           name = "Glucose"))
  z <- substance(1, "mg/dL", "glucose", system = "isolated")
  expect_error(c(x, z), "different systems", fixed = TRUE)
})

test_that("accessors round-trip", {
  x <- substance(c(100, 140), "mg/dL", c("glucose", "sodium"))
  expect_s3_class(drop_substance(x), "units")
  expect_equal(as.numeric(drop_substance(x)), c(100, 140))
  expect_equal(substance_system_of(x), "substances")

  substance_of(x) <- c("sodium", "glucose")
  expect_equal(substance_of(x), c("sodium", "glucose"))
  expect_error(substance_of(x) <- "unobtainium", "unknown substance")
})

test_that("NA substance is allowed and printed as unknown", {
  x <- substance(c(1, 2), "mg/dL", c("glucose", NA))
  expect_equal(substance_of(x), c("glucose", NA))
  expect_match(format(x)[2], "?", fixed = TRUE)
})

test_that("substance_of() has no method for unrelated classes", {
  expect_error(substance_of(1:3), "no `substance_of\\(\\)` method")
})
