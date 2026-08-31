## The base R method surface. These are the methods that make the substance
## survive; without them the attribute is dropped or, worse, left behind at the
## wrong length.

test_that("subsetting and replacement keep the substance aligned", {
  x <- set_substances(c(1, 2, 3), c("glucose", "sodium", "urea"), "mmol/L")

  expect_equal(substances(x[[2]]), "sodium")
  expect_equal(substances(head(x, 2)), c("glucose", "sodium"))
  expect_equal(substances(sort(x, decreasing = TRUE)),
               c("urea", "sodium", "glucose"))
  expect_equal(substances(split(x, c(1, 1, 2))[[1]]), c("glucose", "sodium"))
  expect_equal(substances(as.list(x)[[3]]), "urea")

  x[2] <- set_substances(9, "calcium", "mmol/L")
  expect_equal(substances(x), c("glucose", "calcium", "urea"))
  expect_equal(as.numeric(x), c(1, 9, 3))
})

test_that("assignment converts the incoming unit, substance-aware", {
  x <- set_substances(c(1, 2), "glucose", "mmol/L")
  x[1] <- set_substances(18.0156, "glucose", "mg/dL")   # = 1 mmol/L
  expect_equal(as.numeric(x), c(1, 2), tolerance = 1e-4)
  expect_equal(unit_label(x), "mmol/L")
})

test_that("blanking a value keeps the substance", {
  # NA says the result is missing, not that the row stopped being glucose
  x <- set_substances(c(1, 2), c("glucose", "sodium"), "mmol/L")
  x[1] <- NA
  expect_equal(as.numeric(x), c(NA, 2))
  expect_equal(substances(x), c("glucose", "sodium"))

  is.na(x) <- 2
  expect_equal(substances(x), c("glucose", "sodium"))
})

test_that("assigning something with no substance is refused", {
  x <- set_substances(c(1, 2), "glucose", "mmol/L")
  expect_error(x[1] <- 5, "does not say which substance it is", fixed = TRUE)
  expect_error(x[1] <- units::set_units(5, "mmol/L"),
               "does not say which substance it is", fixed = TRUE)

  substance_system("assign_iso",
                   substances = data.frame(substance_id = "glucose",
                                           name = "Glucose"))
  other <- set_substances(5, "glucose", "mmol/L", system = "assign_iso")
  expect_error(x[1] <- other, "from system 'assign_iso'", fixed = TRUE)
})

test_that("uniqueness compares the value and the substance together", {
  # 5 mmol/L of glucose is not 5 mmol/L of urea
  x <- set_substances(c(5, 5, 5), c("glucose", "glucose", "urea"), "mmol/L")
  expect_equal(duplicated(x), c(FALSE, TRUE, FALSE))
  expect_equal(anyDuplicated(x), 2L)
  expect_equal(substances(unique(x)), c("glucose", "urea"))
  expect_length(unique(x), 2L)
})

test_that("a substance vector survives a data frame round trip", {
  x <- set_substances(c(1, 2, 3), c("glucose", "sodium", "urea"), "mmol/L")
  d <- data.frame(x = x)
  expect_s3_class(d$x, "substances")
  expect_equal(substances(d$x), c("glucose", "sodium", "urea"))

  # rbind() copies attributes, so the substance must come along or the values
  # end up matched to the wrong analyte
  r <- rbind(d, d)
  expect_length(r$x, 6L)
  expect_equal(substances(r$x), rep(c("glucose", "sodium", "urea"), 2))

  expect_equal(substances(as.data.frame(x)[[1]]),
               c("glucose", "sodium", "urea"))
})

test_that("diff() needs one substance, and keeps it", {
  x <- set_substances(c(1, 3, 6), "glucose", "mmol/L")
  expect_equal(as.numeric(diff(x)), c(2, 3))
  expect_equal(substances(diff(x)), c("glucose", "glucose"))
  expect_error(diff(set_substances(c(1, 2), c("glucose", "urea"), "mmol/L")),
               "2 substances", fixed = TRUE)
})

test_that("elementwise maths keeps each element's own substance", {
  x <- set_substances(c(-1, 2), c("glucose", "sodium"), "mmol/L")
  expect_equal(as.numeric(abs(x)), c(1, 2))
  expect_equal(substances(abs(x)), c("glucose", "sodium"))
  expect_equal(substances(round(x)), c("glucose", "sodium"))
  expect_equal(substances(cumsum(x)), c("glucose", "sodium"))
  # units answers sign() with a bare number, and so do we
  expect_false(inherits(sign(x), "substances"))
})

test_that("reductions need one substance, and keep it", {
  x <- set_substances(c(1, 2, 3), "glucose", "mmol/L")
  expect_equal(as.numeric(range(x)), c(1, 3))
  expect_equal(substances(range(x)), c("glucose", "glucose"))
  expect_equal(as.numeric(min(x)), 1)
  expect_equal(as.numeric(max(x)), 3)
  expect_equal(as.numeric(stats::median(x)), 2)
  expect_equal(as.numeric(stats::quantile(x, 0.5)), 2)
  expect_equal(as.numeric(stats::weighted.mean(x, c(1, 0, 0))), 1)
  expect_equal(substances(stats::quantile(x, c(0.25, 0.75))),
               c("glucose", "glucose"))

  # sum() over several vectors goes through c(), so it converts as c() does
  expect_equal(as.numeric(sum(x, set_substances(18.0156, "glucose", "mg/dL"))),
               7, tolerance = 1e-4)
})

test_that("dropping the unit drops the substance with it", {
  x <- set_substances(c(1, 2), "glucose", "mmol/L")
  bare <- units::drop_units(x)
  expect_equal(bare, c(1, 2))
  expect_null(attr(bare, "substance"))
  expect_false(inherits(bare, "substances"))
  expect_false(inherits(bare, "units"))
})

test_that("mixed_units() widens a homogeneous vector", {
  x <- set_substances(c(1, 2), c("glucose", "sodium"), "mmol/L")
  m <- units::mixed_units(x)
  expect_s3_class(m, "mixed_substances")
  expect_equal(substance_units(m), c("mmol/L", "mmol/L"))
  expect_equal(substances(m), c("glucose", "sodium"))
  # and c() accepts the two classes together, widening to mixed
  expect_length(c(m, x), 4L)
})

test_that("recycling is from length one only", {
  # quietly repeating a length-2 substance across 6 values would label four of
  # them wrongly
  expect_error(set_substances(c(1, 2, 3), c("glucose", "sodium"), "mmol/L"),
               "cannot be recycled to 3", fixed = TRUE)
  expect_equal(substances(set_substances(c(1, 2, 3), "glucose", "mmol/L")),
               rep("glucose", 3))
})
