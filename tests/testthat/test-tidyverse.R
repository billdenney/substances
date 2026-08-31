## vctrs and pillar are optional. Everything here has to keep working when they
## are absent, which is why each block skips rather than failing, and why the
## base R methods are tested separately in test-misc.R.

test_that("nothing in the package depends on vctrs or pillar", {
  # the methods are registered into their namespaces at load time, so neither
  # package is imported and neither is needed to use this one
  imports <- names(getNamespaceImports(asNamespace("substances")))
  expect_false("vctrs" %in% imports)
  expect_false("pillar" %in% imports)
  expect_setequal(setdiff(imports, c("", "base")), c("units", "stats", "utils"))
})

test_that("vctrs slicing carries the substance", {
  skip_if_not_installed("vctrs")
  x <- set_substances(c(1, 2, 3), c("glucose", "sodium", "urea"), "mmol/L")

  expect_equal(substances(vctrs::vec_slice(x, 2:3)), c("sodium", "urea"))
  expect_equal(substances(vctrs::vec_c(x, x)),
               rep(c("glucose", "sodium", "urea"), 2))
  expect_equal(substances(vctrs::vec_rep(x, 2)),
               rep(c("glucose", "sodium", "urea"), 2))
  expect_equal(substances(vctrs::vec_recycle(x[1], 3)), rep("glucose", 3))
  expect_equal(as.numeric(vctrs::vec_slice(x, 2)), 2)
})

test_that("vctrs combining converts, substance-aware", {
  skip_if_not_installed("vctrs")
  x <- set_substances(1, "glucose", "mg/dL")
  y <- set_substances(1, "glucose", "mmol/L")
  z <- vctrs::vec_c(x, y)
  expect_equal(unit_label(z), "mg/dL")
  expect_equal(as.numeric(z), c(1, 18.0156), tolerance = 1e-4)
})

test_that("vctrs refuses what the base methods refuse", {
  skip_if_not_installed("vctrs")
  x <- set_substances(1, "glucose", "mg/dL")

  substance_system("vctrs_iso",
                   substances = data.frame(substance_id = "glucose",
                                           name = "Glucose"))
  z <- set_substances(1, "glucose", "mg/dL", system = "vctrs_iso")
  expect_error(vctrs::vec_c(x, z), class = "vctrs_error_incompatible_type")

  expect_error(vctrs::vec_c(x, 1), class = "vctrs_error_incompatible_type")

  # units that no conversion reconciles are refused at the cast, with the
  # conversion engine's own message rather than a bare type error
  expect_error(vctrs::vec_c(x, set_substances(1, "glucose", "L")),
               "cannot convert", fixed = TRUE)
})

test_that("a substance column survives a vctrs data frame operation", {
  skip_if_not_installed("vctrs")
  x <- set_substances(c(1, 2), c("glucose", "sodium"), "mmol/L")
  d <- vctrs::data_frame(id = 1:2, x = x)
  expect_equal(substances(vctrs::vec_slice(d, 2)$x), "sodium")
  expect_equal(substances(vctrs::vec_rbind(d, d)$x),
               c("glucose", "sodium", "glucose", "sodium"))
})

test_that("the vctrs type abbreviations name the unit", {
  skip_if_not_installed("vctrs")
  x <- set_substances(1, "glucose", "mg/dL")
  expect_equal(vctrs::vec_ptype_abbr(x), "subst")
  expect_equal(vctrs::vec_ptype_full(x), "substances<mg/dL>")
})

test_that("pillar shows the substance in a tibble column", {
  skip_if_not_installed("pillar")
  x <- set_substances(c(1, 2), c("glucose", "sodium"), "mmol/L")
  # the unit belongs in the header, the substance in the cells
  expect_equal(pillar::type_sum(x), "subst<mmol/L>")
  shaft <- paste(format(pillar::pillar_shaft(x), width = 40), collapse = " ")
  expect_match(shaft, "glucose", fixed = TRUE)
  expect_match(shaft, "sodium", fixed = TRUE)
  expect_false(grepl("mmol/L", shaft, fixed = TRUE))
})
