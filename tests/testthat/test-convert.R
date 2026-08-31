test_that("dimensional conversions are delegated to units and need no substance", {
  x <- set_substances(c(1, 2), c("albumin", NA), "g/dL")
  y <- set_units(x, "g/L")
  expect_equal(as.numeric(y), c(10, 20))
  expect_equal(unit_label(y), "g/L")
  expect_equal(substances(y), c("albumin", NA))
})

test_that("molar mass bridges mass concentration to amount concentration", {
  x <- set_substances(100, "glucose", "mg/dL")
  expect_equal(as.numeric(set_units(x, "mmol/L")), 5.5507, tolerance = 1e-4)
  # and the same single parameter serves any other unit pair it can bridge
  expect_equal(as.numeric(set_units(set_substances(1, "glucose", "g"), "mol")),
               1 / 180.156, tolerance = 1e-6)
  expect_equal(as.numeric(set_units(set_substances(1, "glucose", "mol"), "g")),
               180.156, tolerance = 1e-6)
  expect_equal(as.numeric(set_units(set_substances(1, "glucose", "ug/L"), "nmol/L")),
               1000 / 180.156, tolerance = 1e-6)
})

test_that("conversion is per-element, so one call handles a mixed vector", {
  x <- set_substances(c(100, 140), c("glucose", "sodium"), "mg/dL")
  y <- set_units(x, "mmol/L")
  expect_equal(as.numeric(y), c(5.5507, 60.897), tolerance = 1e-4)
  expect_equal(substances(y), c("glucose", "sodium"))
})

test_that("two parameters compose: mg/dL to mEq/L needs molar mass and valence", {
  expect_equal(as.numeric(set_units(set_substances(1, "sodium", "mg/dL"), "meq/L")),
               0.43498, tolerance = 1e-4)
  expect_equal(as.numeric(set_units(set_substances(1, "calcium", "mg/dL"), "meq/L")),
               0.4990, tolerance = 1e-3)
})

test_that("activity, not molar mass, relates insulin mIU/L to pmol/L", {
  expect_equal(as.numeric(set_units(set_substances(1, "insulin", "mIU/L"), "pmol/L")),
               6.0, tolerance = 1e-6)
})

test_that("round trips return the input", {
  for (id in c("glucose", "cholesterol", "bilirubin", "creatinine")) {
    x <- set_substances(100, id, "mg/dL")
    expect_equal(as.numeric(set_units(set_units(x, "mmol/L"), "mg/dL")), 100,
                 tolerance = 1e-9, info = id)
  }
})

test_that("an explicit affine conversion beats the dimensional path", {
  # % and mmol/mol are both dimensionless, so a units-first implementation
  # would convert HbA1c by a factor of 10 instead of by the master equation
  x <- set_substances(c(5, 6.5, 7), "hba1c", "%")
  y <- set_units(x, "mmol/mol")
  expect_equal(as.numeric(y), c(31.132, 47.530, 52.995), tolerance = 1e-3)
  expect_false(isTRUE(all.equal(as.numeric(y), c(50, 65, 70))))
})

test_that("the affine conversion inverts exactly", {
  x <- set_substances(c(31.132, 47.530, 52.995), "hba1c", "mmol/mol")
  expect_equal(as.numeric(set_units(x, "%")), c(5, 6.5, 7), tolerance = 1e-4)
})

test_that("a disputed parameter is refused rather than guessed", {
  x <- set_substances(50, "lipoprotein_a", "mg/dL")
  expect_error(set_units(x, "nmol/L"), "cannot convert", fixed = TRUE)
})

test_that("an impossible conversion names the substance and does not return", {
  expect_error(set_units(set_substances(1, "hba1c", "mg/dL"), "mmol/L"),
               "hba1c", fixed = TRUE)
  expect_error(set_units(set_substances(1, NA, "mg/dL"), "mmol/L"),
               "unknown substance", fixed = TRUE)
})

test_that("substance_convertible() agrees with what set_units() does", {
  expect_true(substance_convertible("mg/dL", "mmol/L", "glucose"))
  expect_true(substance_convertible("mg/dL", "g/L", NA))
  expect_false(substance_convertible("mg/dL", "mmol/L", NA))
  expect_false(substance_convertible("mg/dL", "mmol/L", "lipoprotein_a"))
})

test_that("published clinical factors are reproduced from the registry", {
  # Regression corpus: factors taken from a production pipeline, checked
  # against what the registry derives. Tolerance matches their rounding.
  cases <- list(
    list("glucose",           "mg/dL", "mmol/L", 0.0555),
    list("HDL Cholesterol",   "mg/dL", "mmol/L", 0.0259),
    list("LDL Cholesterol",   "mg/dL", "mmol/L", 0.0259),
    list("Total bilirubin",   "mg/dL", "umol/L", 17.1),
    list("BOHB",              "mg/dL", "mmol/L", 0.0961),
    list("C-peptide",         "ug/L",  "nmol/L", 0.331),
    list("Triglycerides",     "mmol/L", "mg/dL", 88.5),
    list("Insulin",           "mIU/L", "pmol/L", 6.0),
    list("Creatinine",        "mg/dL", "umol/L", 88.4),
    list("Sodium",            "mg/dL", "meq/L",  0.435))
  for (cs in cases) {
    # mode = "standard" because the unit comes from a variable, as in units
    got <- as.numeric(set_units(set_substances(1, cs[[1]], cs[[2]]), cs[[3]],
                                mode = "standard"))
    # 5e-3 relative: these factors are published to three significant figures,
    # so 0.0259 for cholesterol is the rounding of the derived 0.0258622
    expect_equal(got, cs[[4]], tolerance = 5e-3,
                 info = paste(cs[[1]], cs[[2]], "->", cs[[3]]))
  }
})

test_that("ambiguous bridges error rather than picking one", {
  params <- list(molar_mass = units::set_units(180, "g/mol"),
                 bogus = units::set_units(999, "g/mol"))
  expect_error(bridge_factor("mg/dL", "mmol/L", params), "ambiguous")
})

test_that("`units<-` converts in place, like set_units()", {
  x <- set_substances(100, "glucose", "mg/dL")
  units(x) <- "mmol/L"
  expect_equal(as.numeric(x), 5.5507, tolerance = 1e-4)
  expect_equal(unit_label(x), "mmol/L")
})

test_that("set_units() with no unit means unitless, as in units", {
  # dropping a real dimension is refused, exactly as units::set_units() does
  expect_error(set_units(set_substances(1, "glucose", "mg/dL")),
               "cannot convert mg/dL to 1", fixed = TRUE)
  # but an already-unitless quantity is unchanged
  x <- set_substances(1, "glucose", units::unitless)
  expect_equal(unit_label(set_units(x)), "1")
  expect_equal(as.numeric(set_units(x)), 1)
})

test_that("converting to the same unit is a no-op", {
  x <- set_substances(100, "glucose", "mg/dL")
  expect_equal(as.numeric(set_units(x, "mg/dL")), 100)
  expect_true(substance_convertible("mg/dL", "mg/dL", "glucose"))
})

test_that("a conversion marked other than ok is refused with its note", {
  substance_system("disputed_sys",
    substances = data.frame(substance_id = "x", name = "X"),
    conversions = data.frame(substance_id = "x", from_unit = "mg/dL",
                             to_unit = "nmol/L", kind = "factor", slope = 2,
                             intercept = NA, status = "disputed",
                             source_id = NA, note = "isoform size varies"))
  x <- set_substances(1, "x", "mg/dL", system = "disputed_sys")
  expect_error(set_units(x, "nmol/L"), "is marked \"disputed\"", fixed = TRUE)
  expect_error(set_units(x, "nmol/L"), "isoform size varies", fixed = TRUE)
})

test_that("an unsupported conversion kind is refused when the system is built", {
  # caught at registration rather than at conversion, so a registry cannot sit
  # in a session looking usable until someone happens to convert
  expect_error(
    substance_system("badkind_sys",
      substances = data.frame(substance_id = "x", name = "X"),
      conversions = data.frame(substance_id = "x", from_unit = "mg/dL",
                               to_unit = "nmol/L", kind = "spline", slope = 2,
                               intercept = NA, status = "ok", source_id = NA,
                               note = NA)),
    "unsupported conversion kind", fixed = TRUE)
})

test_that("a plain factor conversion applies in both directions", {
  substance_system("factor_sys",
    substances = data.frame(substance_id = "x", name = "X"),
    conversions = data.frame(substance_id = "x", from_unit = "mg/dL",
                             to_unit = "nmol/L", kind = "factor", slope = 4,
                             intercept = NA, status = "ok", source_id = NA,
                             note = NA))
  fwd <- set_substances(2, "x", "mg/dL", system = "factor_sys")
  expect_equal(as.numeric(set_units(fwd, "nmol/L")), 8)
  rev <- set_substances(8, "x", "nmol/L", system = "factor_sys")
  expect_equal(as.numeric(set_units(rev, "mg/dL")), 2)
})

test_that("a vector mixing convertible and unconvertible substances errors", {
  x <- set_substances(c(1, 1), c("glucose", "hba1c"), "mg/dL")
  expect_error(set_units(x, "mmol/L"), "hba1c", fixed = TRUE)
})

test_that("set_units() accepts a unit held in a variable", {
  x <- set_substances(100, "glucose", "mg/dL")
  target <- "mmol/L"
  expect_equal(as.numeric(set_units(x, target, mode = "standard")),
               5.5507, tolerance = 1e-4)
})

test_that("the default symbols mode accepts a bare unit expression", {
  # set_units(x, mmol/L) without quotes is the units-package idiom and the
  # default mode, so it needs to work here too
  x <- set_substances(100, "glucose", "mg/dL")
  expect_equal(as.numeric(set_units(x, mmol/L)), 5.5507, tolerance = 1e-4)
  expect_equal(unit_label(set_units(x, mmol/L)), "mmol/L")

  y <- set_substances(1, "glucose", "g")
  expect_equal(as.numeric(set_units(y, mol)), 1 / 180.156, tolerance = 1e-6)
})

test_that("substance_convertible() sees explicit conversions too", {
  # HbA1c % <-> mmol/mol exists only as a registered affine conversion
  expect_true(substance_convertible("%", "mmol/mol", "hba1c"))
  expect_true(substance_convertible("mmol/mol", "%", "hba1c"))
})

test_that("published factors for the wider clinical set are reproduced", {
  # each of these is a conventional-to-SI factor in common clinical use
  cases <- list(
    list("Cortisol",     "ug/dL", "nmol/L", 27.59),
    list("Testosterone", "ng/dL", "nmol/L", 0.0347),
    list("Estradiol",    "pg/mL", "pmol/L", 3.671),
    list("Thyroxine",    "ug/dL", "nmol/L", 12.87),
    list("Uric acid",    "mg/dL", "umol/L", 59.48),
    list("Urea",         "mg/dL", "mmol/L", 0.1665),
    list("BUN",          "mg/dL", "mmol/L", 0.357),
    list("Ammonia",      "ug/dL", "umol/L", 0.5872),
    list("Digoxin",      "ng/mL", "nmol/L", 1.281),
    list("Vitamin D",    "ng/mL", "nmol/L", 2.496),
    list("Phenytoin",    "ug/mL", "umol/L", 3.964),
    list("Caffeine",     "ug/mL", "umol/L", 5.15),
    list("Homocysteine", "mg/L",  "umol/L", 7.397),
    list("Folate",       "ng/mL", "nmol/L", 2.266),
    list("Ethanol",      "mg/dL", "mmol/L", 0.2171))
  for (cs in cases) {
    got <- as.numeric(set_units(set_substances(1, cs[[1]], cs[[2]]), cs[[3]],
                                mode = "standard"))
    expect_equal(got, cs[[4]], tolerance = 5e-3,
                 info = paste(cs[[1]], cs[[2]], "->", cs[[3]]))
  }
})

test_that("substance_convertible() refuses what set_units() would throw on", {
  # the two used to disagree: a disputed row reported TRUE and then errored
  substance_system("conv_disputed",
    substances = data.frame(substance_id = "x", name = "X"),
    conversions = data.frame(substance_id = "x", from_unit = "mg/dL",
                             to_unit = "nmol/L", kind = "factor", slope = 2,
                             status = "disputed", note = "not settled"))
  expect_false(substance_convertible("mg/dL", "nmol/L", "x",
                                     system = "conv_disputed"))
  expect_error(set_units(set_substances(1, "x", "mg/dL", system = "conv_disputed"),
                         "nmol/L"),
               "is marked \"disputed\"", fixed = TRUE)
})

test_that("a failed conversion quotes the withheld parameter and its reason", {
  # the registry has a great deal to say about Lp(a); it used to say none of it
  err <- tryCatch(set_units(set_substances(50, "Lp(a)", "mg/dL"), "nmol/L"),
                  error = conditionMessage)
  expect_match(err, "withholds a parameter", fixed = TRUE)
  expect_match(err, "disputed", fixed = TRUE)
  expect_match(err, "isoform size varies", fixed = TRUE)

  # and a substance the registry simply lacks says only that
  err2 <- tryCatch(set_units(set_substances(1, "hba1c", "mg/dL"), "mmol/L"),
                   error = conditionMessage)
  expect_false(grepl("withholds a parameter", err2, fixed = TRUE))
})
