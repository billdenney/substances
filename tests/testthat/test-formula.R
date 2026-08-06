test_that("molar mass is computed from a formula", {
  expect_equal(as.numeric(molar_mass_from_formula("H2O")), 18.015,
               tolerance = 1e-4)
  expect_equal(as.numeric(molar_mass_from_formula("C6H12O6")), 180.156,
               tolerance = 1e-4)
  expect_equal(as.numeric(molar_mass_from_formula("C27H46O")), 386.664,
               tolerance = 1e-4)
  expect_equal(as.numeric(molar_mass_from_formula("CO2")), 44.009,
               tolerance = 1e-4)
})

test_that("results carry g/mol and vectorise", {
  x <- molar_mass_from_formula(c("H2O", "CO2"))
  expect_s3_class(x, "units")
  expect_equal(as.character(units(x)), "g/mol")
  expect_length(x, 2L)
})

test_that("a single atom is its atomic weight", {
  expect_equal(as.numeric(molar_mass_from_formula("Na")), 22.98976928,
               tolerance = 1e-8)
})

test_that("a trailing ionic charge is ignored", {
  expect_equal(as.numeric(molar_mass_from_formula("Mg+2")),
               as.numeric(molar_mass_from_formula("Mg")))
  expect_equal(as.numeric(molar_mass_from_formula("Cl-")),
               as.numeric(molar_mass_from_formula("Cl")))
})

test_that("unsupported formulae error rather than being mis-parsed", {
  expect_error(molar_mass_from_formula("Ca(OH)2"), "not supported", fixed = TRUE)
  expect_error(molar_mass_from_formula("CuSO4.5H2O"), "not supported",
               fixed = TRUE)
  expect_error(molar_mass_from_formula("Xx2"), "no atomic weight registered",
               fixed = TRUE)
})

test_that("NA and empty formulae give NA, not an error", {
  expect_true(is.na(as.numeric(molar_mass_from_formula(NA_character_))))
})

test_that("computed masses agree with PubChem to its stated precision", {
  # PubChem reports 3-5 significant figures; agreement to that is the check
  reference <- c("C6H12O6" = 180.16, "C27H46O" = 386.7, "C57H104O6" = 885.4,
                 "C33H36N4O6" = 584.7, "C4H8O3" = 104.10, "C4H7N3O" = 113.12,
                 "CH4N2O" = 60.056, "C5H4N4O3" = 168.11, "C3H6O3" = 90.08,
                 "CO2" = 44.009, "H2O" = 18.015, "C2H6O" = 46.07,
                 "C8H9NO2" = 151.16)
  for (f in names(reference)) {
    got <- as.numeric(molar_mass_from_formula(f))
    digits <- nchar(sub("^[0-9]*[.]?", "", as.character(reference[[f]])))
    expect_equal(round(got, digits), reference[[f]],
                 tolerance = 1.5 * 10^(-digits), info = f)
  }
})

test_that("a formula that is not element symbols at all is refused", {
  expect_error(molar_mass_from_formula("2H2O"), "cannot parse formula",
               fixed = TRUE)
  expect_error(molar_mass_from_formula("h2o"), "cannot parse formula",
               fixed = TRUE)
})

test_that("an element with no registered molar mass is reported", {
  substance_system("no_mass",
                   substances = data.frame(substance_id = "carbon",
                                           name = "Carbon", formula = "C"),
                   synonyms = data.frame(substance_id = "carbon", synonym = "C",
                                         context = "element symbol",
                                         source_id = NA))
  expect_error(molar_mass_from_formula("C", system = "no_mass"),
               "no atomic weight registered", fixed = TRUE)
})

test_that("formulae with unusual elements parse and agree with PubChem", {
  # exercises I, Co, Cl, F, S and a formula written hydrogen-first
  reference <- c("C15H11I4NO4" = 776.87,        # thyroxine, 4 iodine
                 "C63H88CoN14O14P" = 1355.4,    # cobalamin, cobalt not C+o
                 "C66H75Cl2N9O24" = 1449.2,     # vancomycin, chlorine
                 "C22H29FO5" = 392.5,           # dexamethasone, fluorine
                 "C9H7Cl2N5" = 256.09,          # lamotrigine
                 "C4H9NO2S" = 135.19,           # homocysteine, sulfur
                 "C62H111N11O12" = 1202.6,      # ciclosporin
                 "H3N" = 17.031)                # ammonia, not N-first
  for (f in names(reference)) {
    got <- as.numeric(molar_mass_from_formula(f))
    expect_equal(got, reference[[f]], tolerance = 1e-3, info = f)
  }
})

test_that("Co is read as cobalt, not carbon followed by oxygen", {
  # the one place a two-letter symbol could be mis-split
  co <- as.numeric(molar_mass_from_formula("Co"))
  expect_equal(co, 58.933194, tolerance = 1e-6)
  expect_false(isTRUE(all.equal(co, as.numeric(molar_mass_from_formula("CO")))))
})
