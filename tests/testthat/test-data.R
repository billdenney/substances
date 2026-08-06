## The registry is data, so it needs tests that data can fail. Each of these
## corresponds to a way a hand-maintained conversion table goes wrong.

sys <- get_system("substances")

test_that("every source_id resolves, and every ok value has one", {
  used <- unique(c(sys$parameters$source_id, sys$conversions$source_id,
                   sys$synonyms$source_id))
  used <- used[!is.na(used)]
  expect_setequal(setdiff(used, sys$sources$source_id), character(0))

  needs_source <- sys$parameters[sys$parameters$status == "ok", ]
  expect_true(all(!is.na(needs_source$source_id)),
              info = paste("parameters with no source:",
                           paste(needs_source$substance_id[is.na(needs_source$source_id)],
                                 collapse = ", ")))
  ok_conv <- sys$conversions[sys$conversions$status == "ok", ]
  expect_true(all(!is.na(ok_conv$source_id)))
})

test_that("every unit in the registry parses", {
  for (u in unique(sys$parameters$unit))
    expect_true(unit_is_defined(u), info = u)
  for (u in unique(c(sys$conversions$from_unit, sys$conversions$to_unit)))
    expect_true(unit_is_defined(u), info = u)
})

test_that("every parameter has the units its kind requires", {
  for (i in seq_len(nrow(sys$parameters))) {
    kind <- sys$parameters$parameter[i]
    expect_true(
      units::ud_are_convertible(sys$parameters$unit[i],
                                substance_parameter_units[[kind]]),
      info = paste(sys$parameters$substance_id[i], kind, sys$parameters$unit[i]))
  }
})

test_that("referential integrity holds across the tables", {
  ids <- sys$substances$substance_id
  expect_setequal(setdiff(sys$parameters$substance_id, ids), character(0))
  expect_setequal(setdiff(sys$synonyms$substance_id, ids), character(0))
  expect_setequal(setdiff(sys$conversions$substance_id, ids), character(0))
  expect_false(any(duplicated(ids)))
  expect_false(any(duplicated(sys$parameters[, c("substance_id", "parameter")])))
})

test_that("no synonym claims two different substances", {
  key <- tolower(sys$synonyms$synonym)
  clash <- tapply(sys$synonyms$substance_id, key, function(z) length(unique(z)))
  expect_true(all(clash == 1L),
              info = paste("ambiguous synonyms:",
                           paste(names(clash)[clash > 1L], collapse = ", ")))
})

test_that("no lookup key resolves to two different substances", {
  # substance_id and name are often the same string for elements, which is fine;
  # what must not happen is one key pointing at two different substances
  keys <- data.frame(
    key = tolower(c(sys$substances$substance_id, sys$substances$name,
                    sys$synonyms$synonym)),
    id = c(sys$substances$substance_id, sys$substances$substance_id,
           sys$synonyms$substance_id))
  clash <- tapply(keys$id, keys$key, function(z) length(unique(z)))
  expect_true(all(clash == 1L),
              info = paste("ambiguous keys:",
                           paste(names(clash)[clash > 1L], collapse = ", ")))
})

test_that("every formula-derived molar mass recomputes from the atomic weights", {
  # this is what makes those values reviewable rather than transcribed
  derived <- sys$parameters[sys$parameters$source_id %in% "computed-from-formula" &
                              sys$parameters$parameter == "molar_mass", ]
  expect_gt(nrow(derived), 10L)
  for (i in seq_len(nrow(derived))) {
    formula <- sys$substances$formula[
      match(derived$substance_id[i], sys$substances$substance_id)]
    expect_equal(derived$value[i],
                 as.numeric(molar_mass_from_formula(formula)),
                 tolerance = 1e-5, info = paste(derived$substance_id[i], formula))
  }
})

test_that("every substance with a molar mass round-trips mg/dL to mmol/L", {
  have <- sys$parameters$substance_id[sys$parameters$parameter == "molar_mass" &
                                        sys$parameters$status == "ok" &
                                        !is.na(sys$parameters$value)]
  for (id in have) {
    x <- substance(100, "mg/dL", id)
    expect_equal(as.numeric(set_units(set_units(x, "mmol/L"), "mg/dL")), 100,
                 tolerance = 1e-9, info = id)
  }
})

test_that("a disputed parameter carries a note explaining why", {
  disputed <- sys$parameters[sys$parameters$status != "ok", ]
  expect_gt(nrow(disputed), 0L)
  expect_true(all(!is.na(disputed$note) & nzchar(disputed$note)))
})

test_that("reading an absent or empty registry file is an error", {
  dir <- withr_tempdir()
  expect_error(substance_read_csv("substances", path = dir),
               "registry file not found", fixed = TRUE)
  writeLines("substance_id,name", file.path(dir, "substances.csv"))
  expect_error(substance_read_csv("substances", path = dir),
               "registry file is empty", fixed = TRUE)
})

test_that("substance_read_csv() reads each shipped table from the install", {
  for (tbl in registry_tables) {
    d <- substance_read_csv(tbl)
    expect_s3_class(d, "data.frame")
    expect_gt(nrow(d), 0L)
  }
  expect_error(substance_read_csv("nope"), "'arg' should be one of")
})

test_that("the shipped registry can be rebuilt from the CSVs", {
  sys2 <- substance_load_default("rebuilt")
  expect_s3_class(sys2, "substance_system")
  expect_equal(nrow(sys2$substances), nrow(sys$substances))
  expect_equal(nrow(sys2$parameters), nrow(sys$parameters))
  # and it is a usable system, not just a parsed one
  expect_equal(as.numeric(set_units(substance(100, "mg/dL", "glucose",
                                              system = "rebuilt"), "mmol/L")),
               5.5507, tolerance = 1e-4)
})

test_that("the loaded default system is the one the CSVs describe", {
  expect_true("substances" %in% substance_systems())
  expect_equal(substance_default_system(), "substances")
  expect_gt(nrow(sys$substances), 100L)   # elements plus the clinical set
})
