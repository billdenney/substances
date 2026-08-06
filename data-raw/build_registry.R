## Build the CSV registry shipped in inst/extdata.
##
## Run from the package root:  Rscript data-raw/build_registry.R
##
## This bootstraps by sourcing R/ directly rather than loading the package,
## because .onLoad() reads the very CSVs this script writes.
##
## Molar masses of ordinary molecules are COMPUTED from their formula and the
## atomic weights, not transcribed. That way a reviewer can re-derive every one
## of them, and the only values needing an external citation are the atomic
## weights themselves and the things that have no formula (proteins, and
## activity standards).

library(units)
library(vctrs)

for (f in list.files("R", pattern = "[.][Rr]$", full.names = TRUE)) source(f)
assign("systems", list(), envir = substances_env)
assign("default_system", "bootstrap", envir = substances_env)
install_extra_units()

out_dir <- "inst/extdata"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

## ---------------------------------------------------------------- sources ---
sources <- data.frame(stringsAsFactors = FALSE, rbind(
  c("ciaaw-2021", "CIAAW, Standard Atomic Weights 2021 (conventional values)",
    "https://ciaaw.org/atomic-weights.htm", "2026-08-05"),
  c("periodictable-pkg",
    "R package 'PeriodicTable' (values predate the 2009 IUPAC revision; see note)",
    "https://cran.r-project.org/package=PeriodicTable", "2023-10-21"),
  c("computed-from-formula",
    "Computed from the molecular formula and CIAAW 2021 atomic weights", "", "2026-08-05"),
  c("ngsp-master-eq", "NGSP, IFCC Standardization: IFCC and NGSP",
    "https://ngsp.org/ifccngsp.asp", "2026-08-05"),
  c("who-is-66-304",
    "WHO 1st International Standard for Insulin, Human, recombinant DNA (66/304)",
    "https://www.nibsc.org/", "2026-08-05"),
  c("uniprot-P01308", "UniProt P01308 (INS_HUMAN), mature insulin chains",
    "https://www.uniprot.org/uniprotkb/P01308", "2026-08-05"),
  c("uniprot-P02768", "UniProt P02768 (ALBU_HUMAN), mature chain",
    "https://www.uniprot.org/uniprotkb/P02768", "2026-08-05"),
  c("pubchem", "PubChem Compound database",
    "https://pubchem.ncbi.nlm.nih.gov/", "2026-08-05")))
names(sources) <- c("source_id", "citation", "url", "accessed")

## --------------------------------------------------------------- elements ---
elements <- read.csv("inst/extdata/chemical_elements.csv", stringsAsFactors = FALSE)

# CIAAW 2021 conventional atomic weights for the elements that clinical and
# environmental data actually uses. The rest keep their inherited values with
# honest provenance until the whole table is regenerated from CIAAW.
ciaaw <- c(H = 1.008, Li = 6.94, C = 12.011, N = 14.007, O = 15.999,
           F = 18.998403162, Na = 22.98976928, Mg = 24.305, Al = 26.9815384,
           P = 30.973761998, S = 32.06, Cl = 35.45, K = 39.0983, Ca = 40.078,
           Fe = 55.845, Cu = 63.546, Zn = 65.38, Se = 78.971, Br = 79.904,
           I = 126.90447)

element_substances <- data.frame(
  stringsAsFactors = FALSE,
  substance_id = tolower(elements$substance_name),
  name         = elements$substance_name,
  formula      = elements$substance_identifier,
  cas          = NA_character_,
  inchikey     = NA_character_,
  pubchem_cid  = NA_character_)

element_parameters <- data.frame(
  stringsAsFactors = FALSE,
  substance_id = element_substances$substance_id,
  parameter    = "molar_mass",
  value        = elements$substance_g / elements$substance_mol,
  unit         = "g/mol",
  status       = "ok",
  source_id    = "periodictable-pkg",
  note         = NA_character_)

hit <- match(elements$substance_identifier, names(ciaaw))
element_parameters$value[!is.na(hit)] <- ciaaw[hit[!is.na(hit)]]
element_parameters$source_id[!is.na(hit)] <- "ciaaw-2021"
element_parameters$note[is.na(hit)] <-
  "inherited value, predates the 2009 IUPAC revision; regenerate from CIAAW"

# element symbols as synonyms, so formulae and "Na" both resolve
element_synonyms <- data.frame(
  stringsAsFactors = FALSE,
  substance_id = element_substances$substance_id,
  synonym      = elements$substance_identifier,
  context      = "element symbol",
  source_id    = NA_character_)

## ------------------------------------------------------- molecules, cited ---
mol <- function(id, name, formula, cas = NA, inchikey = NA, cid = NA)
  data.frame(stringsAsFactors = FALSE, substance_id = id, name = name,
             formula = formula, cas = cas, inchikey = inchikey,
             pubchem_cid = as.character(cid))

molecules <- rbind(
  mol("glucose", "D-Glucose", "C6H12O6", "50-99-7",
      "WQZGKKKJIJFFOK-GASJEMHNSA-N", 5793),
  mol("cholesterol", "Cholesterol", "C27H46O", "57-88-5",
      "HVYWMOMLDIMFJA-DPAQBDIFSA-N", 5997),
  mol("triolein", "Triolein", "C57H104O6", NA,
      "PHYFQTYBJUILEZ-IUPFWZBJSA-N", 5497163),
  mol("bilirubin", "Bilirubin", "C33H36N4O6", NA,
      "BPYKTIZUTYGOLE-IFADSCNNSA-N", 5280352),
  mol("3_hydroxybutyrate", "3-Hydroxybutyric acid", "C4H8O3", NA,
      "WHBMMWSBFZVSSR-UHFFFAOYSA-N", 441),
  mol("creatinine", "Creatinine", "C4H7N3O", NA,
      "DDRJAANPRJIHGJ-UHFFFAOYSA-N", 588),
  mol("urea", "Urea", "CH4N2O", NA, "XSQUKJJJFZCRTK-UHFFFAOYSA-N", 1176),
  mol("uric_acid", "Uric acid", "C5H4N4O3", NA,
      "LEHOTFFKMJEONL-UHFFFAOYSA-N", 1175),
  mol("lactic_acid", "Lactic acid", "C3H6O3", NA,
      "JVTAAEKCZFNVCJ-UHFFFAOYSA-N", 612),
  mol("carbon_dioxide", "Carbon dioxide", "CO2", "124-38-9",
      "CURLTUGMZLYLDI-UHFFFAOYSA-N", 280),
  mol("water", "Water", "H2O", "7732-18-5",
      "XLYOFNOQVPJJNP-UHFFFAOYSA-N", 962),
  mol("ethanol", "Ethanol", "C2H6O", "64-17-5",
      "LFQSCWFLJHTTHZ-UHFFFAOYSA-N", 702),
  mol("acetaminophen", "Acetaminophen", "C8H9NO2", "103-90-2",
      "RZVAJINKPMORJF-UHFFFAOYSA-N", 1983),
  mol("bicarbonate", "Bicarbonate", "CHO3", "71-52-3", NA, 769),
  # no usable formula -> external citation required
  mol("insulin", "Insulin (human)", NA, "11061-68-0", NA, NA),
  mol("c_peptide", "C-peptide (human)", NA, NA, NA, NA),
  mol("albumin", "Serum albumin (human)", NA, NA, NA, NA),
  mol("hba1c", "Haemoglobin A1c", NA, NA, NA, NA),
  mol("lipoprotein_a", "Lipoprotein(a)", NA, NA, NA, NA))

## Build an elements-only system so formulae can be evaluated.
substance_system("bootstrap",
                 substances = rbind(element_substances, molecules),
                 synonyms = element_synonyms,
                 parameters = element_parameters,
                 sources = sources)

has_formula <- !is.na(molecules$formula)
computed <- data.frame(
  stringsAsFactors = FALSE,
  substance_id = molecules$substance_id[has_formula],
  parameter    = "molar_mass",
  value        = signif(as.numeric(molar_mass_from_formula(
                   molecules$formula[has_formula], system = "bootstrap")), 7),
  unit         = "g/mol",
  status       = "ok",
  source_id    = "computed-from-formula",
  note         = paste0(molecules$formula[has_formula],
                        " from CIAAW 2021 atomic weights"))

## Parameters that cannot be computed.
literal <- function(id, parameter, value, unit, status, source_id, note = NA)
  data.frame(stringsAsFactors = FALSE, substance_id = id, parameter = parameter,
             value = value, unit = unit, status = status, source_id = source_id,
             note = note)

extra_parameters <- rbind(
  # valences, for mEq conversions
  literal("sodium", "valence", 1, "eq/mol", "ok", "ciaaw-2021", "Na+"),
  literal("potassium", "valence", 1, "eq/mol", "ok", "ciaaw-2021", "K+"),
  literal("chlorine", "valence", 1, "eq/mol", "ok", "ciaaw-2021",
          "Cl-; magnitude of the charge"),
  literal("calcium", "valence", 2, "eq/mol", "ok", "ciaaw-2021", "Ca2+"),
  literal("magnesium", "valence", 2, "eq/mol", "ok", "ciaaw-2021", "Mg2+"),
  literal("bicarbonate", "valence", 1, "eq/mol", "ok", "computed-from-formula",
          "HCO3-"),
  # proteins
  literal("insulin", "molar_mass", 5807.57, "g/mol", "ok", "uniprot-P01308",
          "mature A+B chains with three disulfide bonds"),
  literal("insulin", "activity", 6.00e-9, "mol/IU", "ok", "who-is-66-304",
          paste("1 IU = 6.00 nmol; this, not the molar mass, is what relates",
                "mU/L to pmol/L")),
  literal("c_peptide", "molar_mass", 3020.29, "g/mol", "ok", "uniprot-P01308",
          "human C-peptide, 31 residues"),
  literal("albumin", "molar_mass", 66437, "g/mol", "ok", "uniprot-P02768",
          "mature chain, 585 residues"),
  # a conversion that should not be made
  literal("lipoprotein_a", "molar_mass", NA, "g/mol", "disputed", NA,
          paste("apo(a) isoform size varies between individuals, so there is no",
                "valid fixed mass<->molar factor; measure nmol/L directly")))

parameters <- rbind(element_parameters, computed, extra_parameters)

## ------------------------------------------------------------ conversions ---
conversions <- data.frame(
  stringsAsFactors = FALSE,
  substance_id = "hba1c",
  from_unit    = "mmol/mol",
  to_unit      = "%",
  kind         = "affine",
  slope        = 0.09148,
  intercept    = 2.152,
  status       = "ok",
  source_id    = "ngsp-master-eq",
  note = paste("NGSP master equation, stored in the published direction;",
               "the inverse is derived, not transcribed"))

## --------------------------------------------------------------- synonyms ---
syn <- function(id, ...) data.frame(stringsAsFactors = FALSE,
  substance_id = id, synonym = c(...), context = "clinical",
  source_id = NA_character_)

molecule_synonyms <- rbind(
  syn("glucose", "Glucose", "Blood glucose", "Fasting plasma glucose", "FPG"),
  syn("cholesterol", "Cholesterol", "Total Cholesterol", "HDL Cholesterol",
      "LDL Cholesterol", "Cholesterol (HDL)", "Cholesterol (LDL)", "HDL-C",
      "LDL-C", "Non-HDL Cholesterol"),
  syn("triolein", "Triglycerides", "Triglyceride", "TG"),
  syn("bilirubin", "Total bilirubin", "Bilirubin (Total)", "Direct Bilirubin",
      "Total Bilirubin"),
  syn("3_hydroxybutyrate", "3 hydroxy-butyrate (BOHB)", "BOHB",
      "beta-hydroxybutyrate", "3-hydroxybutyrate"),
  syn("c_peptide", "C-peptide", "C peptide"),
  syn("insulin", "Insulin"),
  syn("albumin", "Albumin", "Serum albumin"),
  syn("hba1c", "HbA1c", "Hemoglobin A1c", "Haemoglobin A1c", "A1c"),
  syn("lipoprotein_a", "Lipoprotein (a) [Lp(a)]", "Lp(a)", "Lipoprotein (a)"),
  syn("creatinine", "Creatinine"),
  syn("urea", "Urea", "Blood urea nitrogen", "BUN"),
  syn("uric_acid", "Uric acid", "Urate"),
  syn("lactic_acid", "Lactate", "Lactic acid"),
  syn("bicarbonate", "Bicarbonate", "HCO3"),
  syn("carbon_dioxide", "Carbon dioxide", "CO2"),
  syn("sodium", "Sodium"), syn("potassium", "Potassium"),
  syn("calcium", "Calcium"), syn("magnesium", "Magnesium"),
  syn("chlorine", "Chloride"))

synonyms <- rbind(element_synonyms, molecule_synonyms)
synonyms <- synonyms[!duplicated(tolower(synonyms$synonym)), ]

## ------------------------------------------------------------------ write ---
substances <- rbind(element_substances, molecules)

write_registry <- function(x, file) {
  utils::write.csv(x, file.path(out_dir, file), row.names = FALSE, na = "")
  message("wrote ", file, " (", nrow(x), " rows)")
}
write_registry(sources, "sources.csv")
write_registry(substances, "substances.csv")
write_registry(synonyms, "substance_synonyms.csv")
write_registry(parameters, "substance_parameters.csv")
write_registry(conversions, "substance_conversions.csv")
