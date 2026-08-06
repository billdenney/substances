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
##
## Inputs are cached files, not live downloads, so the build is reproducible and
## the inputs are reviewable in a diff:
##   data-raw/pubchem_periodictable.csv  from
##     https://pubchem.ncbi.nlm.nih.gov/rest/pug/periodictable/CSV
##   inst/extdata/chemical_elements.csv  as contributed in PR 3

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
    "https://ciaaw.org/atomic-weights.htm", "2026-08-06"),
  c("periodictable-pkg",
    "R package 'PeriodicTable' (values predate the 2009 IUPAC revision; see note)",
    "https://cran.r-project.org/package=PeriodicTable", "2023-10-21"),
  c("computed-from-formula",
    "Computed from the molecular formula and CIAAW 2021 atomic weights", "",
    "2026-08-06"),
  c("pubchem-periodictable",
    "PubChem Periodic Table of Elements (density and standard state)",
    "https://pubchem.ncbi.nlm.nih.gov/periodic-table/", "2026-08-06"),
  c("pubchem", "PubChem Compound database (formula and identifiers)",
    "https://pubchem.ncbi.nlm.nih.gov/", "2026-08-06"),
  c("ngsp-master-eq", "NGSP, IFCC Standardization: IFCC and NGSP",
    "https://ngsp.org/ifccngsp.asp", "2026-08-06"),
  c("who-is-66-304",
    "WHO 1st International Standard for Insulin, Human, recombinant DNA (66/304)",
    "https://www.nibsc.org/", "2026-08-06"),
  c("uniprot-P01308", "UniProt P01308 (INS_HUMAN), mature insulin chains",
    "https://www.uniprot.org/uniprotkb/P01308", "2026-08-06"),
  c("uniprot-P02768", "UniProt P02768 (ALBU_HUMAN), mature chain",
    "https://www.uniprot.org/uniprotkb/P02768", "2026-08-06")))
names(sources) <- c("source_id", "citation", "url", "accessed")

## --------------------------------------------------------------- elements ---
elements <- read.csv("inst/extdata/chemical_elements.csv", stringsAsFactors = FALSE)

# CIAAW 2021 conventional atomic weights for the elements that clinical and
# environmental data actually uses. The rest keep their inherited values with
# honest provenance until the whole table is regenerated from CIAAW.
ciaaw <- c(H = 1.008, Li = 6.94, C = 12.011, N = 14.007, O = 15.999,
           F = 18.998403162, Na = 22.98976928, Mg = 24.305, Al = 26.9815384,
           P = 30.973761998, S = 32.06, Cl = 35.45, K = 39.0983, Ca = 40.078,
           Fe = 55.845, Co = 58.933194, Cu = 63.546, Zn = 65.38, Se = 78.971,
           Br = 79.904, I = 126.90447)

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

## ------------------------------- element density and gas molar volume ------
## How amount, mass and volume are bridged depends on the state of the element.
##
## For a SOLID or LIQUID, density links mass and volume, and combining it with
## the atomic weight gives the volume per mole of atoms -- the ordinary "atomic
## volume" of reference tables.
##
## For a GAS, that route is wrong. Avogadro's law says a mole of any gas
## occupies the same ~22.4 L at STP, so the bridge from amount to volume is the
## molar volume and it barely depends on the substance at all. Deriving volume
## from mass instead makes it look substance-specific, and worse, it silently
## answers a different question: a tabulated element density is a property of
## the element in its STANDARD STATE, which for H, N, O, F and Cl is the
## diatomic molecule, whereas the atomic weight describes a single atom. Pairing
## them gives the volume per mole of ATOMS -- 11.2 L for hydrogen, half of what
## a mole of gas occupies, and a quantity nobody wants, since monatomic hydrogen
## is not something you can have.
##
## So gases carry `molar_volume` and no density, and the diatomic elements are
## registered as the molecules they actually are. Elements not naturally or
## typically handled in elemental form are left out entirely rather than
## carrying values nobody can use.
ptable <- read.csv("data-raw/pubchem_periodictable.csv", stringsAsFactors = FALSE)
ptable$Density <- suppressWarnings(as.numeric(ptable$Density))
## Join on the SYMBOL, not the name: the two sources disagree on at least one
## (W is "Wolfram" in the element table and "Tungsten" in PubChem), and a name
## join silently drops those rows or invents new substances.
ptable$id <- element_substances$substance_id[
  match(ptable$Symbol, elements$substance_identifier)]
ptable <- ptable[!is.na(ptable$id), ]

## Elements available as the element: everything up to bismuth except the two
## with no stable isotope, plus thorium and uranium. This drops the synthetic
## and intensely radioactive ones, which have tabulated densities but no
## practical elemental form.
accessible <- (ptable$AtomicNumber <= 83 & !ptable$AtomicNumber %in% c(43, 61)) |
  ptable$AtomicNumber %in% c(90, 92)
ptable <- ptable[accessible & !is.na(ptable$Density), ]

is_gas <- grepl("^Gas$", ptable$StandardState)

## Elements whose standard state is a diatomic molecule. Their standard-state
## property belongs to the molecule, registered below, not to the atom.
diatomic <- c(hydrogen = "dihydrogen", nitrogen = "dinitrogen",
              oxygen = "dioxygen", fluorine = "difluorine",
              chlorine = "dichlorine", bromine = "dibromine",
              iodine = "diiodine")

atomic_ok <- !ptable$id %in% names(diatomic)

## Solids and liquids, as atoms: density.
element_density <- data.frame(
  stringsAsFactors = FALSE,
  substance_id = ptable$id[atomic_ok & !is_gas],
  parameter    = "density",
  value        = ptable$Density[atomic_ok & !is_gas],
  unit         = "g/mL",
  status       = "ok",
  source_id    = "pubchem-periodictable",
  note         = paste0("standard state ",
                        tolower(ptable$StandardState[atomic_ok & !is_gas]),
                        "; at or near room temperature"))

## Monatomic gases, as atoms: molar volume. The element IS the gas particle
## here, so the atomic entry is the right home for it.
mono_gas <- ptable[atomic_ok & is_gas, ]
mono_mass <- element_parameters$value[
  match(mono_gas$id, element_parameters$substance_id)]
element_molar_volume <- data.frame(
  stringsAsFactors = FALSE,
  substance_id = mono_gas$id,
  parameter    = "molar_volume",
  value        = signif(mono_mass / mono_gas$Density / 1000, 6),   # mL -> L
  unit         = "L/mol",
  status       = "ok",
  source_id    = "pubchem-periodictable",
  note         = paste("monatomic gas; derived as atomic weight / standard-state",
                       "density at STP (0 C, 101.325 kPa)"))

## The atoms of the diatomic elements get neither, and say why.
withheld <- data.frame(
  stringsAsFactors = FALSE,
  substance_id = names(diatomic),
  parameter    = "density",
  value        = NA_real_,
  unit         = "g/mL",
  status       = "wrong_entity",
  source_id    = NA_character_,
  note         = paste0(
    "withheld: the standard state is ", names(diatomic), "'s diatomic form, so ",
    "its density and molar volume belong to ", diatomic, ", not to the atom. ",
    "Pairing a molecular density with an atomic weight would give volume per ",
    "mole of atoms, which for a gas is half the molar volume."))
withheld <- withheld[names(diatomic) %in% element_substances$substance_id, ]

## ------------------------------------------------------- molecules, cited ---
## Formula and identifiers from PubChem; molar mass is computed from the
## formula below, never transcribed.
molecules <- utils::read.csv(stringsAsFactors = FALSE, strip.white = TRUE,
                             text = "
substance_id,name,formula,inchikey,pubchem_cid
glucose,D-Glucose,C6H12O6,WQZGKKKJIJFFOK-GASJEMHNSA-N,5793
galactose,D-Galactose,C6H12O6,WQZGKKKJIJFFOK-SVZMEOIVSA-N,6036
fructose,D-Fructose,C6H12O6,LKDRXBCSQODPBY-VRPWFDPXSA-N,2723872
cholesterol,Cholesterol,C27H46O,HVYWMOMLDIMFJA-DPAQBDIFSA-N,5997
triolein,Triolein,C57H104O6,PHYFQTYBJUILEZ-IUPFWZBJSA-N,5497163
bilirubin,Bilirubin,C33H36N4O6,BPYKTIZUTYGOLE-IFADSCNNSA-N,5280352
3_hydroxybutyrate,3-Hydroxybutyric acid,C4H8O3,WHBMMWSBFZVSSR-UHFFFAOYSA-N,441
acetoacetic_acid,Acetoacetic acid,C4H6O3,WDJHALXBUFZDSR-UHFFFAOYSA-N,96
creatinine,Creatinine,C4H7N3O,DDRJAANPRJIHGJ-UHFFFAOYSA-N,588
creatine,Creatine,C4H9N3O2,CVSVTCORWBXHQV-UHFFFAOYSA-N,586
urea,Urea,CH4N2O,XSQUKJJJFZCRTK-UHFFFAOYSA-N,1176
uric_acid,Uric acid,C5H4N4O3,LEHOTFFKMJEONL-UHFFFAOYSA-N,1175
lactic_acid,Lactic acid,C3H6O3,JVTAAEKCZFNVCJ-UHFFFAOYSA-N,612
pyruvic_acid,Pyruvic acid,C3H4O3,LCTONWCANYUPML-UHFFFAOYSA-N,1060
oxalic_acid,Oxalic acid,C2H2O4,MUBZPKHOEPUJKR-UHFFFAOYSA-N,971
citric_acid,Citric acid,C6H8O7,KRKNYBCHXYNGOX-UHFFFAOYSA-N,311
glycerol,Glycerol,C3H8O3,PEDCQBHIVMGVHV-UHFFFAOYSA-N,753
homocysteine,L-Homocysteine,C4H9NO2S,FFFHZYDWPBMWHY-VKHMYHEASA-N,91552
ammonia,Ammonia,H3N,QGZKDVFQNNGYKY-UHFFFAOYSA-N,222
dihydrogen,Dihydrogen,H2,UFHFLCQGNIYNRP-UHFFFAOYSA-N,783
dinitrogen,Dinitrogen,N2,IJGRMHOSHXDMSA-UHFFFAOYSA-N,947
dioxygen,Dioxygen,O2,MYMOFIZGZYHOMD-UHFFFAOYSA-N,977
difluorine,Difluorine,F2,PXGOKWXKJXAPGV-UHFFFAOYSA-N,24524
dichlorine,Dichlorine,Cl2,KZBUYRJDOAKODT-UHFFFAOYSA-N,24526
dibromine,Dibromine,Br2,GDTBXPJZTBHREO-UHFFFAOYSA-N,24408
diiodine,Diiodine,I2,PNDPGZBMCMUPRI-UHFFFAOYSA-N,807
carbon_dioxide,Carbon dioxide,CO2,CURLTUGMZLYLDI-UHFFFAOYSA-N,280
bicarbonate,Bicarbonate,CHO3,,769
water,Water,H2O,XLYOFNOQVPJJNP-UHFFFAOYSA-N,962
cortisol,Cortisol,C21H30O5,JYGXADMDTFJGBT-VWUMJDOOSA-N,5754
prednisolone,Prednisolone,C21H28O5,OIGNJSKKLXVSLS-VWUMJDOOSA-N,5755
aldosterone,Aldosterone,C21H28O5,PQSUYGKTWSAVDQ-ZVIOFETBSA-N,5839
dexamethasone,Dexamethasone,C22H29FO5,UREBDLICKHMUKA-CXSFZGCWSA-N,5743
testosterone,Testosterone,C19H28O2,MUMGGOZAMZWBJJ-DYKIIFRCSA-N,6013
dhea,Dehydroepiandrosterone,C19H28O2,FMGSKLZLMKYGDP-USOAJAOKSA-N,5881
androstenedione,Androstenedione,C19H26O2,AEMFNILZOJDQLW-QAGGRKNESA-N,6128
estradiol,Estradiol,C18H24O2,VOXZDWNPVJITMN-ZBRFXRBCSA-N,5757
progesterone,Progesterone,C21H30O2,RJKFOVLPORLFTN-LEKSSAKUSA-N,5994
thyroxine,Thyroxine (T4),C15H11I4NO4,XUIIKFGFIJCVMT-LBPRGKRZSA-N,5819
triiodothyronine,Triiodothyronine (T3),C15H12I3NO4,AUYYCJSJGJYCDS-LBPRGKRZSA-N,5920
folate,Folic acid,C19H19N7O6,OVBPIULPVIDEAO-LBPRGKRZSA-N,6037
cobalamin,Cyanocobalamin,C63H88CoN14O14P,FDJOLVPMNUYSCM-WZHZPDAFSA-L,5311498
ascorbic_acid,L-Ascorbic acid,C6H8O6,CIWBSHSKHKDKBQ-JLAZNSOCSA-N,54670067
calcifediol,25-Hydroxyvitamin D3,C27H44O2,JWUBBDSIWDLEOM-DTOXIADCSA-N,5283731
ethanol,Ethanol,C2H6O,LFQSCWFLJHTTHZ-UHFFFAOYSA-N,702
methanol,Methanol,CH4O,OKKJLVBELUTLKV-UHFFFAOYSA-N,887
ethylene_glycol,Ethylene glycol,C2H6O2,LYCAIKOWRPUZTN-UHFFFAOYSA-N,174
acetone,Acetone,C3H6O,CSCPPACGZOOCGX-UHFFFAOYSA-N,180
acetaminophen,Acetaminophen,C8H9NO2,RZVAJINKPMORJF-UHFFFAOYSA-N,1983
salicylic_acid,Salicylic acid,C7H6O3,YGSDEFSMJLZEOE-UHFFFAOYSA-N,338
ibuprofen,Ibuprofen,C13H18O2,HEFNNWSXXWATRW-UHFFFAOYSA-N,3672
caffeine,Caffeine,C8H10N4O2,RYYVLZVUVIJVGH-UHFFFAOYSA-N,2519
theophylline,Theophylline,C7H8N4O2,ZFXYFBGIUFBOJW-UHFFFAOYSA-N,2153
phenytoin,Phenytoin,C15H12N2O2,CXOFVDLJLONNDW-UHFFFAOYSA-N,1775
phenobarbital,Phenobarbital,C12H12N2O3,DDBREPKUVSBGFI-UHFFFAOYSA-N,4763
valproic_acid,Valproic acid,C8H16O2,NIJJYAXOARWZEE-UHFFFAOYSA-N,3121
carbamazepine,Carbamazepine,C15H12N2O,FFGPTBGBLSHEPO-UHFFFAOYSA-N,2554
lamotrigine,Lamotrigine,C9H7Cl2N5,PYZRQGJRPPTADH-UHFFFAOYSA-N,3878
levetiracetam,Levetiracetam,C8H14N2O2,HPHUVLMMVZITSG-ZCFIWIBFSA-N,441341
metformin,Metformin,C4H11N5,XZWYZXLIPXDOLR-UHFFFAOYSA-N,4091
warfarin,Warfarin,C19H16O4,PJVWKTKQMONHTI-UHFFFAOYSA-N,54678486
digoxin,Digoxin,C41H64O14,LTMHDMANZUZIPE-PUGKRICDSA-N,2724385
methotrexate,Methotrexate,C20H22N8O5,FBOZXECLQNJBKD-ZDUSSCGKSA-N,126941
ciclosporin,Ciclosporin A,C62H111N11O12,PMATZTZNYRCHOR-CGLBZJNRSA-N,5284373
tacrolimus,Tacrolimus,C44H69NO12,QJJXYPPXXYFBGM-LFZNUXCKSA-N,445643
vancomycin,Vancomycin,C66H75Cl2N9O24,MYPYJXKWCTUITO-LYRMYLQWSA-N,14969
morphine,Morphine,C17H19NO3,BQJCRHHNABKAKU-KBQPJGBKSA-N,5288826
codeine,Codeine,C18H21NO3,OROGSEYTTFOCAN-DNJOTXNNSA-N,5284371
")

## Things with no usable formula, so an external citation is required.
## Things with no usable formula. `urea_nitrogen` is here because BUN is
## reported as the mass of nitrogen, not of urea: treating "BUN" as a synonym
## of urea would convert mg/dL with urea's molar mass (60.056) and be wrong by
## a factor of 2.14. It is a different substance, not a different name.
proteins <- data.frame(
  stringsAsFactors = FALSE,
  substance_id = c("insulin", "c_peptide", "albumin", "hba1c", "lipoprotein_a",
                   "urea_nitrogen"),
  name = c("Insulin (human)", "C-peptide (human)", "Serum albumin (human)",
           "Haemoglobin A1c", "Lipoprotein(a)", "Urea nitrogen"),
  formula = NA_character_, inchikey = NA_character_,
  pubchem_cid = NA_character_)

molecules$cas <- NA_character_
molecules$cas[molecules$substance_id == "glucose"] <- "50-99-7"
molecules$cas[molecules$substance_id == "cholesterol"] <- "57-88-5"
molecules$cas[molecules$substance_id == "water"] <- "7732-18-5"
molecules$cas[molecules$substance_id == "carbon_dioxide"] <- "124-38-9"
molecules$cas[molecules$substance_id == "ethanol"] <- "64-17-5"
proteins$cas <- NA_character_

cols <- c("substance_id", "name", "formula", "cas", "inchikey", "pubchem_cid")
molecules <- molecules[, cols]
proteins <- proteins[, cols]

## Build an elements-only system so formulae can be evaluated.
substance_system("bootstrap",
                 substances = rbind(element_substances, molecules, proteins),
                 synonyms = element_synonyms,
                 parameters = element_parameters,
                 sources = sources)

computed <- data.frame(
  stringsAsFactors = FALSE,
  substance_id = molecules$substance_id,
  parameter    = "molar_mass",
  value        = signif(as.numeric(molar_mass_from_formula(
                   molecules$formula, system = "bootstrap")), 7),
  unit         = "g/mol",
  status       = "ok",
  source_id    = "computed-from-formula",
  note         = paste0(molecules$formula, " from CIAAW 2021 atomic weights"))

## Parameters that cannot be computed.
literal <- function(id, parameter, value, unit, status, source_id, note = NA)
  data.frame(stringsAsFactors = FALSE, substance_id = id, parameter = parameter,
             value = value, unit = unit, status = status, source_id = source_id,
             note = note)

extra_parameters <- rbind(
  # valences, for mEq conversions
  literal("sodium", "valence", 1, "eq/mol", "ok", "ciaaw-2021", "Na+"),
  literal("potassium", "valence", 1, "eq/mol", "ok", "ciaaw-2021", "K+"),
  literal("lithium", "valence", 1, "eq/mol", "ok", "ciaaw-2021", "Li+"),
  literal("chlorine", "valence", 1, "eq/mol", "ok", "ciaaw-2021",
          "Cl-; magnitude of the charge"),
  literal("calcium", "valence", 2, "eq/mol", "ok", "ciaaw-2021", "Ca2+"),
  literal("magnesium", "valence", 2, "eq/mol", "ok", "ciaaw-2021", "Mg2+"),
  literal("zinc", "valence", 2, "eq/mol", "ok", "ciaaw-2021", "Zn2+"),
  literal("copper", "valence", 2, "eq/mol", "ok", "ciaaw-2021", "Cu2+"),
  literal("bicarbonate", "valence", 1, "eq/mol", "ok", "computed-from-formula",
          "HCO3-"),
  # a valence that cannot be stated without the oxidation state
  literal("iron", "valence", NA, "eq/mol", "disputed", NA,
          paste("iron circulates as both Fe2+ and Fe3+, so mEq is ambiguous",
                "unless the oxidation state is specified")),
  # proteins and activity standards
  literal("insulin", "molar_mass", 5807.57, "g/mol", "ok", "uniprot-P01308",
          "mature A+B chains with three disulfide bonds"),
  literal("insulin", "activity", 6.00e-9, "mol/IU", "ok", "who-is-66-304",
          paste("1 IU = 6.00 nmol; this, not the molar mass, is what relates",
                "mU/L to pmol/L")),
  literal("c_peptide", "molar_mass", 3020.29, "g/mol", "ok", "uniprot-P01308",
          "human C-peptide, 31 residues"),
  literal("albumin", "molar_mass", 66437, "g/mol", "ok", "uniprot-P02768",
          "mature chain, 585 residues"),
  literal("urea_nitrogen", "molar_mass", 28.014, "g/mol", "ok", "ciaaw-2021",
          paste("2 x N (14.007) per urea molecule. BUN is reported as nitrogen",
                "mass, so this converts a BUN mass concentration to a urea",
                "molar concentration: 1 mg/dL BUN = 0.357 mmol/L urea")),
  # a conversion that should not be made
  literal("lipoprotein_a", "molar_mass", NA, "g/mol", "disputed", NA,
          paste("apo(a) isoform size varies between individuals, so there is no",
                "valid fixed mass<->molar factor; measure nmol/L directly")))

## The standard-state figure, now attached to the molecule it actually
## describes. The gases get a molar volume, so dihydrogen lands on ~22.4 L/mol
## alongside helium and neon as Avogadro's law requires; bromine and iodine are
## a liquid and a solid, so density is the right bridge for them.
di_row <- match(names(diatomic), ptable$id)
di_mass <- as.numeric(molar_mass_from_formula(
  c("H2", "N2", "O2", "F2", "Cl2", "Br2", "I2"), system = "bootstrap"))
di_is_gas <- grepl("^Gas$", ptable$StandardState[di_row])

diatomic_gas_volume <- data.frame(
  stringsAsFactors = FALSE,
  substance_id = unname(diatomic[di_is_gas]),
  parameter    = "molar_volume",
  value        = signif(di_mass[di_is_gas] / ptable$Density[di_row][di_is_gas] / 1000, 6),
  unit         = "L/mol",
  status       = "ok",
  source_id    = "pubchem-periodictable",
  note         = paste("diatomic gas; derived as molecular weight /",
                       "standard-state density at STP (0 C, 101.325 kPa)"))

diatomic_condensed_density <- data.frame(
  stringsAsFactors = FALSE,
  substance_id = unname(diatomic[!di_is_gas]),
  parameter    = "density",
  value        = ptable$Density[di_row][!di_is_gas],
  unit         = "g/mL",
  status       = "ok",
  source_id    = "pubchem-periodictable",
  note         = paste0("standard state ",
                        tolower(ptable$StandardState[di_row][!di_is_gas]),
                        "; at or near room temperature"))

parameters <- rbind(element_parameters, element_density, element_molar_volume,
                    withheld, diatomic_gas_volume, diatomic_condensed_density,
                    computed, extra_parameters)

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
  syn("galactose", "Galactose"),
  syn("fructose", "Fructose"),
  syn("cholesterol", "Cholesterol", "Total Cholesterol", "HDL Cholesterol",
      "LDL Cholesterol", "Cholesterol (HDL)", "Cholesterol (LDL)", "HDL-C",
      "LDL-C", "Non-HDL Cholesterol"),
  syn("triolein", "Triglycerides", "Triglyceride", "TG"),
  syn("bilirubin", "Total bilirubin", "Bilirubin (Total)", "Direct Bilirubin",
      "Total Bilirubin"),
  syn("3_hydroxybutyrate", "3 hydroxy-butyrate (BOHB)", "BOHB",
      "beta-hydroxybutyrate", "3-hydroxybutyrate"),
  syn("acetoacetic_acid", "Acetoacetate"),
  syn("c_peptide", "C-peptide", "C peptide"),
  syn("insulin", "Insulin"),
  syn("albumin", "Albumin", "Serum albumin"),
  syn("hba1c", "HbA1c", "Hemoglobin A1c", "Haemoglobin A1c", "A1c"),
  syn("lipoprotein_a", "Lipoprotein (a) [Lp(a)]", "Lp(a)", "Lipoprotein (a)"),
  syn("creatinine", "Creatinine"), syn("creatine", "Creatine"),
  syn("urea", "Urea"),
  syn("urea_nitrogen", "BUN", "Blood urea nitrogen", "Urea nitrogen"),
  syn("uric_acid", "Uric acid", "Urate"),
  syn("lactic_acid", "Lactate", "Lactic acid"),
  syn("pyruvic_acid", "Pyruvate"), syn("oxalic_acid", "Oxalate"),
  syn("citric_acid", "Citrate"), syn("glycerol", "Glycerol"),
  syn("homocysteine", "Homocysteine"), syn("ammonia", "Ammonia"),
  syn("bicarbonate", "Bicarbonate", "HCO3"),
  syn("carbon_dioxide", "Carbon dioxide", "CO2"),
  syn("cortisol", "Cortisol"), syn("prednisolone", "Prednisolone"),
  syn("aldosterone", "Aldosterone"), syn("dexamethasone", "Dexamethasone"),
  syn("testosterone", "Testosterone"),
  syn("dhea", "DHEA", "Dehydroepiandrosterone"),
  syn("androstenedione", "Androstenedione"),
  syn("estradiol", "Estradiol", "Oestradiol", "E2"),
  syn("progesterone", "Progesterone"),
  syn("thyroxine", "Thyroxine", "T4", "Free T4", "FT4"),
  syn("triiodothyronine", "Triiodothyronine", "T3", "Free T3", "FT3"),
  syn("folate", "Folate", "Folic acid"),
  syn("cobalamin", "Vitamin B12", "B12", "Cobalamin", "Cyanocobalamin"),
  syn("ascorbic_acid", "Vitamin C", "Ascorbate", "Ascorbic acid"),
  syn("calcifediol", "25-hydroxyvitamin D", "25-OH vitamin D", "Vitamin D"),
  syn("ethanol", "Ethanol", "Alcohol"), syn("methanol", "Methanol"),
  syn("ethylene_glycol", "Ethylene glycol"), syn("acetone", "Acetone"),
  syn("acetaminophen", "Acetaminophen", "Paracetamol"),
  syn("salicylic_acid", "Salicylate", "Salicylic acid"),
  syn("ibuprofen", "Ibuprofen"), syn("caffeine", "Caffeine"),
  syn("theophylline", "Theophylline"), syn("phenytoin", "Phenytoin"),
  syn("phenobarbital", "Phenobarbital", "Phenobarbitone"),
  syn("valproic_acid", "Valproate", "Valproic acid"),
  syn("carbamazepine", "Carbamazepine"), syn("lamotrigine", "Lamotrigine"),
  syn("levetiracetam", "Levetiracetam"), syn("metformin", "Metformin"),
  syn("warfarin", "Warfarin"), syn("digoxin", "Digoxin"),
  syn("methotrexate", "Methotrexate"),
  syn("ciclosporin", "Ciclosporin", "Cyclosporine", "Cyclosporin A"),
  syn("tacrolimus", "Tacrolimus"), syn("vancomycin", "Vancomycin"),
  syn("morphine", "Morphine"), syn("codeine", "Codeine"),
  syn("sodium", "Sodium"), syn("potassium", "Potassium"),
  syn("calcium", "Calcium"), syn("magnesium", "Magnesium"),
  syn("chlorine", "Chloride"), syn("lithium", "Lithium"),
  syn("iron", "Iron"), syn("zinc", "Zinc"), syn("copper", "Copper"),
  syn("wolfram", "Tungsten"),
  syn("dihydrogen", "Hydrogen gas", "H2"),
  syn("dinitrogen", "Nitrogen gas", "N2"),
  syn("dioxygen", "Oxygen gas", "O2"))

synonyms <- rbind(element_synonyms, molecule_synonyms)
synonyms <- synonyms[!duplicated(tolower(synonyms$synonym)), ]

## ------------------------------------------------------------------ write ---
substances <- rbind(element_substances, molecules, proteins)

write_registry <- function(x, file) {
  utils::write.csv(x, file.path(out_dir, file), row.names = FALSE, na = "")
  message("wrote ", file, " (", nrow(x), " rows)")
}
write_registry(sources, "sources.csv")
write_registry(substances, "substances.csv")
write_registry(synonyms, "substance_synonyms.csv")
write_registry(parameters, "substance_parameters.csv")
write_registry(conversions, "substance_conversions.csv")
