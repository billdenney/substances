# substances

Substance-aware unit conversions.

The [units](https://github.com/r-quantities/units) package converts between
commensurable units. It cannot convert mg/dL to mmol/L, because that needs the
analyte's molar mass, and a unit string has nowhere to put one.

```r
library(substances)

x <- substance(c(100, 140, 5.5), "mg/dL", c("glucose", "sodium", "glucose"))
x
#> <substance<mg/dL>[3]>
#> [1] 100 [mg/dL] glucose 140 [mg/dL] sodium  5.5 [mg/dL] glucose

set_units(x, "mmol/L")
#> <substance<mmol/L>[3]>
#> [1]  5.550745 [mmol/L] glucose 60.896653 [mmol/L] sodium
#> [3]  0.305291 [mmol/L] glucose
```

Each element converts by its own substance, in one call. Quantities of different
substances do not mix:

```r
substance(1, "mmol/L", "glucose") + substance(1, "mmol/L", "sodium")
#> Error: cannot use `+` on different substances: glucose and sodium
```

## Why the substance cannot live in the unit

The obvious approach is to define per-substance units in udunits. It does not
work, in either direction:

```r
units::install_unit("mol_glucose", "0.0055507 mol")
units::install_unit("mol_sodium",  "0.0434978 mol")
units::set_units(1, "mol_glucose") + units::set_units(1, "mol_sodium")
#> 8.836453 [mol_glucose]
```

Defining them relative to `mol` makes every substance mutually convertible.
Defining them as base units instead keeps them apart, but then `mol` to `g`
conversion within a substance is impossible, and adding the definition that
would allow it recreates the leak. **Isolation and conversion are mutually
exclusive inside udunits**, so the substance has to be a property of the vector,
which is what this package does.

## Conversions are derived, not tabulated

A substance carries parameters that are themselves `units` quantities, and a
conversion is the product of small integer powers of them that makes the two
units commensurable. One molar mass therefore serves every unit pair it can
bridge:

```r
set_units(substance(1, "g", "glucose"),     "mol")     #> 0.005550745 [mol]
set_units(substance(1, "ug/L", "glucose"),  "nmol/L")  #> 5.550745 [nmol/L]
set_units(substance(1, "mg/dL", "glucose"), "mmol/L")  #> 0.05550745 [mmol/L]
```

Parameters compose. Sodium mg/dL to mEq/L needs molar mass *and* valence:

```r
set_units(substance(1, "mg/dL", "sodium"), "meq/L")    #> 0.4349761 [meq/L]
```

Volume is bridged differently depending on the state, because the physics is
different. A solid or liquid carries a density, linking mass and volume:

```r
set_units(substance(19.3, "g", "gold"), "cm^3")     #> 1.000934 [cm^3] gold
set_units(substance(1, "mol", "gold"), "cm^3")      #> 10.21505 [cm^3] gold
```

A gas carries a molar volume instead, since Avogadro's law makes volume
proportional to amount rather than mass — so every gas lands on the same figure:

```r
set_units(substance(1, "mol", "helium"), "L")       #> 22.42354 [L] helium
set_units(substance(1, "mol", "neon"), "L")         #> 22.42409 [L] neon
set_units(substance(1, "mol", "dihydrogen"), "L")   #> 22.42990 [L] dihydrogen
```

Note `dihydrogen`, not `hydrogen`. A tabulated element density describes the
standard state, which for hydrogen is H₂, so pairing it with the *atomic* weight
would give the volume per mole of atoms — half a molar volume, and a quantity
nobody wants, since monatomic hydrogen is not something you can have. The
diatomic elements are registered as the molecules they are, and the atomic
entries say why they carry no volume bridge.

And the parameter is not always a molar mass. Insulin mU/L to pmol/L is fixed by
the WHO activity standard, not by insulin's mass:

```r
set_units(substance(1, "mIU/L", "insulin"), "pmol/L")  #> 6 [pmol/L]
```

Some conversions are not multiplicative at all. HbA1c is affine, and is stored
in the direction NGSP publishes it, with the inverse derived rather than
transcribed:

```r
set_units(substance(c(6.5, 7), "%", "hba1c"), "mmol/mol")
#> [1] 47.52951 [mmol/mol] hba1c  52.99519 [mmol/mol] hba1c
```

Note that `%` and `mmol/mol` are both dimensionless, so a units-first
implementation would convert these by a factor of 10. Explicit conversions are
consulted before the dimensional path for exactly this reason.

## Arithmetic keeps the substance

Concentration times volume is an amount, still of the same substance, and
dividing two quantities of the same substance cancels it:

```r
conc <- substance(2, "mmol/L", "glucose")
conc * units::set_units(3, "L")
#> <substance<mmol>[1]>
#> [1] 6 [mmol] glucose

substance(6, "mmol/L", "glucose") / substance(2, "mmol/L", "glucose")
#> 3 [1]
```

Mixing quantities of a bare unit with a substance beyond `*` and `/` is refused,
as is adding across substances.

This interoperation requires **R >= 4.3.0**, for `chooseOpsMethod()`. Below that,
R will not choose between `substance` and `units` operator methods and the
expression fails with "Incompatible methods" before any method here runs.

## Every value has a citation

Reference data ships as CSV with a `source_id` on each value, and a `status`
column so a contested conversion is refused rather than guessed:

```r
substance_info("LDL Cholesterol")
#> <substance cholesterol>  Cholesterol
#>   formula: C27H46O
#>   parameters:
#>     molar_mass  386.664  g/mol  [ok] Computed from the molecular formula and CIAAW 2021 atomic weights

set_units(substance(50, "mg/dL", "Lp(a)"), "nmol/L")
#> Error: cannot convert mg/dL to nmol/L for: lipoprotein_a
```

Lp(a) has no molar mass in the registry because apo(a) isoform size varies
between individuals, so there is no valid fixed mass-to-molar factor. Recording
that as `disputed`, with a note, is different from leaving the row out — which
would read as "not looked up yet".

Substance identity is the point, and it is not always obvious. Blood urea
nitrogen is reported as the mass of *nitrogen*, so treating "BUN" as another
name for urea would apply urea's molar mass and be wrong by a factor of 2.14.
They are registered as different substances:

```r
set_units(substance(1, "mg/dL", "BUN"),  "mmol/L")   #> 0.3569644 [mmol/L] urea_nitrogen
set_units(substance(1, "mg/dL", "urea"), "mmol/L")   #> 0.1665113 [mmol/L] urea
```

Molar masses of ordinary molecules are computed from their formula rather than
transcribed, so a reviewer can re-derive every one of them:

```r
molar_mass_from_formula(c("C6H12O6", "C27H46O"))
#> Units: [g/mol]
#> [1] 180.156 386.664
```

## Bringing your own substances

The shipped registry covers common chemistry and a working clinical set. It is
not meant to be exhaustive, and it is not the only registry you can have — a
package or project supplies its own from a data frame, with no identity table
and no ceremony:

```r
substance_system("my_project", inherit = "substances", parameters = data.frame(
  substance_id = c("widgetol", "widgetol"),
  parameter    = c("molar_mass", "density"),
  value        = c(100, 1.2),
  unit         = c("g/mol", "g/mL"),
  source_id    = "internal-spec"))

set_units(substance(1, "mg/dL", "widgetol", system = "my_project"), "mmol/L")
#> [1] 0.1 [mmol/L] widgetol
set_units(substance(120, "g", "widgetol", system = "my_project"), "mL")
#> [1] 100 [mL] widgetol
```

Identity rows are derived from the `substance_id`s the parameters mention, so
registering a substance is one row. Because the substance is R data rather than
global udunits state, registries coexist in one session and vectors from
different systems refuse to combine, so one project's definitions cannot leak
into another's.

## Scope

In scope: conversions that need a property of the substance, and the registry
that records them, with citations.

Out of scope, deliberately:

- **Converting between substances.** H₂ to H, CO₂ to C, reactants to products —
  offering any of these implies offering all of them, which is a different
  package. Quantities of different substances simply do not combine.
- **Normalising unit strings** (`ng/ml` versus `ng/mL`, `IU/L` versus `U/L`).
  That is per-source data cleaning and belongs in the consuming package.
- **Conversions `units` already performs** — `mg/dL` to `g/L` is dimensional,
  and so is enzyme `U/L` to `ukat/L` once `U` is defined as `umol/min`. Both are
  delegated untouched.

## Status

Prototype, implementing the design proposed on issues
[#1](https://github.com/r-quantities/substances/issues/1),
[#5](https://github.com/r-quantities/substances/issues/5) and
[#2](https://github.com/r-quantities/substances/issues/2). **The design is not
yet agreed upstream and the API should be expected to change.** Nothing has been
released, so `NEWS.md` will stay empty until there is a first release to change
from; `vignette("substances")` is the tour of what the package does.
