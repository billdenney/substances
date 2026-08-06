# substances 0.0.0.9000

First prototype, implementing the design proposed on issues #1, #5 and #2. Not
yet agreed upstream — the API should be expected to change.

## Object model (#5)

* `substance()` creates a vector with a **different substance per element**,
  built on a `vctrs` record so the substance survives `[`, `c()`, `rev()`,
  `rep()`, `unique()` and data-frame row subsetting. The structure originally
  sketched in #5 — a `units` object with a `substance` attribute — loses the
  attribute on the first subset, silently, including when subclassed.
* The class deliberately does **not** inherit from `units`, so any operation we
  have not implemented fails loudly instead of returning a bare `units` vector
  with the substance quietly dropped.
* `mixed_substances()` handles the long-format case where the unit varies per
  element too, and converts to a homogeneous `substance`.

## Conversions (#1)

* `set_units()` converts substance-aware, per element, in three stages: an
  explicit conversion registered for that substance and unit pair, then an
  ordinary dimensional conversion delegated to `units`, then a bridge composed
  from the substance's parameters.
* Bridge parameters (`molar_mass`, `density`, `valence`, `activity`) are
  themselves `units` quantities, so one molar mass serves every unit pair it can
  bridge. Two parameters compose: sodium `mg/dL` to `mEq/L` uses molar mass and
  valence together.
* Explicit conversions are tried **before** the dimensional path, because some
  are between dimensionally-equivalent units — HbA1c `%` and `mmol/mol` are both
  dimensionless, and a units-first implementation converts them by a factor of
  10 instead of by the NGSP master equation.
* Ambiguity is an error: if two combinations of parameters both apply and
  disagree, `bridge_factor()` refuses rather than picking one.
* Arithmetic requires matching substances; division by the same substance
  cancels it, leaving a `units` quantity.

## Systems, and bringing your own substances

* `substance_system()` is the extension point, and takes a **data frame of
  parameters alone** — the identity table is optional and is derived from
  whatever the other tables declare, so registering a substance is one row
  rather than a schema. Supply `substances` explicitly to record names,
  formulae and identifiers, and the referential check then applies. Synonyms
  and conversions extend the same way.
* Registries are isolated and coexist in one session, optionally inheriting from
  another with local entries taking precedence.
  Because the substance lives in R data rather than the udunits database, this
  needs no global state, and a `substance` vector records its system so vectors
  from different systems cannot be combined.
* `U`, `IU` and `eq` are installed into udunits at load, and removed at unload.
  `U` is `umol/min`; `IU` is a separate base dimension, since the WHO
  biological-standard unit is not the enzyme unit.

## Documentation

* `vignette("substances")` walks through the problem, the udunits demonstration,
  per-element substances, the bridge parameters including density and molar
  volume, affine conversions, substance identity, citations and systems.

## Reference data (#2)

* Five CSVs in `inst/extdata`, with a `source_id` on every value and a `status`
  column so a disputed conversion can be recorded and refused rather than
  guessed. Lp(a) mass/molar is one case; iron's valence is another, withheld
  because iron circulates as both Fe2+ and Fe3+ while its other parameters stay
  usable.
* Molar masses of ordinary molecules are **computed** from their formula and the
  atomic weights by `molar_mass_from_formula()`, not transcribed, so a reviewer
  can re-derive every one. A test recomputes them all, and a second checks a
  sample against PubChem, including formulae with iodine, cobalt, chlorine,
  fluorine and sulfur.
* 61 substances now carry a formula-derived molar mass, covering common
  chemistry panels, lipids, hormones, vitamins, therapeutic drug monitoring and
  toxicology. 15 published conventional-to-SI factors are pinned as a regression
  corpus.
* **Volume bridges for the elements**, from the PubChem periodic table, with the
  standard state and reference conditions in the note. Which parameter applies
  depends on the state:
  * a **solid or liquid** carries `density`, which links mass and volume and,
    with the atomic weight, gives the molar volume — gold 10.2 cm3/mol, sodium
    23.7, mercury 14.8;
  * a **gas** carries `molar_volume` instead, because Avogadro's law makes
    amount, not mass, the thing volume is proportional to. Helium, neon,
    dihydrogen and dinitrogen all land within 0.2% of 22.4 L/mol, as they must.
* The **diatomic elements are registered as the molecules they are**
  (`dihydrogen`, `dioxygen`, `dichlorine`, …), and the atomic entries carry no
  volume bridge at all. A tabulated element density describes the standard
  state, which for hydrogen is H2, so pairing it with an atomic weight silently
  answers a different question: volume per mole of *atoms*, 11.2 L for hydrogen,
  half a molar volume and a quantity nobody wants, since monatomic hydrogen is
  not something you can have. The withheld entries say so in their note.
* Elements with no practical elemental form — technetium, promethium, the
  transuranics — carry no volume bridge, rather than a tabulated number nobody
  can use.
* `chemical_elements.csv` is migrated into the parameter table. The 21 elements
  clinical data actually uses carry CIAAW 2021 values; the rest keep their
  inherited values, flagged in `note` as predating the 2009 IUPAC revision.
* Blood urea nitrogen is registered as a **separate substance** from urea, not a
  synonym. BUN is reported as the mass of nitrogen, so using urea's molar mass
  would be wrong by a factor of 2.14 — a substance-identity error of exactly the
  kind this package exists to catch.
* `substance_info()` now prints each parameter's note, which is where reference
  conditions and caveats live.

## Interoperating with plain `units` quantities

`x * units::set_units(3, "L")` works, and so does the reverse order and
division both ways, with the substance carried through. This needs
`chooseOpsMethod()`, hence **R >= 4.3.0**: without it R refuses to dispatch a
binary operator when both operands carry methods from different classes, and
the expression fails with "Incompatible methods" before any method of ours
runs. Notably the only S3 arrangement that works *without* `chooseOpsMethod()`
is inheriting from `units`, which returns a plain `units` object with the
substance silently dropped — so the tie-break is what lets the class stay
composed rather than inherited.

We claim the tie only when the other operand is a `units` quantity; any other
`Ops` conflict keeps R's default behaviour rather than being captured by us.

The reverse direction needs a `vec_arith.units` to route from, which this
package registers. `vctrs` ships such methods for the base classes but not for
`units`, and it belongs here rather than upstream: hosting it in `units` would
oblige that package to take a `vctrs` dependency for this one method and
nothing else. Its default defers to `vctrs`' own error, so `units` gains no
behaviour it did not already have, and a test pins that ordinary
units-to-units arithmetic is unaffected.

Only `*` and `/` are defined against a bare `units` quantity. Addition and
comparison error, because a quantity with no substance should not silently
combine with one that has a substance; whether comparison against a
substance-free threshold should be allowed is an open question.

## Deliberately out of scope

* **Converting between substances.** H2 to H, CO2 to C, reactants to products:
  offering any of these implies offering all of them, which is a different
  package. Quantities of different substances do not combine, and that is the
  end of it. A project that needs a particular relationship registers the
  substance it actually measures — `urea_nitrogen` alongside `urea` is the
  worked example.
* **Unit string normalisation**, which is per-source data cleaning.
* **Conversions `units` already performs**, which are delegated untouched.
* Ordering and comparison fall back to the `vctrs` record defaults, which
  compare value then substance. Whether comparing across substances should be
  an error is an open question.
