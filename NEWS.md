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

## Systems

* `substance_system()` creates isolated registries that coexist in one session,
  optionally inheriting from another with local entries taking precedence.
  Because the substance lives in R data rather than the udunits database, this
  needs no global state, and a `substance` vector records its system so vectors
  from different systems cannot be combined.
* `U`, `IU` and `eq` are installed into udunits at load, and removed at unload.
  `U` is `umol/min`; `IU` is a separate base dimension, since the WHO
  biological-standard unit is not the enzyme unit.

## Reference data (#2)

* Five CSVs in `inst/extdata`, with a `source_id` on every value and a `status`
  column so a disputed conversion can be recorded and refused rather than
  guessed (Lp(a) mass/molar is the case in point).
* Molar masses of ordinary molecules are **computed** from their formula and the
  atomic weights by `molar_mass_from_formula()`, not transcribed, so a reviewer
  can re-derive every one. A test recomputes them all.
* `chemical_elements.csv` is migrated into the parameter table. The twenty
  elements clinical data actually uses carry CIAAW 2021 values; the rest keep
  their inherited values, flagged in `note` as predating the 2009 IUPAC revision.

## Known limitations

* `x * units::set_units(3, "L")` does not work and cannot be made to: R refuses
  to dispatch a binary operator when both operands carry methods from different
  classes, so this fails before any method of ours runs. Use
  `substance_scale()`. A test pins the behaviour so a change in R or `vctrs`
  would surface.
* Cross-substance (stoichiometric) conversion is designed but not implemented,
  as agreed on #1.
* Ordering and comparison fall back to the `vctrs` record defaults, which
  compare value then substance. Whether comparing across substances should be
  an error is an open question.
