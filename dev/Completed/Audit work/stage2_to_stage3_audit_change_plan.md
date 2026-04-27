# Audit contract implementation plan for WMFM

## Goal
Translate the frozen explanation-audit contract into targeted code and test work, without redesigning unrelated explanation systems.

## Recommendation
Treat the current code as the baseline implementation and do a tighten-and-protect pass rather than a structural rebuild.

## Priority order
1. Expand regression tests to cover the contract
2. Confirm app/runtime paths do not create conflicting audit definitions
3. Decide whether to improve `print.wmfmExplanationAudit()` now or leave it minimal
4. Only then make any small structural repairs that the tests reveal

## Proposed concrete tasks

### Task 1. Add contract-shape tests
Update `tests/testthat/test-buildModelExplanationAudit.R` so it asserts:
- all required top-level audit fields exist
- required subfields exist within `overview`, `promptInputs`, `interpretationScale`, `numericAnchor`, `confidenceIntervals`, and `rawPromptIngredients`
- empty sections still have the correct type

Expected scope:
- test-only

### Task 2. Add numeric-anchor rule tests
Add tests that explicitly cover:
- numeric predictor with 0 inside range -> anchor is 0
- numeric predictor with 0 outside range -> anchor is sample mean
- no numeric predictors -> empty numeric-anchor table

Expected scope:
- mostly test-only unless helper edge cases are exposed

### Task 3. Add factor-reference tests
Add tests that explicitly cover:
- factor predictor appears in `referenceLevels`
- reported `referenceLevel` matches the first factor level in the model frame
- no factor predictors -> empty data frame

Expected scope:
- test-only

### Task 4. Add model-family interpretation tests
Extend audit tests so they explicitly check:
- linear model interpretation-scale fields
- logistic model interpretation-scale fields
- Poisson log-link interpretation-scale fields

Expected scope:
- test-only

### Task 5. Add CI/evidence shape tests
Add tests that explicitly check:
- `confidenceIntervals` exists even when CI-derived tables are sparse
- `baselineEvidence` and `effectEvidence` have supported columns when non-empty
- `displayedScales` reflect the relevant family-specific display behaviour

Expected scope:
- test-only unless CI helpers expose shape drift

### Task 6. Add storage/runtime tests
Tighten `tests/testthat/test-explanation-audit-runtime.R` so it checks:
- `runModel()` stores the audit
- app/runtime access works without chat access
- runtime helpers do not replace the stored contract with a divergent structure

Expected scope:
- test-only unless duplicate runtime construction paths need cleanup

### Task 7. Decide on print-method scope
Review `R/methods-print-wmfmExplanationAudit.R`.

Decision options:
- keep current minimal print method and only test that it works
- modestly enrich it with overview and CI summaries for better developer inspection

Recommendation:
- do a modest enrichment only if it stays deterministic and compact
- otherwise leave it alone for now and protect it with a smoke test

Expected scope:
- optional code + tests

## Files most likely to change

### Likely test files
- `tests/testthat/test-buildModelExplanationAudit.R`
- `tests/testthat/test-explanation-audit-runtime.R`

### Possibly touched R files
- `R/methods-print-wmfmExplanationAudit.R`
- `R/app-equation-runtime.R`
- `R/model-explanationAudit.R`

## Non-goals for this pass
Do not use this pass to:
- redesign the audit object shape unnecessarily
- fold teaching-summary fields into the core audit object unless already justified
- rebuild the student-facing UI
- merge old branch architecture wholesale

## Suggested completion criterion
This pass is complete when:
- the contract document is accepted
- contract-level tests pass offline
- `runModel()` storage is protected by regression tests
- the audit object remains deterministic and app-accessible
- any remaining open items are about future teaching/UI layers, not the audit foundation itself
