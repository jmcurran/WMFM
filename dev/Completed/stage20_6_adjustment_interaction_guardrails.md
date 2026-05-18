# Stage 20.6: adjustment-variable interaction guardrails

## Summary
- Added explicit interaction guardrail labeling in the interaction selector UI so any selected interaction involving an adjustment variable is clearly marked as such.
- Added a focused helper that detects whether any model term (including interactions) involves an adjustment variable.
- Updated deterministic term-evidence prompt guidance to label interactions involving adjustment variables explicitly, rather than implicitly treating them as ordinary primary-predictor interactions.
- Preserved model fitting behavior and existing non-interaction Stage 20.2 to 20.5 behavior.

## Chosen guardrail
- Implemented conservative **labeling guardrails**:
  - UI interaction choices now append `[includes adjustment variable]` when applicable.
  - Prompt-side term evidence now labels relevant terms as `interaction involving adjustment variable`.
- This is the smallest safe behavior aligned with current app design because it does not block valid model formulas or alter fitting, but prevents accidental ordinary-substantive interpretation.

## Version
- DESCRIPTION version bumped: `0.1.7.583 -> 0.1.7.584`.

## Tests
- Added `tests/testthat/test-adjustment-interaction-guardrails.R` for:
  - adjustment-interaction detection helper behavior,
  - explicit interaction-role labeling when an adjustment variable is in an interaction,
  - regression that ordinary interactions remain labeled as primary when no adjustment variable is involved.
- Ran full `devtools::test()` suite.

## Stage 20.7 follow-up
- Consider adding a compact fitted-equation role-summary line that lists interaction terms involving adjustment variables for additional UI visibility after fitting.
