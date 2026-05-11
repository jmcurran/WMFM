# Stage 20.4: Suppress substantive adjustment-variable explanations

## Summary
- Added explanation/prompt helpers that separate primary predictors from adjustment variables using existing `wmfm_adjustment_variables` metadata.
- Added explicit prompt guidance to:
  - interpret primary predictors as substantive findings,
  - mention adjustment variables only as adjusted-for controls,
  - avoid substantive coefficient interpretation for adjustment variables,
  - avoid causal inference from adjustment.
- Updated term-evidence prompt guidance to label adjustment-variable roles and apply adjusted-for wording.
- Kept model fitting and formula construction unchanged.
- Preserved Stage 20.3 and Stage 20.3.1 fitted-equation behavior by leaving equation-display helpers untouched.

## Version
- DESCRIPTION version bumped: `0.1.7.581 -> 0.1.7.582`.

## Stage 20.5 follow-up
- Add optional UI-facing deterministic explanation post-processing that can inject a compact "after adjusting for ..." phrase when needed.
- Expand interaction-specific adjustment phrasing tests for higher-order terms.
