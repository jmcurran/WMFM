# Stage 20.5: adjustment-variable regression hardening

## Summary
- Added focused regression tests that exercise the adjustment-variable workflow end to end without changing feature behavior.
- Covered selection/storage sanitization, fitted-formula retention, fitted-equation role display separation, deterministic explanation constraints, and prompt-role separation in one contract-level test path.
- Added explicit no-adjustment regression coverage to confirm Stage 20 behavior remains unchanged when no adjustment variables are selected.
- Kept model fitting, formula construction, fitted-equation behavior, and explanation/prompt generation logic unchanged.

## Version
- DESCRIPTION version bumped: `0.1.7.582 -> 0.1.7.583`.

## Tests
- Added `tests/testthat/test-adjustment-workflow-regression.R` with focused Stage 20.5 regression coverage.
- Ran full `devtools::test()` suite.

## Stage 20.6 follow-up
- Add interaction-term-aware adjustment-role tests once interaction terms are explicitly in scope.
