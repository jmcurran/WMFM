# Stage 20.7: adjustment-variable polish and release readiness audit

## Summary

- Audited the full Stage 20 adjustment-variable path across UI wording, reactive state storage, formula/model metadata flow, fitted-equation role summaries, deterministic explanation blocks, prompt construction, interaction guardrails, and offline regression tests.
- Confirmed app-facing wording remains `Adjust for this variable` and internal terminology remains `adjustment variable` throughout operational code paths.
- Kept model fitting and formula construction behavior unchanged while tightening namespace style in Stage 20 prompt helpers.

## Changes made

- Updated prompt helper namespace usage to follow project convention:
  - Added `@importFrom stats formula model.frame` to adjustment prompt-role helper docs and removed direct `stats::` calls in that file.
  - Added `@importFrom stats anova model.frame terms` to term-evidence helper docs and removed direct `stats::` calls where equivalent imports are available.
- Roxygen regeneration is still required in an R-enabled environment to apply `@importFrom` updates into `NAMESPACE`.
- Bumped package version for successful Stage 20.7 build: `0.1.7.584 -> 0.1.8.584`.

## Validation

- Attempted to run `devtools::test()`, but this container does not include an `R` executable, so tests could not be run here.
- No behavioral logic changes were made to model fitting or formula construction in this stage.

## Remaining risks

- No release-blocking Stage 20 inconsistencies found in current offline coverage.
- As in prior stages, interactive end-to-end UX validation still depends on manual Shiny run-through in a local GUI session.

## Readiness

- Stage 20 workstream appears ready to merge back to `master` once broader release checklist items outside Stage 20 scope are complete.
