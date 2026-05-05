# Stage 17.15 server inventory

Stage 17.15 continues the structural app-server refactor.

## Extracted section

- Moved the contrast tab UI, contrast-specific dynamic UI, contrast list management, custom-weight status, and contrast computation observers into `R/app-server-contrasts.R`.
- Added `registerContrastObservers()` as the orchestration boundary called from `appServer()`.

## Result

- `R/app-server.R` now delegates contrast behaviour to an explicit registration helper.
- This removes a large cohesive section from the top-level server while preserving the existing reactive wiring and user-facing behaviour.

## Next candidates

- Extract model-plot outputs into a plotting server helper.
- Extract formula building and validation observers into a model-formula helper.
- Extract data-loading observers into a data-source helper.
