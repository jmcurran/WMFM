# Stage 17.8 server inventory

## Scope

Stage 17.8 continues the low-risk data-loading helper extraction started in Stage 17.7.

## Change

- Moved the loaded-example status string out of `R/app-server.R`.
- Added `buildLoadedExampleStatus()` to `R/app-server-data-load-helpers.R`.
- Added focused tests for the extracted loaded-example status wording.

## Behaviour

The app-server flow is unchanged. Loading an example still updates the example status text and switches to the Model tab.

## Refactor direction

This keeps reducing inline user-facing wording in the monolithic server while preserving the same Shiny reactivity and data-loading side effects.
