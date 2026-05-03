# Stage 17.6 server inventory

## Scope

Stage 17.6 continues the app-server refactor by extracting contrast-related notification wording from `R/app-server.R`.

## Files added

- `R/app-server-contrast-helpers.R`
- `tests/testthat/test-app-server-contrast-helpers.R`

## Server area touched

- Pairwise contrast list management
- Pairwise contrast execution guard
- Average/custom contrast validation messages

## Refactor rule

This stage preserves behaviour by keeping the existing strings unchanged and routing the server through pure helper functions. No contrast calculation logic is moved in this stage.

## Next candidate

A later stage can extract contrast label construction and validation into pure helpers before moving contrast UI/server code into a module.
