# Stage 17.9 server inventory

## Scope

Stage 17.9 continues the app-server refactor by extracting data-context modal and notification wording from `R/app-server.R` into pure helper functions.

## Extracted helper area

- `buildLoadDataFirstMessage()`
- `buildDataDescriptionTitle()`
- `buildProvideDataContextTitle()`
- `buildProvideDataContextHelpText()`
- `buildProvideDataContextPlaceholder()`
- `buildDataContextSavedMessage()`
- `buildDataContextClearedMessage()`

## Behaviour

The Shiny control flow is unchanged. This stage only moves fixed strings and simple title construction into `R/app-server-data-load-helpers.R`.

## Test coverage

`tests/testthat/test-app-server-data-load-helpers.R` now checks the extracted data-context helper wording.
