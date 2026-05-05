# Stage 17.2 server inventory

## Scope

Stage 17.2 continues the app-server refactor by extracting developer-mode
status and notification text into pure helpers.

## Files changed

- `R/app-server.R` now delegates developer-mode text construction to helpers.
- `R/app-server-developer-mode-helpers.R` contains the extracted pure helper
  functions.
- `tests/testthat/test-app-server-developer-mode-helpers.R` covers the helper
  text and locked/unlocked status behaviour.

## Behaviour preserved

The same status and notification strings are used by the Shiny server. The
change only moves text selection out of the main server function so the server
body becomes slightly more focused and the text can be tested without a Shiny
session.

## Next candidates

- Continue extracting small message builders around provider unlocking.
- Look for pure validation helpers near data-source setup.
- Avoid moving reactive modelling code until several small extraction patterns
  have been proven safe.
