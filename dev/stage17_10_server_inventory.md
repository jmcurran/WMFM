# Stage 17.10 server inventory

## Focus

Stage 17.10 continues the app-server refactor by extracting variable-management modal wording from `R/app-server.R` into pure helpers.

## Extracted responsibilities

- Numeric-as-factor confirmation modal title, body text, and button labels.
- Add-derived-variable modal title, help text, placeholder, and confirmation label.
- Reuse of the existing `buildLoadDataFirstMessage()` helper for derived-variable guards.

## Behaviour

No Shiny control flow was changed. The server still owns modal display, input handling, bucket state, and derived-variable creation. This stage only centralises user-facing wording for the next module boundary.

## Test coverage

Focused helper tests were added in `tests/testthat/test-app-server-variable-helpers.R`.
