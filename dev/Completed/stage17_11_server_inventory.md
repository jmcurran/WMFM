# Stage 17.11 server inventory

## Scope

Stage 17.11 continues the app-server refactor by extracting model-fitting notification and validation wording from `R/app-server.R` into pure helper functions.

## Files changed

- `R/app-server.R`
- `R/app-server-fit-model-helpers.R`
- `tests/testthat/test-app-server-fit-model-helpers.R`
- `dev/stage17_11_server_inventory.md`

## Notes

- The Shiny control flow remains in `appServer()`.
- Extracted helpers cover chat provider fallback messages, predictor-count validation, binary-factor recoding, logistic response validation, Poisson response warnings, and unknown model-type errors.
- This keeps Stage 17 focused on reducing inline server text before moving larger reactive/model-fitting responsibilities into modules.
