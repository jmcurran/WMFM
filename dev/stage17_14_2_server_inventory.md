# Stage 17.14.2 server inventory

This repair keeps the Stage 17.14 structural extraction intact and narrows the startup-loading UI source-boundary test.

The Stage 17.14 extraction moved `applyLoadedExampleToInputs()` into `R/app-server-state-helpers.R`. That helper file also contains reset logic that legitimately uses `updateRadioButtons()` for other UI controls. The test now checks specifically that the loaded-example/model-type path uses `updateSelectInput(session, "model_type", ...)` and does not use `updateRadioButtons(session, "model_type", ...)`.
