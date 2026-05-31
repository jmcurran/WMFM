# Stage 25.3.3 GLM follow-up repair

This repair keeps the Stage 25.3 GLM follow-up changes in place and fixes the remaining two-predictor earthquake failure.

## Problem

The two-predictor earthquake follow-up can parse `Magnitude = 3` and `Locn = WA`, but the shared deterministic prediction new-data builder only accepted numeric and factor predictors. In the installed earthquake data path, `Locn` may appear as a character predictor in the model frame, causing the deterministic GLM path to return `needs_input` before resolved values and intervals are assembled.

## Change

- Treat character predictors as categorical predictors when building deterministic prediction `newdata`.
- Prefer fitted model `xlevels` when available, falling back to observed non-missing character values.
- Preserve existing factor handling unchanged.
- Preserve numeric handling unchanged.
- Keep the explicit unsupported reason for future GLM prediction intervals.

## Testing intent

This is intended to clear the remaining Stage 25.3 earthquake test without weakening its expectations for `status = "ok"`, resolved `Magnitude`, resolved `Locn`, response-scale confidence intervals, and explicit unsupported prediction-interval reasoning.
