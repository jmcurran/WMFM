# Stage 25.3.6 GLM follow-up repair

This repair addresses the remaining Stage 25.3 test failures after the Stage 25.3.5 bundle.

Changes:

- Allow deterministic GLM confidence-interval requests to return fitted mean-response predictions with response-scale confidence intervals.
- Keep explicit GLM prediction-interval requests unsupported for now, with a clear unsupported reason.
- Add `intervalScale = "response"` to GLM confidence-interval payloads while preserving the existing `scale = "response"` field.
- Fix natural numeric predictor extraction so phrases such as `scored 15 on Assign` resolve to `15`, not the final digit `5`.

Prediction intervals for future GLM observations remain deferred to a later stage.
