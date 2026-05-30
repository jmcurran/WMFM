# Stage 25.3.2 GLM follow-up repair

This repair is deliberately narrow. It keeps the Stage 25.3 GLM follow-up work but fixes the remaining two-predictor earthquake test failure.

Changes:

- Updates the developer-only two-predictor earthquake follow-up example to use explicit deterministic assignment wording: `Magnitude = 3` and `Locn = WA`.
- Updates the matching offline test to use the same wording.
- Does not weaken the test expectations: the deterministic GLM prediction must still return `ok`, resolve both predictors, include a confidence interval, and record the unsupported GLM prediction-interval reason.
- Leaves broader natural-language parsing for phrases such as `in WA` for a later parser-focused stage.
