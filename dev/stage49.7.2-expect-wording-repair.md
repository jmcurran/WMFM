# Stage 49.7.2: prediction wording repair

Stage 49.7.2 repairs the remaining Stage 49.7 test failure.

The research-question prediction detector recognised `expected response` and
`expected value`, but not ordinary personal wording such as `What exam mark
should I expect ...?`. Consequently, `runModel()` classified a complete
individual prediction request as `explain_fitted_model` and produced no
prediction payload.

This repair adds `expect`, `expects`, and `expected` to the established
prediction-verb detector. The existing association-only exclusion remains in
place, so ordinary questions about relationships or effects continue to use
the model-explanation route.
