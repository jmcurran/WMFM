# Stage 47.4: Missing information and definitions

## Objective

Stage 47.4 generalises the shared question-routing contract when a meaningful question cannot yet be answered because the user has omitted information that WMFM must not invent.

## Behaviour

Prediction requests that omit fitted-model predictor values now receive a precise deterministic clarification. The response names only the missing predictors, asks for values in `name = value` form, and does not request unrelated columns from the original data set.

Questions about passing, failing, or succeeding that do not define the relevant outcome threshold are routed to `needs_input` with reason `missing_outcome_threshold`. WMFM asks the user to state the rule explicitly before deciding whether the current fitted model can answer the completed question. It does not assume a pass mark or other cutoff.

The same route object is used for research and follow-up questions. Follow-up responses use the route's deterministic clarification before the ordinary prediction-failure fallback, so the visible answer remains specific and provider-independent.

## Compatibility

Existing deterministic prediction, residual, comparable-observation, unit-change, adjustment-comparison, explanation-preference, purpose, and educational pathways are unchanged. Stage 47.4 does not yet decide whether a threshold-defined probability question requires a different response model; that belongs to Stage 47.5 alternative-analysis guidance.

## Runner robustness

The Stage 47.4 runner performs post-commit package building in an isolated temporary source tree created from the committed archive. The package installation therefore cannot rewrite tracked source files in the working repository. The runner verifies the source tree immediately after the commit and again after archive, build, and installation steps.
