# Stage 27.1 GLM extrapolation policy

## Purpose

Stage 27.1 adds a deterministic extrapolation policy for GLM follow-up predictions.
The goal is to keep student-facing follow-up answers useful while preventing the app
from silently presenting predictions that are far outside the data used to fit the
model.

## Scope

This stage applies to deterministic GLM follow-up predictions for binomial-logit and
Poisson-log models. It focuses on numeric predictors supplied in the follow-up
question. Factor predictors continue to use the existing level-validation pathway.

## Policy

For each numeric predictor value supplied by the student, WMFM records the observed
range from the fitted model frame and classifies the requested value as follows.

- `in_range`: the requested value is inside the observed range.
- `extrapolation_warning`: the requested value is outside the observed range, but no
  farther than 10% of the observed range width beyond the nearest boundary.
- `extrapolation_blocked`: the requested value is farther than 10% of the observed
  range width beyond the nearest boundary.

When all supplied numeric values are `in_range`, the prediction proceeds normally.
When at least one supplied numeric value receives `extrapolation_warning` and none are
blocked, the prediction proceeds and the payload includes a warning plus diagnostics.
When any supplied numeric value receives `extrapolation_blocked`, WMFM suppresses the
prediction and returns an `extrapolation_blocked` status.

## Diagnostics contract

The prediction payload should include an `extrapolationPolicy` object containing:

- `status`: the overall classification.
- `thresholdFraction`: the fraction of observed range width used for warning versus
  blocking.
- `numericPredictors`: one entry per supplied numeric predictor with the observed
  minimum, observed maximum, requested value, nearest boundary, distance outside the
  range, tolerance, and classification.
- `message`: plain-language explanatory text suitable for diagnostics and prompt
  failure pathways.

## Teaching rationale

The policy deliberately distinguishes slight extrapolation from extreme extrapolation.
Small boundary slips can still be pedagogically useful if the app makes the limitation
clear. Extreme requests should be blocked because the fitted model has little direct
data support there and a deterministic answer could be misleading for students.

## Future work

Later Stage 27 work can expose the same policy in UI-specific language, extend the
policy to ordinary linear-model follow-ups, and make the threshold configurable if
manual testing suggests that 10% is too strict or too permissive for teaching examples.
