# Stage 27.4 GLM prediction interval design note

## Purpose

Stage 27.4 records the intended teaching and implementation direction for true GLM prediction intervals. Earlier GLM follow-up work computes fitted mean-response predictions with confidence intervals. That is statistically valid, but it is not the same as uncertainty for a future individual outcome.

This stage does not change student-visible prediction behaviour. It adds deterministic metadata so later stages can implement prediction intervals without confusing them with confidence intervals.

## Distinction to preserve

- A confidence interval describes uncertainty in the fitted mean response at supplied predictor values.
- A prediction interval describes uncertainty for a future observation at supplied predictor values.

For GLMs these are different objects and should not be presented as interchangeable.

## Logistic / Bernoulli models

For a binomial logistic model with a Bernoulli response, the future observation is binary. A conventional continuous 95% prediction interval is not appropriate in the same way it is for a linear model.

Teaching direction:

- Continue to report the fitted probability and confidence interval for the fitted probability.
- If the student asks for future-outcome uncertainty, explain that a future response is one of the outcome levels.
- Consider reporting the predicted probability of each outcome and a plain-language statement about the possible future outcome.
- Do not present a numeric lower-to-upper prediction interval unless a deliberate discrete-outcome representation is designed and tested.

Implementation direction:

- Keep logistic prediction-interval support disabled for now.
- Record metadata explaining why it is disabled.
- In a later stage, add a deterministic Bernoulli outcome framing rather than a conventional interval.

## Poisson count models

For a Poisson model, the fitted mean is the expected count. A future count is discrete and non-negative, so prediction intervals can be constructed from the predictive count distribution.

Teaching direction:

- Report the fitted expected count and confidence interval for the expected count.
- When prediction intervals are implemented, label them as intervals for a future count, not for the fitted expected count.
- The interval should be integer-valued and non-negative.

Implementation direction:

- Start with a conditional Poisson predictive interval using the fitted mean as the Poisson rate.
- Store the method name explicitly, for example `conditional_poisson_quantile`.
- Consider whether to include fitted-mean uncertainty later; if added, mark it as a different method.
- Add diagnostics fields for requested level, lower count, upper count, method, and whether parameter uncertainty is included.

## Diagnostics contract

Every GLM follow-up payload should be able to carry a `predictionIntervalPolicy` list with stable fields:

- `supported`: logical
- `glmFamily`: character
- `glmLink`: character
- `futureObservationType`: character
- `recommendedNextStage`: character
- `method`: character or `NULL`
- `parameterUncertaintyIncluded`: logical
- `studentExplanation`: character
- `developerExplanation`: character

This lets the developer diagnostics explain the current behaviour even before prediction intervals are implemented.

## Stage 27.4 scope

Stage 27.4 adds this metadata scaffold only. The actual Poisson future-count interval should be implemented in a later stage after the contract is in place.
