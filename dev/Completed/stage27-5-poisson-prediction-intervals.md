# Stage 27.5 Poisson future-count prediction intervals

## Goal

Stage 27.5 implements the first supported GLM future-observation prediction interval pathway for deterministic follow-up answers.

The supported case is:

- fitted model class: `glm`
- family: Poisson
- response interpretation: future count
- interval type: conditional future-count prediction interval

## Student-facing distinction

WMFM already reports confidence intervals for the fitted expected count. Those intervals describe uncertainty in the fitted mean response.

A prediction interval for a future count answers a different question: what range of count values would be plausible for one new observation with the requested predictor values?

For Stage 27.5, the future-count interval is only attached when the user explicitly asks for a prediction interval.

## Statistical rule

Let `lambdaHat` be the fitted response-scale expected count from the Poisson GLM at the requested predictor values.

Stage 27.5 returns the discrete interval:

- lower: `qpois(0.025, lambda = lambdaHat)`
- upper: `qpois(0.975, lambda = lambdaHat)`

The fitted expected count is treated as fixed.

## Metadata contract

The prediction interval payload records:

- `distribution = "poisson"`
- `method = "conditional_poisson_quantile"`
- `intervalScale = "count"`
- `parameterUncertaintyIncluded = FALSE`

This makes the limitation explicit. The interval includes conditional count variation but does not include parameter uncertainty in the fitted expected count.

## Deliberate limits

Stage 27.5 does not add a binomial/logistic prediction interval. Logistic future outcomes are binary, so they need a separate student-facing framing rather than a conventional continuous interval.

Stage 27.5 does not add simulation or bootstrap intervals. Those may be useful later, but this stage keeps the first implementation deterministic, fast, and offline-safe.
