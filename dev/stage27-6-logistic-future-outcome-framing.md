# Stage 27.6 logistic future-outcome framing

## Goal

Stage 27.6 implements the logistic/binomial part of the Stage 27.4 prediction-interval design.

The important teaching distinction is that an individual future logistic outcome is binary. It is not a continuous response with a lower and upper prediction interval in the ordinary linear-model sense.

## Behaviour

For binomial-logit GLM follow-up questions that explicitly ask for a prediction interval, WMFM now returns deterministic Bernoulli future-outcome framing.

The payload keeps the fitted probability and confidence interval for the fitted probability. When a future-observation interval is requested, it also reports:

- `predictionIntervalPolicy$supported = TRUE`
- `predictionIntervalPolicy$futureObservationType = "future_binary_outcome"`
- `predictionIntervalPolicy$method = "bernoulli_outcome_framing"`
- `predictionInterval$distribution = "bernoulli"`
- `predictionInterval$outcomeProbabilities`
- `predictionInterval$parameterUncertaintyIncluded = FALSE`

The returned object deliberately uses `scale = "binary_outcome"` and `intervalScale = "binary_outcome"` to avoid implying that this is a conventional continuous prediction interval.

## Interpretation

If the fitted probability is `p`, the future binary outcome has probability `p` for the modelled event and `1 - p` for the other outcome.

For a numeric 0/1 logistic response, WMFM labels these outcomes as `0` and `1`.

For a two-level factor response, WMFM labels the first factor level as the other level and the second factor level as the modelled event level, matching R's binomial GLM convention.

## Limitation

Stage 27.6 treats the fitted probability as fixed. It does not integrate uncertainty in the fitted probability into the future-outcome probabilities.

A later stage could investigate whether to combine fitted-probability uncertainty and Bernoulli outcome uncertainty in a teaching-safe way.

## Stage 27.6.1 repair note

Stage 27.6.1 keeps conventional logistic `prediction interval` wording on the
legacy unsupported path. Logistic models do not have a continuous future-outcome
prediction interval analogous to an `lm` prediction interval.

The Bernoulli framing is now reserved for explicit future-outcome wording such
as outcome probabilities or possible outcomes. This preserves compatibility with
older prediction-interval tests while still allowing deterministic explanation of
the two possible binary outcomes when that is what the student asks for.
