# Stage 27.6.2 logistic future-outcome framing repair

## Purpose

Stage 27.6.1 overcorrected the logistic prediction-interval repair by restoring the earlier unsupported pathway for binomial GLM prediction-interval wording. That made the older unsupported tests pass, but it contradicted the Stage 27.6 contract: logistic future-observation wording should return deterministic Bernoulli outcome framing.

## Contract after this repair

For binomial-logit GLM follow-ups:

- ordinary fitted-probability requests still return fitted probability and confidence interval only
- explicit prediction-interval or future-outcome wording returns an `ok` prediction payload
- the prediction payload remains a fitted mean-response prediction
- `predictionInterval` contains Bernoulli future-outcome framing
- `predictionIntervalPolicy$supported` is `TRUE`
- `predictionIntervalUnsupportedReason` is `NULL`

## Outcome probability naming

The Bernoulli framing now includes stable numeric aliases:

- `outcomeProbabilities[["1"]]` is the fitted event probability
- `outcomeProbabilities[["0"]]` is one minus the fitted event probability

When the model response has display labels, those labels are also included as additional named entries so UI code can use either stable numeric aliases or student-facing level labels.

## Testing intent

This repair updates legacy expectations that treated all GLM prediction-interval requests as unsupported. Poisson support was added in Stage 27.5 and logistic Bernoulli outcome framing was added in Stage 27.6, so the tests now distinguish unsupported families from supported Poisson and binomial-logit policies.
