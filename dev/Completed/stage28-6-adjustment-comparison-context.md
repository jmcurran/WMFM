# Stage 28.6 - Log-log adjustment comparison follow-up

## Goal

Add deterministic support for the Diamonds IV follow-up question:

```text
Does adjusting for cut, color, and clarity improve our predictions substantially?
```

## Terminology

Continue the Stage 28 terminology decision:

- use `log-log` internally and in technical developer notes
- use proportional-change language in student-facing explanations
- avoid student-facing `power law` and `elasticity` wording

## Deterministic comparison

For adjusted log-log `lm` models, WMFM now constructs a simpler comparison model
from the fitted model frame:

```r
log(response) ~ log(primaryPredictor)
```

It compares the adjusted fitted model against this simpler model using:

- adjusted R-squared
- residual standard error on the log-response scale
- AIC

This is deliberately described as an in-sample fit comparison. The prompt tells
the language model not to claim out-of-sample predictive improvement or invent
cross-validation results.

## Scope

This stage is conservative. It supports adjusted `lm` log-log models and leaves
other model classes for later deterministic pathways.
