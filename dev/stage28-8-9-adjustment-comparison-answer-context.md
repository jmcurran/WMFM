# Stage 28.8.9 - Adjustment-comparison follow-up answer tightening

## Goal

Diamonds IV uses the follow-up question:

```text
Does adjusting for cut, color, and clarity improve our predictions substantially?
```

The deterministic follow-up classifier correctly recognises this as an
`adjustment_prediction_comparison` request, and the model payload contains the
comparison between the adjusted log-log model and the simpler weight-only
log-log model.

However, manual testing showed that the generated explanation could still
explain only the adjusted model without directly answering the comparison
question.

## Change

This stage tightens the deterministic prompt payload for adjustment-comparison
follow-ups.

The prompt now explicitly tells the language model to:

- directly answer whether the adjustment terms improve prediction;
- use the deterministic direct answer supplied by WMFM;
- include the simpler-model versus adjusted-model comparison;
- describe the result as in-sample fit only;
- avoid claiming out-of-sample predictive improvement.

## Notes

This remains deliberately conservative. The comparison uses in-sample fit
summaries only, such as adjusted R-squared, residual standard error, and AIC.
It does not claim that the adjusted model would necessarily predict better on a
new data set.
