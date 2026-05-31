# Stage 28.8.7 - Log-log plot prediction fix

## Issue

Direct log-log formulas such as:

```r
log(price) ~ log(carat)
```

fit successfully, but the fitted-model plot failed when it tried to call
`predict()` on a plotting grid containing `log(carat)` rather than the original
`carat` variable required by the formula evaluation environment.

## Fix

The plot grid now detects a simple natural-log numeric predictor. For plotting,
it keeps the transformed column used by ggplot, but for prediction it also
supplies the original predictor variable that `predict.lm()` needs.

## Regression coverage

Added deterministic tests showing that `drawModelPlot()` works for direct
log-log formulas with and without confidence bands.
