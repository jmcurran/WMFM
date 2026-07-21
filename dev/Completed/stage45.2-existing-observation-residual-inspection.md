# Stage 45.2: existing-observation residual inspection

## Purpose

Stage 45.2 adds deterministic inspection of observations already used to fit an ordinary linear model. It follows the Stage 45.1 boundary: specified-case value thresholds such as a "good deal" remain conditional-quantile questions and are not answered by residual ranking.

## Supported questions

The bounded classifier recognises questions that explicitly ask WMFM to identify, list, show, rank, or find existing observations relative to their fitted values. It distinguishes:

- the most negative raw residuals;
- the most positive raw residuals; and
- the largest absolute raw residuals.

The question must contain observation-selection language, an observation type, model-relative language, and a direction. Broad uses of words such as high, low, cheap, good, or expected are not sufficient on their own.

## Deterministic calculation

For ordinary `lm` objects, WMFM calculates:

```text
raw residual = observed response - fitted response
```

The result records the model-frame row mapping and, when meaningful row names are available, uses them as observation labels. Otherwise it reports source row numbers. Ties are resolved stably by source row.

The deterministic payload includes rank, observation label, source row, observed response, fitted response, residual, and ranking percentile. The percentile describes position in the selected ranking metric; it is not a conditional response percentile.

## Interpretation boundary

WMFM describes observations only as lower than fitted, higher than fitted, or furthest from fitted under the current model. The prompt prohibits unsupported labels such as bargain, anomaly, outlier, data error, overperformer, underperformer, or causal effect.

Residual rankings concern observations already used to fit the model. They do not answer what value would be cheap, good, high, or low for a newly specified case.

## Scope

Stage 45.2 supports ordinary linear models only. It does not add:

- GLM residual inspection;
- studentised residuals, leverage, or Cook's distance;
- formal outlier or influence diagnostics;
- quantile regression;
- a new plot or dedicated UI table; or
- Stage 44 analysis-recipe integration.
