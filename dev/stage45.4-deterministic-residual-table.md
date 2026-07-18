# Stage 45.4: deterministic residual result table

## Purpose

Stage 45.2 established the deterministic existing-observation residual result and Stage 45.3 added a provider-independent prose answer. The result was still not presented as a dedicated structured object in the application.

Stage 45.4 adds a compact table to the Model Explanation tab whenever the fitted model carries an existing-observation residual request.

## Displayed information

The table shows only the bounded observations already retained in the deterministic result:

- rank;
- observation label;
- source row;
- observed response;
- fitted response;
- raw residual; and
- ranking percentile.

No values are recalculated in the UI layer. Display formatting is applied to the verified deterministic values.

## Interpretation boundary

The accompanying note states that residuals are observed minus fitted on the response scale and that the ranking does not by itself identify bargains, outliers, data errors, or causal effects.

Unsupported model types receive a deterministic scope message rather than an empty or invented table.

## Scope

Stage 45.4 does not add:

- a residual plot;
- alternative residual definitions;
- influence diagnostics;
- GLM residual inspection;
- automatic identifier inference beyond the existing result; or
- conditional-quantile estimation.
