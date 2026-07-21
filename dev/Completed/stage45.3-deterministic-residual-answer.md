# Stage 45.3: deterministic residual follow-up answers

## Purpose

Stage 45.2 created a deterministic residual-ranking result and supplied it to the language-model prompt. The general deterministic follow-up answer layer did not yet recognise this result category. Consequently, the ranked values could still be absent from the final student-facing explanation if a provider omitted them, and the result had no reusable deterministic prose representation.

Stage 45.3 adds a provider-independent answer for supported and unsupported existing-observation residual requests.

## Behaviour

For a supported ordinary linear model, WMFM now appends a bounded residual-ranking answer that states:

- how many fitted observations were ranked;
- whether the ranking concerns negative, positive, or absolute raw residuals;
- each retained observation label and source row;
- its observed value, fitted value, and raw residual; and
- the calibrated limitation that the comparison applies only under the current fitted model.

The answer uses only values already present in `observationResidualResult`. It performs no new statistical calculation and does not ask the language model to reproduce the ranking.

For an unsupported request, WMFM reports the deterministic scope reason and explicitly states that it has not invented ranked observations.

## Interpretation boundary

The deterministic answer describes an observation as above, below, or equal to its fitted value. It does not label observations as bargains, anomalies, outliers, data errors, overperformers, underperformers, or causal effects.

## Scope

Stage 45.3 does not add:

- a dedicated Shiny table;
- a residual plot;
- alternative residual metrics;
- influence diagnostics;
- GLM residual inspection; or
- conditional-quantile estimation.

Those remain separate later decisions after the deterministic result and answer contracts are stable.
