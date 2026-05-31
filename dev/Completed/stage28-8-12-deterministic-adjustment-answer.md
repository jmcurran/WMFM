# Stage 28.8.12 - Deterministic adjustment-comparison answer

## Context

The Diamonds IV follow-up asks whether adjusting for cut, color, and clarity improves predictions substantially. Earlier Stage 28 work successfully classified this as an adjustment-prediction comparison and built a richer deterministic payload using nested-model fit evidence.

However, the LLM could still ignore the follow-up and only describe the adjusted model's carat effect. This showed that prompt instructions alone were not a reliable control mechanism for this follow-up type.

## Design decision

For adjustment-comparison follow-ups, WMFM should make the model-comparison judgement deterministically and append a student-facing answer when the LLM omits or under-emphasises it.

The deterministic payload may contain log-likelihood, likelihood-ratio, deviance, AIC, and residual standard error diagnostics. These diagnostics support the judgement but should not be named in the student-facing explanation unless explicitly requested.

## User-facing rule

The student-facing appended answer should say whether the adjusted model improves in-sample predictions compared with the simpler weight-only model, and should include the caution that this is not evidence from a separate test set or cross-validation.

It should not mention log-likelihood, likelihood-ratio tests, p-values, deviance, AIC, adjusted R-squared, or residual standard error in the ordinary explanation.
