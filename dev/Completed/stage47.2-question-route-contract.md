# Stage 47.2: Executable characterisation and route contract

## Status

Stage 47.2 implements the first package-code stage of intelligent question handling. It does not change visible application responses.

Version:

```text
1.1.3.002
```

## Changes

Stage 47.2 adds a shared internal `wmfmQuestionRoute` contract for research and follow-up questions. The contract records the question source, high-level route, answerability status, stable reason code, missing information, deterministic-computation requirement, and the existing mature payload that produced the route.

The new `routeModelQuestion()` wrapper delegates established follow-up requests to `classifyModelFollowupQuestion()` rather than replacing that classifier. Existing prediction, residual, comparable-observation, unit-change, adjustment-comparison, conditional-quantile, and explanation-preference categories are therefore preserved.

Prediction-shaped research questions now expose the same route contract while retaining their established payload fields and deterministic prediction result.

An offline table-driven unusual-question corpus records current classification alongside the proposed Stage 47 route, reason code, and response owner. It is intentionally a characterisation corpus: most proposed unusual-question routes are implemented in later substages.

## Compatibility

There is no intended user-visible behaviour change. Existing payload fields remain available. The attached `questionRoute` field is additive for prediction-shaped research-question payloads.

## Next stage

Stage 47.3 can use the stable contract and corpus to implement unclear, purpose, and educational routes without altering mature deterministic statistical pathways.
