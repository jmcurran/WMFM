# Stage 49.8: natural routing and stronger evaluation

## Purpose

Complete the remaining question-routing work exposed by the Stage 49.7 evaluation suite.

## Changes

- Recognise ordinary numeric profile wording such as "got 17 in the test".
- Resolve attendance wording including "students who attend", "attending", and "non-attending".
- Extract explicit outcome thresholds such as "over 50" without confusing them with predictor values.
- Construct two complete profiles for natural binary-factor comparisons that share a numeric predictor value.
- Recognise adjusted-effect wording such as "matter after allowing for".
- Return specialised deterministic answers without appending a redundant generic model explanation.
- Extend question-routing evaluation metadata and summaries to check supplied profiles, missing fields, outcome thresholds, and deterministic prediction status.

## Evaluation

```r
runWMFMEvaluationSuite(
    suite = "question_routing",
    outputDir = "evaluation/stage49.8-question-routing",
    overwrite = TRUE,
    resume = FALSE
)
```
