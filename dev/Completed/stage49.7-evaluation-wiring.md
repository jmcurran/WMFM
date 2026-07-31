# Stage 49.7: evaluation-path question-aware wiring

## Purpose

Bring the command-line and developer evaluation paths onto the same research-question objective machinery used by the app.

## Changes

- `runModel()` now constructs and attaches the full research-question objective as well as the shared route.
- The objective and route are returned in `wmfmModel$meta` for diagnostics and evaluation.
- Missing-input and capability routes replace generic model prose with a deterministic clarification or capability response.
- `runWMFMEvaluationSuite()` records the research-question objective, observed archetype, route, and follow-up requirement.
- Developer example metadata now states the expected archetype, route, and follow-up requirement so evaluation summaries can compare intended and observed behaviour.

## Evaluation

Rerun the developer suite with:

```r
runWMFMEvaluationSuite(
    suite = "question_routing",
    outputDir = "evaluation/stage49.7-question-routing",
    overwrite = TRUE,
    resume = FALSE
)
```

The examples remain developer-only examples under `inst/extdata/examples`; they are not unit tests.
