# Stage 49.6.5: vague individual-prediction repair

## Purpose

Repair question-objective construction so vague continuous-outcome wording such as `do well`, `perform well`, and `good mark` does not create an unsupported `outcome_threshold` requirement.

Threshold requirements remain the responsibility of the established question route. Explicit pass, fail, success, and numeric-threshold questions therefore keep their existing handling.

## Developer examples

A developer-only suite has been added under:

```text
inst/extdata/examples/developer/question-routing
```

The examples cover vague prediction wording, complete and incomplete profiles, explicit and missing thresholds, expected responses, comparisons, model-level questions, and unsupported causal wording. They are interactive examples, not unit tests.

## Path portability

All new paths are deliberately short enough for Git checkouts on Windows.
