# Stage 47.4.2 predictor-omission routing repair

Stage 47.4.2 repairs the distinction between values supplied by the user and
values resolved internally by the deterministic prediction machinery.

The prediction payload's `completedPredictorValues` field contains all resolved
predictor values, including values that the user supplied. It therefore cannot
be used by itself as evidence that WMFM filled in a missing value. The shared
route now computes omitted predictors as the fitted model's required predictors
minus the names in `suppliedPredictorValues`.

Consequently:

- a prediction with every required predictor supplied remains answerable;
- a prediction for which deterministic computation filled one or more omitted
  predictors is routed to `needs_input`;
- the clarification names only the omitted predictors; and
- research-question predictions that already disable automatic completion keep
  their established answerable or needs-input behaviour.

The unusual-question corpus is also updated so the pass/chance-to-pass cases
are assessed against the implemented `missing_outcome_threshold` route rather
than their historical classifier categories.
