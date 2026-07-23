# Stage 47.4.3 follow-up prediction input repair

Stage 47.4 requires prediction questions to provide every fitted-model predictor explicitly. The earlier repair correctly implemented strict validation for research-question predictions, but the ordinary follow-up enrichment pathway still called `computeModelQuestionPrediction()` with its historical default, which permits deterministic completion of omitted predictors.

Stage 47.4.3 passes `allowMissingPredictorCompletion = FALSE` from `enrichFollowupPayloadWithLmPrediction()`. Consequently, a question supplying `attend = 80` for a model containing `attend` and `test` now returns `needs_input` and asks only for `test`; it no longer substitutes the sample mean and produces a prediction.

Fully specified prediction questions remain answerable. Existing prediction calculations, interval selection, extrapolation checks, and research-question routing are otherwise unchanged.
