# Stage 47.6: Model-specific capability guidance

## Objective

Stage 47.6 replaces generic question suggestions with a small set of examples
that are grounded in the fitted model and established WMFM answer pathways.

## Behaviour

For unclear questions, explicit requests for guidance, and unsupported
follow-up questions, WMFM now derives up to four answerable examples from the
current model. The examples use the fitted response and predictor names and may
include:

- an association question for a fitted predictor;
- an uncertainty question for that relationship;
- a group-comparison question when a factor predictor is present; and
- a prediction question containing representative values for every fitted
  predictor.

Representative numeric values use the observed median. Representative factor
values use the most frequent observed level. WMFM does not invent variables or
suggest questions that require a pathway outside the fitted model.

## Integration

The fitted model is now supplied when the shared route is attached in the Shiny
fit workflow, `runModel()`, and the linear-model prediction enrichment pathway.
The existing route contract remains unchanged. Model-specific examples are
added through the existing deterministic response and the stable recommended
capability name `model_specific_question_examples`.

## Compatibility

Established prediction, residual, comparable-observation, unit-change,
adjustment-comparison, conditional-quantile, educational, purpose, and
alternative-analysis routes retain their precedence and behaviour. Generic
fallback text remains available when no fitted model is supplied.

## Validation

Offline tests cover model-specific example generation, research-question
capability requests, unclear follow-up payloads, deterministic follow-up
answers, and unsupported fitted-model questions.
