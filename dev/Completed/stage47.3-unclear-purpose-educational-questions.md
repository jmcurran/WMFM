# Stage 47.3: Unclear, purpose, and educational questions

## Objective

Stage 47.3 implements the first user-visible routes built on the shared Stage 47 question contract. It handles a deliberately small group of questions that should not receive an ordinary fitted-model interpretation:

- empty-content or non-question responses such as `I don't know`;
- vague requests such as `Tell me something interesting`;
- questions about why a prediction or analysis is being performed; and
- basic definitions of linear, logistic, and Poisson regression.

## Behaviour

The new routes provide bounded deterministic responses. They do not send the unusual question to the language model and they do not allow a conventional model interpretation to masquerade as the answer.

Follow-up questions retain the existing payload shape through `classifyAndRouteModelFollowupQuestion()`. Mature statistical categories continue to use the established classifier and deterministic enrichment pathways. Newly recognised Stage 47.3 questions use the additive `question_route_response` compatibility category and carry their `wmfmQuestionRoute` object.

Research questions are routed when the model is fitted. When a research question is classified as unclear, purpose-oriented, or educational, `lmExplanation()` returns the deterministic route response without calling the configured chat provider.

## Compatibility

Prediction, residual, comparable-observation, unit-change, proportional-change, adjustment-comparison, and explanation-preference pathways are unchanged. Stage 47.3 does not yet implement missing-threshold guidance, causal boundaries, model-diagnostic guidance, or alternative-model recommendations; those remain later Stage 47 work.

## Validation

Offline tests cover route classification, compatibility payloads, student-visible deterministic follow-up responses, suppression of conventional LLM explanations for routed research questions, and preservation of existing prediction classification.
