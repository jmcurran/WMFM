# Stage 47.8: Focused intelligent-question-handling examples

## Objective

Stage 47.8 replaces the mixed Stage 47.7 evaluation suite with twelve examples
that directly exercise intelligent handling of unclear, purpose-oriented,
educational, causal, diagnostic, and model-capability questions.

## Example questions

1. Why are you predicting my exam result?
2. Why are we doing this?
3. I don't know.
4. What is the chance I will pass the course?
5. Can this model answer my question?
6. What should I ask instead?
7. Is this analysis useful?
8. What does this result mean?
9. Does this prove attendance improves marks?
10. I don't understand the question.
11. Tell me whether the model is any good.
12. Should I use a different model?

All examples use the existing Course data and fitted model so that differences
in the responses arise from question handling rather than from changes of data
or model. The evaluation suite label is `intelligent_question_handling`.

The Stage 47.7 examples that mixed prediction, residual, percentile, and
comparable-observation work into this suite are removed.
