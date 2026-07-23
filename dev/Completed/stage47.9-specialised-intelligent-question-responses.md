# Stage 47.9: specialised intelligent-question responses

Stage 47.9 uses the focused Stage 47.8 evaluation results to extend intelligent question handling.

The stage:

- recognises the six focused questions that previously fell through to the ordinary model explanation;
- treats questions about unspecified capability, usefulness, unclear wording, unspecified results, and alternative models as requests for targeted guidance;
- recognises proof-and-improvement wording as a causal claim that the fitted observational model cannot establish;
- returns the deterministic specialised response directly for non-model-answer follow-up routes, instead of generating and then appending it to a full coefficient-by-coefficient explanation; and
- leaves established model-answer and explanation-preference follow-ups on their existing pathways.

The durable evaluation suite remains `intelligent_question_handling`.
