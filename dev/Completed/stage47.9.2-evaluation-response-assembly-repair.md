# Stage 47.9.2: evaluation response assembly repair

Stage 47.9.2 repairs the remaining end-to-end response assembly problem for
focused intelligent-question examples.

The specialised deterministic response was already returned by
`lmExplanation()`, but `runModel()` subsequently passed it through the general
anchored factor-comparison safeguard. That safeguard appended the ordinary
Course model summary to every focused response.

This substage:

- skips anchored factor-comparison insertion for `question_route_response`
  payloads;
- retains anchored comparison insertion for ordinary explanations and the
  established prediction and observation pathways;
- adds an end-to-end `runModel()` test using the Course data so the saved
  evaluation response cannot contain the ordinary anchored summary; and
- restores removal of superseded `stage*_completed.zip` archives after the
  current completed archive is created.
