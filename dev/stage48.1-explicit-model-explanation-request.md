# Stage 48.1: Explicit model-explanation requests

Stage 48 begins the move towards student-authored explanations.

## Design decision

Fitting a model and asking an LLM to explain it are now separate actions.
Clicking **Fit model** fits the model, creates deterministic equations and the
explanation audit, and returns to the Fitted Model tab. It does not connect to
an LLM or generate explanation prose.

The Model Explanation tab remains visible but is initially unpopulated. After a
model has been fitted it offers an **Explain this model** button. Only that
explicit action connects to the configured provider and generates the model
explanation.

Refitting or resetting clears any explanation and its provenance so prose from
an earlier model cannot remain visible.

## Scope

This substage establishes the interaction needed for the later student-answer
workspace. It does not yet add the student editor, insertion toolbar, grading
workflow, or revision history.

## Validation expectations

Focused tests confirm that:

- the fit-model observer contains no provider acquisition or explanation call;
- deterministic explanation-audit state is still created at fit time;
- the empty explanation state offers the explicit generation button; and
- provider acquisition and explanation generation occur in the button observer.
