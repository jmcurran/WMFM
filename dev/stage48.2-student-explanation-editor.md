# Stage 48.2: Student explanation editor prototype

Stage 48.2 adds the first student-authored explanation workspace.

## User interface

A new **Write an explanation** tab appears before the optional model explanation.
It contains:

- a substantial text area for the student's explanation;
- a model-aware coefficient selector;
- a model-aware confidence-interval selector; and
- insertion buttons that place sentence fragments at the current cursor position.

The inserted text supplies exact statistical results but does not write a complete
interpretation. Students remain responsible for explaining direction, context,
uncertainty, and relevance to the research question.

## Behaviour

The toolbar is unavailable until a model is fitted. Its choices are rebuilt from
the current fitted model, and fitting a new model clears the previous student text
so that an explanation cannot silently carry over to a different analysis.

## Scope

This is deliberately an editor prototype. Grading, revision feedback, fitted means,
contrasts, predictions, and insertion metadata are deferred to later substages.
