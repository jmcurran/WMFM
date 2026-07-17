# Stage 44.7.3: compact ordered-factor preparation

## Purpose

Stage 44.7.3 narrows the generated data-preparation section for selected
ordered factors. It replaces repeated variable-specific conditionals with a
single explanatory paragraph, a character vector of affected variable names,
and one conversion loop.

## Behaviour

WMFM records which variables in the Factors bucket were ordered before model
fitting. The standalone analysis then:

- names those affected selected variables in a short explanation;
- explains why polynomial contrasts would complicate interpretation;
- creates one `orderedFactors` character vector; and
- converts each affected variable with `factor(as.character(...))` in one loop.

Ordered factors that are not selected for the model are not changed. Ordinary
selected factors continue to be treated as nominal factors without producing
the ordered-factor explanation.

## Validation

Focused offline tests cover recipe metadata, compact rendering for one and
several ordered factors, omission of repeated `is.ordered()` conditionals, and
absence of the explanation for ordinary factors.
