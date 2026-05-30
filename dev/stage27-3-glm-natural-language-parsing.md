# Stage 27.3 GLM natural-language parsing expansion

## Goal

Stage 27.3 extends deterministic GLM follow-up parsing for realistic student questions that do not always use explicit `Predictor = value` syntax.

## Scope

This stage keeps the fitted-prediction and extrapolation-policy machinery from Stages 27.1 and 27.2 unchanged. The intended change is narrower:

- recognise more prediction-style question wording;
- infer a numeric value for a single numeric predictor when the question gives one unambiguous number without naming the predictor;
- handle common attendance wording such as "attendance is yes";
- map common location aliases such as "Washington" to the fitted factor level `WA` when the model uses the `Locn` predictor;
- keep ambiguous cases on the existing clarification pathway.

## Non-goals

This stage does not add GLM prediction intervals, change confidence-interval calculations, or loosen extrapolation blocking. It also does not introduce language-model parsing. The parser remains deterministic and testable offline.

## Examples covered

- "What would you predict for a student who scored 15?"
- "What happens if attendance is yes?"
- "How many earthquakes would you expect in Washington at magnitude 5.6?"
- "What are the odds of passing for someone with a test mark of 10?"

## Design notes

The parser should prefer explicit syntax when it is present. Natural-language fallback is only used when the model structure makes the request unambiguous, such as a single remaining numeric predictor and exactly one numeric value in the question.
