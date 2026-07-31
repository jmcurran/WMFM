# Stage 49.8.10: objective route precedence repair

Stage 49.8.10 repairs answer selection when the preliminary research-question
route requests missing input but the completed question objective contains a
successful specialised answer, such as a natural two-profile comparison.

`lmExplanation()` now treats the validated research-question objective as the
authoritative route for answer selection. A preliminary `needs_input` route no
longer short-circuits a completed `model_answer` objective before the
specialised deterministic answer can be rendered.

The existing behaviour-named cached-answer test covers this regression.
