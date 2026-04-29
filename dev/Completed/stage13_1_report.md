# WMFM Stage 13.1 example-suite sweep report

## Scope

Stage 13.1 returned to the original close-out sweep plan: run the existing example suite, inspect developer-feedback JSON files, and classify issues before making further changes. The body.df Poisson two-factor example idea was discarded because the data are more naturally a two-way table of counts formed from body image by ethnicity than a count-response Poisson regression problem.

The sweep reviewed examples covering:

- intercept-only linear model
- numeric-predictor linear model
- one-factor linear model
- additive two-factor linear model
- two-factor interaction linear model
- additive numeric-plus-factor linear model
- numeric-by-factor interaction linear model
- intercept-only binomial GLM

## Main findings

### 1. Final-answer tagging is the dominant recurring issue

Several examples produced statistically acceptable or near-acceptable explanations, but the sentence-level roles did not identify the true final answer consistently.

Observed patterns:

- Intercept-only models: the estimate-plus-CI sentence is also the final answer.
- Main-effect models: intermediate effect statements were sometimes tagged as answer-like even when the final summary sentence was the real answer.
- Interaction models: subgroup-specific effects were sometimes treated as answers, while the integrated interaction conclusion was not.
- The code currently uses the legacy `answer` role rather than a separate `finalAnswer` role, so the immediate implementation target should be correct and consistent `answer` assignment rather than introducing a new ontology term.

Recommended follow-up:

- Add or strengthen regression coverage for structural answer selection.
- Ensure intermediate subgroup or component-effect statements do not retain the `answer` tag when a later integrated conclusion answers the research question.
- Keep the current role ontology unless a later stage explicitly redesigns it.

### 2. Prompt guidance needs targeted tightening, not redesign

The sweep did not show a need for broad prompt rewriting. The issues are mostly stable, small wording and statistical-communication constraints.

Recurring prompt patterns:

- Avoid qualified model labels such as "fitted model", "linear model", "logistic model", or "Poisson model" in student-facing explanations. Plain "the model" is acceptable.
- Preserve the terminology used in the research question and data documentation, especially "mark" rather than drifting to "score".
- Avoid model-process or scaffolding language unless it genuinely helps interpret the model structure. A useful context sentence such as "In this model, the effect was estimated separately for the two groups" can be retained.
- Do not use overlap or non-overlap of separate confidence intervals as the justification for group differences.
- Prefer direct weak-evidence wording, such as "there is no clear evidence of a difference based on these data".
- When group fitted values are reported from additive models that also include numeric predictors, explicitly state the numeric reference value in simple language.
- For numeric predictors, prefer concrete wording like "for each one-mark increase" over abstract phrasing such as "per unit increase".
- For interactions, include numeric estimates for within-group effects or differences between effects where available; do not rely only on qualitative words like "steeper", "flatter", or "stronger".

### 3. Statistical structure is mostly sound

The Stage 12 prompt and payload work appears to have substantially improved the statistical structure of the explanations.

Working patterns observed:

- Intercept-only GLM probability output was concise and on the response scale.
- Factor-by-factor interaction output correctly compared within-group effects and used an interaction contrast with uncertainty.
- Numeric-by-factor interaction output had a good conceptual structure and a useful model-context sentence.
- Additive models generally reported the relevant effects, but need clearer conditioning and comparison phrasing.

## Classification by issue type

| Issue pattern | Classification | Priority | Stage 13.2 action |
| --- | --- | --- | --- |
| Missing or misplaced final answer tag | Sentence tagging / structural answer selection | Very high | Add focused deterministic tests and verify existing structural selection behaviour |
| Intermediate subgroup effects tagged as answers | Sentence tagging | Very high | Add interaction-specific regression test |
| Qualified model wording such as "fitted model" | Prompt guidance | Medium-high | Add explicit wording control |
| Terminology drift from mark to score | Prompt guidance | Medium-high | Add terminology-preservation instruction |
| CI overlap used as evidence | Prompt guidance / statistical reasoning | High | Add explicit prohibition and replacement guidance |
| Vague weak-effect wording | Prompt guidance | Medium | Prefer direct "no clear evidence" wording |
| Missing numeric conditioning for group fitted values | Prompt guidance | High | Require simple reference-value wording |
| Numeric slope phrased as "per unit increase" | Prompt guidance | Medium | Prefer explicit one-unit / one-mark change wording |
| Interaction explained only qualitatively | Prompt guidance | Medium-high | Ask for numeric support where available |

## Recommended conclusion

Stage 13.1 can be closed as a successful diagnostic sweep. The immediate Stage 13.2 work should be deliberately small: add prompt guidance and deterministic regression coverage for the recurring issues above. It should not add new example datasets or redesign the explanation system.
