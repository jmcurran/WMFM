# Stage 4.6.6 data contract note

## Purpose

This note records the current data contract for the teaching-summary feature as implemented in the attached `stage4.6.4_completed.zip` snapshot.

The goal is to identify the minimum inputs, outputs, and dependency boundaries that must be preserved if the feature is repaired, migrated, or polished in the current branch.

## Summary finding

In the inspected snapshot, the teaching summary is already implemented as a deterministic layer that sits between:

1. the fitted model and its deterministic explanation audit
2. the student-facing UI
3. the optional tutor-style AI explanation

This is a good architecture.

The teaching summary is not built from the explanation text itself.
Instead, it is built from the deterministic explanation audit plus model metadata, then rendered in the app.
The optional AI tutor explanation is downstream of that deterministic summary and is constrained by it.

## Core builder

Primary builder:

- `buildExplanationTeachingSummary(audit, model, researchQuestion = NULL)`

Declared input requirements:

- `audit` must not be `NULL`
- `model` must inherit from `lm` or `glm`
- `researchQuestion` may be `NULL` or a single character string

If `researchQuestion` is not supplied, the builder falls back to:

- `attr(model, "wmfm_research_question", exact = TRUE)`

The builder also reads:

- the model frame via `stats::model.frame(model)`
- the response variable name from the model formula
- predictor names and their numeric/factor type split
- `attr(model, "wmfm_response_noun_phrase", exact = TRUE)` when available

## Minimum effective input contract

The teaching summary currently depends on four categories of information.

### 1. Deterministic explanation audit

The audit is the main source of explanation-construction information.
From the inspected code, the teaching summary directly depends at least on:

- `audit$confidenceIntervals$level`
- baseline/effect interpretation information consumed by helper builders such as:
  - `buildExplanationTeachingInterpretationScale()`
  - `buildExplanationTeachingBaselineChoice()`
  - `buildExplanationTeachingMainEffectDescription()`
  - `buildExplanationTeachingUncertaintySummary()`

This means the audit remains the primary contract surface.
The teaching summary should continue to consume current audit fields rather than re-derive explanation logic independently.

### 2. Fitted model metadata

The builder uses the fitted model for:

- response name
- predictor names
- numeric versus factor predictor classification
- stored research question attribute
- stored response noun phrase attribute

This means a valid fitted model object is part of the contract, not just the audit.

### 3. Optional research question

The teaching summary can receive the research question explicitly or recover it from the model attribute.
That research question is used both in:

- the evidence table
- the final research-question linkage paragraph

This makes the research question an optional but first-class input.

### 4. Plain-language response wording

If present, `wmfm_response_noun_phrase` is used to improve student-facing wording.
If absent, the summary falls back to the response variable name.

This is not structurally required, but it materially improves the student experience.

## Output contract

The builder returns a classed list with class:

- `wmfmExplanationTeachingSummary`

Top-level fields currently produced:

- `dataDescription`
- `interpretationScale`
- `baselineChoice`
- `xChangeDescription`
- `mainEffectDescription`
- `uncertaintySummary`
- `evidenceTable`
- `researchQuestionLink`

### Field roles

- `dataDescription`: plain-language orientation to response and predictors
- `interpretationScale`: how the explanation scale is framed for students
- `baselineChoice`: how starting values and comparison groups were chosen
- `xChangeDescription`: what kind of change the explanation discusses
- `mainEffectDescription`: how the main model result is phrased
- `uncertaintySummary`: how confidence interval uncertainty is described
- `evidenceTable`: a compact student-facing list of main ingredients used
- `researchQuestionLink`: how the explanation connects back to the research question

## UI contract

Rendering is handled separately from summary construction.

UI helper functions:

- `renderTeachingSummaryText(text)`
- `renderExplanationTeachingSummaryUi(summary)`

Important characteristics:

- rendering accepts the already-built summary object
- rendering does not recompute interpretation logic
- backticked names are displayed as code-style chips
- the student-facing panel is shown under the heading:
  - `How this explanation was constructed`
- the summary is shown as an accordion with separate sections for scale, baseline choice, change, result wording, uncertainty, and research-question connection

This is a clean separation between model logic and UI logic and should be preserved.

## App integration contract

In `app-server.R`, the teaching summary is rebuilt on demand from:

- `rv$modelExplanationAudit`
- `modelFit()`
- `rv$researchQuestion` or the model attribute fallback

The explanation tab can still show the teaching summary even if no LLM explanation text was generated.
This is good behavior and should be preserved.

Current fallback behavior:

- if both explanation text and audit are absent, the app shows a help message
- if explanation text is absent but the audit is present, the app still shows the teaching summary
- if teaching-summary construction fails, the app suppresses that panel gracefully

## Relationship to claim evidence mapping

The current flow is:

1. build deterministic audit
2. optionally build teaching summary
3. if explanation text exists, build claim-evidence map using:
   - explanation text
   - audit
   - teaching summary
   - model

This suggests that the teaching summary is also part of the downstream explanation-support contract.
It is not only a UI feature.

Therefore, if the teaching-summary object changes shape, the claim-evidence map builder may also need review.

## Relationship to the optional tutor-style AI layer

The tutor-style AI helper does not inspect raw audit internals directly.
Instead it consumes:

- the deterministic teaching summary
- the existing model explanation text
- the research question

That means the safe architectural rule is:

- deterministic teaching summary first
- optional AI elaboration second

This is preferable to sending raw audit structures to the AI layer.

## Recommended contract to preserve going forward

The minimum stable contract for future work should be:

### Inputs

- a valid `wmfmExplanationAudit` object
- a fitted `lm` or `glm` model
- optional research question text
- optional response noun phrase attribute on the model

### Output object

A `wmfmExplanationTeachingSummary` object containing at least:

- `dataDescription`
- `interpretationScale`
- `baselineChoice`
- `xChangeDescription`
- `mainEffectDescription`
- `uncertaintySummary`
- `evidenceTable`
- `researchQuestionLink`

### Rules

- deterministic only
- no direct dependence on LLM explanation text for the core summary
- rendering kept separate from summary construction
- optional AI tutor mode must consume the teaching summary rather than replace it

## Main drift risks to check in the current branch

If this feature is missing or broken in the current branch, likely break points are:

1. audit contract drift
   - renamed or reshaped audit fields used by the teaching-summary helpers

2. model attribute drift
   - loss of `wmfm_research_question`
   - loss of `wmfm_response_noun_phrase`

3. app-state drift
   - renamed reactive values such as `rv$modelExplanationAudit`
   - moved explanation-tab rendering path

4. downstream dependency drift
   - claim-evidence mapping may expect the teaching-summary object to exist or have specific fields

## Recommendation for Stage 4.6.7

Stage 4.6.7 should not start by rewriting the builder.
It should first compare the current branch against this contract and classify the situation as one of:

- intact and only needs polish
- present but drifted and needs repair
- partially missing and needs targeted reconstruction
- absent and needs full rebuild from this contract

That comparison should be done file-by-file for:

- `R/model-explanationTeachingSummary.R`
- `R/app-explanationAudit-uiHelpers.R`
- `R/app-explanationTutor.R`
- `R/app-server.R`
- any teaching-summary tests
