# WMFM Stage 45 context: observation-level and residual-inspection questions

Use the **r-development-style** and **wmfm-stage-script-generator** skills for this work.

## Starting point

Stage 44 has been completed and merged into `master`.

Stage 44 delivered a deterministic analysis-recipe architecture and a downloadable Quarto analysis showing how WMFM calculations can be reproduced using ordinary R code. The exact educational design of that export has deliberately been paused and recorded for later review.

Stage 45 should not reopen or redesign the Stage 44 export unless a small compatibility change is strictly necessary for Stage 45.

Create a fresh Stage 45 feature branch from the updated `master`. Confirm the branch name, current commit, package version, and clean working tree before changing code.

## Stage 45 objective

Stage 45 introduces a bounded pathway for questions about individual observations relative to an ordinary linear model.

Representative questions include:

- "What is a good deal on a diamond?"
- "Which observations are unusually cheap relative to the fitted model?"
- "Which students performed better than expected?"
- "Which observations have outcomes substantially above or below their fitted values?"

These are not ordinary coefficient-interpretation questions. They require observation-level calculations based on the fitted model.

The first implementation should remain deliberately narrow:

- ordinary linear models only;
- observed values, fitted values, and residual-based ranking;
- deterministic calculations outside the language model;
- cautious language that does not treat a residual as proof of a bargain, anomaly, causal effect, or data error.

Do not introduce quantile regression in the initial Stage 45 implementation.

## First task: architecture and behaviour audit

Before implementation, inspect the current code paths for:

- research-question classification;
- deterministic prediction and contrast handling;
- explanation request construction;
- model and source-data state;
- observation identifiers and retained descriptive columns;
- plot and table rendering;
- analysis-recipe extension points; and
- offline tests for unsupported or model-specific questions.

Identify the smallest clean extension that allows an observation-level question to produce a deterministic result object before any language-model prompt is constructed.

Do not route these questions through ordinary coefficient interpretation and ask the language model to invent a ranking.

## Required deterministic result

Define a stable observation-level result object for supported ordinary linear models.

At minimum, each returned observation should be able to carry:

- a stable row index;
- a useful observation identifier when one is available;
- the observed response;
- the fitted response;
- the raw residual;
- the ranking direction;
- the rank; and
- a percentile or comparable relative-position measure.

Preserve useful descriptive columns where this can be done safely and clearly. Do not automatically expose every source-data column.

The result should also include metadata describing:

- the model family and support status;
- the response variable;
- the ranking metric;
- whether larger positive or larger negative values are being sought;
- any exclusions caused by missing values; and
- any limitations needed by the renderer or explanation layer.

## Residual metric for the initial implementation

Begin with raw residuals for ordinary linear models:

```text
residual = observed response - fitted response
```

For questions seeking observations that are low relative to the model, rank from the most negative residual upward.

For questions seeking observations that are high relative to the model, rank from the most positive residual downward.

For questions asking which observations are generally unusual, the first stage may use absolute raw residuals, but the distinction between directional and non-directional questions must be explicit.

Do not silently substitute standardised, studentised, proportional, or ratio residuals.

The audit should record whether a later stage should add:

- standardised residuals;
- studentised residuals;
- proportional differences;
- log-scale-specific summaries; or
- influence diagnostics.

Those extensions are not required for the first Stage 45 result.

## Question classification

Add deterministic classification for observation-level questions before ordinary model interpretation.

The classifier should distinguish at least:

- unusually low relative to the fitted model;
- unusually high relative to the fitted model;
- unusually far from the fitted model in either direction;
- unsupported observation-level requests; and
- questions requiring more information.

The classification result should retain the evidence or reason code used to select the pathway.

Avoid broad lexical rules that capture ordinary coefficient questions merely because they contain words such as "high", "low", "best", or "expected".

## Observation identifiers

A ranked table is much more useful when observations can be recognised.

Audit the fitted data for candidate identifiers, including:

- explicitly selected ID variables, if WMFM already supports them;
- row names that carry meaningful non-default values;
- obvious unique identifier columns; and
- concise combinations of descriptive columns where no single identifier exists.

Do not claim that an arbitrary row number is a substantive identifier. A row index may be retained as a stable technical reference when nothing better is available.

Any automatic identifier selection must be conservative and tested. It may be preferable for the first implementation to use a row index plus a small set of safe descriptive columns rather than infer semantic identity too aggressively.

## Tables and plots

Provide a deterministic table of the leading observations, with a bounded default number of rows.

The table should make the ranking understandable by showing at least:

- identifier or row index;
- observed value;
- fitted value;
- residual; and
- rank or percentile.

Consider a simple plot that places observed values against fitted values or displays residuals for the ranked observations. Any plot should clarify the calculation rather than imply stronger anomaly detection than the model supports.

Do not add decorative plotting work before the result object and table are stable.

## Explanation behaviour

The language model may explain only the deterministic quantities supplied by WMFM.

Prompts should explicitly prevent claims such as:

- "This diamond is definitely a bargain";
- "This observation is erroneous";
- "The model proves this student overperformed"; or
- "The residual identifies a causal effect."

Preferred wording should remain calibrated, for example:

- the observation is lower than the fitted value by a specified amount;
- it is among the most negative residuals in the fitted data;
- this makes it a candidate for closer inspection under the current model; and
- the conclusion depends on the model specification and available predictors.

The deterministic result should be useful even when no language-model provider is available.

## Scope restrictions

The initial Stage 45 implementation should reject or defer:

- logistic and Poisson models;
- individual causal claims;
- assertions that an observation is erroneous;
- automatic deletion or correction of observations;
- formal outlier testing unless explicitly designed and justified;
- quantile-regression estimates;
- leverage and Cook's-distance workflows; and
- comparisons requiring variables absent from the fitted model or retained data.

Unsupported requests should receive a clear deterministic reason rather than falling through to a generic model explanation.

## Testing requirements

Add offline tests covering at least:

1. ranking the most negative residuals for a supported ordinary linear model;
2. ranking the most positive residuals;
3. ranking by absolute residual for a non-directional unusual-observation question;
4. correct preservation of observed, fitted, and residual values;
5. stable handling of missing observations omitted by the model;
6. fallback to row indices when no useful identifier exists;
7. conservative use of an available identifier or descriptive columns;
8. rejection of unsupported GLM requests;
9. prevention of ordinary coefficient questions being misclassified as observation-level questions;
10. deterministic output without an LLM provider; and
11. prompt content that contains verified values and explicit interpretation guardrails.

Use exact-value tests where the calculations are deterministic. Keep all tests offline.

## Documentation and development record

Create a Stage 45 design or implementation note in `dev/` that records:

- the chosen classification contract;
- the deterministic result-object structure;
- the initial residual metric;
- identifier-selection behaviour;
- supported and unsupported model classes;
- explanation guardrails; and
- deferred extensions.

Maintain `NEWS.md` using version-number headings only.

## Explicitly deferred work

Record, but do not implement unless the audit proves it unavoidable:

- standardised or studentised residual ranking;
- proportional or ratio-based ranking;
- specialised handling of log-response models;
- influence diagnostics;
- quantile regression;
- automated anomaly or data-error claims;
- editable observation identifiers; and
- integration of observation-level results into the paused Stage 44 export design.

## Completion criteria

Stage 45 is complete when:

- observation-level questions are identified before ordinary interpretation;
- supported ordinary linear models produce a deterministic ranked result;
- tables clearly show observed, fitted, and residual quantities;
- identifier handling is conservative and reproducible;
- unsupported models and requests fail safely with informative reasons;
- language-model explanations are constrained to verified deterministic results;
- offline tests cover classification, computation, rendering, and guardrails;
- package validation succeeds; and
- the completed stage is committed and archived using the WMFM stage workflow.

## Suggested opening request for the next chat

```text
I want to begin WMFM Stage 45 using the attached context and the current codebase. Stage 44 has been merged into master. Please use my r-development-style and wmfm-stage-script-generator skills. Start with an architecture and behaviour audit before proposing implementation changes. The initial scope is ordinary linear models and deterministic residual-based observation ranking only.
```
