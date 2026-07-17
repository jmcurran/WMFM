# Stage 44 analysis export: design pause and future review

## Purpose of this note

Stage 44 introduced a downloadable Quarto analysis intended to connect the work performed in WMFM with the ordinary R code a student could use independently.

Development reached a useful and functional stopping point, but the final design is not regarded as settled. This note records the current understanding, the remaining ambiguity, and the principles that should guide any later return to the work.

## Core purpose of the exported document

The exported Quarto document is not primarily a statistical report and is not intended to reproduce the language-model interpretation shown in WMFM.

Its central purpose is:

> To demonstrate how the statistical analyses performed by WMFM can be reproduced using standard R code.

The document should therefore reinforce the connection between the app and a conventional R workflow. A student should be able to use it to understand how to:

- load and prepare the data;
- fit the selected model;
- inspect standard model output;
- calculate fitted values or fitted means;
- calculate requested predictions or contrasts;
- obtain appropriate uncertainty intervals; and
- reproduce relevant plots.

The exported analysis should not encourage students to make `ellmer` or another language-model interface part of their normal statistical workflow.

## Current boundary between WMFM and the export

WMFM has two related but distinct roles:

1. it performs deterministic statistical calculations; and
2. it may ask a language model to explain verified results.

Only the first role belongs in the reproducible code document.

Language-model prose should not be copied into the Quarto analysis merely because it is available in a payload. The export should instead contain visible, ordinary R code that reproduces the underlying calculation.

This distinction applies even where the app already has carefully constructed text describing fitted equations or fitted means. That text may help identify what the app calculated, but the document should show how to calculate it in R.

## Current Stage 44.8 position

Stage 44.8 adopted the following provisional rules:

- Models containing only factor predictors generate fitted means over the complete factor-level grid.
- Confidence intervals are attached to those fitted means because they demonstrate a prediction calculation not otherwise visible in the coefficient table.
- Models containing continuous predictors retain explicit fitted-equation code based on `coef(modelFit)`.
- The separate section giving confidence intervals for every coefficient is disabled for newly created recipes.
- User-requested predictions and contrasts retain their own intervals.
- The export contains no language-model interpretation.

These rules are coherent enough to retain for now, but they should not be treated as the final educational design.

## Remaining ambiguity

The unresolved issue is not simply which confidence intervals to display. It is the broader question of which calculations genuinely help a student move from WMFM to independent R work.

Examples of questions still requiring judgement include:

- When is an explicit fitted equation more helpful than a table of fitted values?
- For models containing continuous predictors, should the export also show predictions at typical values?
- Should fitted-value confidence intervals be included automatically, only when they were displayed in the app, or only when they teach a distinct calculation?
- When does output merely duplicate information already available in the regression table?
- How much explanatory prose is needed to connect code blocks without turning the analysis into an interpreted report?
- Should the exported analysis reproduce every deterministic calculation made during the session, or only a stable core workflow?

These questions should be revisited using actual generated analyses and teaching experience rather than extending the implementation speculatively.

## Recommended review method

Before making further structural changes, generate and inspect a varied set of exported analyses, including:

- a simple continuous-predictor linear model;
- a factor-only linear model;
- a mixed continuous-and-factor model;
- a model with an interaction;
- logistic and Poisson models;
- a model with a requested mean-response prediction;
- a model with an individual prediction interval; and
- a model with one or more requested contrasts.

For each document, ask:

1. Does this code teach the student how to reproduce a calculation performed by WMFM?
2. Does the section add something not already obvious from earlier output?
3. Is the calculation expressed using ordinary, readable R?
4. Would a student reasonably include this step in an independent analysis?
5. Is the section present because it is educationally useful, or merely because WMFM happens to have the information?

A later stage should simplify or remove sections that repeatedly fail these tests.

## Plotting idea deliberately deferred

For models containing only factors, a future plotting stage may offer a choice between:

- boxplots, which provide a compact distribution summary; and
- beeswarm-style plots, which show the individual observations.

The fitted means and intervals could be layered consistently over either display.

This is a plotting enhancement and should remain separate from the unresolved design of the reproducible analysis export.

## Safe position when work resumes

The current implementation should be treated as a functional baseline rather than a final specification.

When the work resumes:

- begin by inspecting generated documents rather than immediately altering renderers;
- preserve the deterministic analysis-recipe architecture;
- keep numerical calculations outside the language model;
- keep language-model interpretation outside the reproducible code document;
- prefer standard R code over WMFM-specific runtime helpers; and
- make changes only where they improve the student's ability to reproduce the analysis independently.

The most important design question remains:

> Does this section help a student understand how to perform the analysis in R?

If the answer is unclear, the section should not be expanded until its educational purpose is clearer.
