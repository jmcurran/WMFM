# Stage 44.3: Core Quarto analysis sections

## Purpose

Stage 44.3 implements the first deterministic Quarto renderer for the WMFM analysis recipe. It covers the beginning of the statistical workflow established in Stage 44.1:

1. load packages;
2. load the data;
3. prepare the data; and
4. fit the model.

The renderer produces portable, student-facing R code from the verified `wmfmAnalysisRecipe` state. It does not use an LLM and does not depend on the current Shiny session when rendering code.

## Bounded support

The first data-loading implementation supports:

- package datasets identified by package and dataset name; and
- uploaded CSV files, using the same `read.table()` settings as WMFM.

Portable TXT loading requires the selected separator to be retained in the recipe. Portable RDA/RData loading requires the selected data-frame object name to be retained. Stage 44.3 therefore fails explicitly for these formats rather than inventing code. Those metadata gaps should be addressed before the download workflow claims complete support.

## Preparation code

The preparation section reproduces:

- recorded derived-variable expressions; and
- variables explicitly converted to factors by WMFM.

The renderer uses `analysisData` as the stable student-facing data object.

## Model code

The fitted model is assigned to `modelFit`. The renderer distinguishes:

- `lm()` for ordinary linear models;
- `glm(..., family = binomial(link = "logit"))` for logistic models; and
- `glm(..., family = poisson(link = "log"))` for Poisson models.

## Deferred work

Stage 44.3 does not yet:

- write files to disk;
- add a download control;
- package uploaded data;
- render model summaries, ANOVA tables, confidence intervals, diagnostics, plots, predictions, or contrasts; or
- render TXT and RDA/RData loading code.

These remain later Stage 44 increments.
