# Stage 29 - Diagnostic plots for models

## Goal

Add model diagnostic plots that help students assess fitted models in the WMFM app.

## Motivation

The app currently focuses on fitting models, equations, confidence intervals, explanations, and follow-up questions. The next teaching gap is helping students see whether a fitted model is plausible.

Diagnostic plots should be student-facing and educational rather than just reproducing base R defaults.

## Suggested scope

1. Identify the first diagnostic plots to support for `lm` models.
2. Decide which diagnostics are appropriate for GLMs.
3. Add a model diagnostics UI panel or section.
4. Keep plot generation deterministic and offline.
5. Add tests for plot-data helpers rather than fragile image snapshots.
6. Provide plain-language notes explaining what each diagnostic plot is checking.

## Candidate diagnostics

For linear models:

- residuals versus fitted values
- normal Q-Q plot
- scale-location plot
- residuals versus leverage or influence summary

For GLMs:

- deviance or Pearson residuals versus fitted values
- response-scale fitted values versus observed outcomes where useful
- simple influence or leverage diagnostics if they can be explained clearly

## Teaching requirements

Each plot should include a short note covering:

- what pattern students should look for
- what a concerning pattern might mean
- what the plot does not prove

## Development conventions

Continue to use:

- `=` assignment
- camelCase identifiers
- braces on all control structures
- roxygen2 documentation
- modular helper functions
- deterministic offline tests
- thin orchestration layers
