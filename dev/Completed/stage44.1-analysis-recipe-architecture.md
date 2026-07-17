# Stage 44.1: Reproducible analysis architecture

## Purpose

Stage 44 introduces deterministic generation of a reproducible analysis document from the current WMFM analysis state. The initial idea of a Code panel is broadened into a reproducible analysis report that contains the R code used to create each substantive result.

This document is the reference architecture for the remainder of the WMFM 1.1.1 development line. Stage 44.1 changes no package behaviour. It records the design decisions that subsequent implementation stages should follow.

## Primary goal

WMFM should be able to produce a complete reproducible analysis corresponding to the current analysis state.

The generated document should:

- load or reconstruct the selected data;
- reproduce data preparation performed by WMFM;
- fit the selected model;
- reproduce the substantive statistical outputs computed by WMFM;
- contain clear, student-facing R code rather than a dump of internal application objects;
- use deterministic package state rather than language-model-generated code;
- remain usable outside the active Shiny session.

The document is not merely a transcript of the interface. It is a structured statistical analysis that can be rerun, inspected, modified, and used for teaching.

## Design principles

### Deterministic code generation

All generated code must come from WMFM's verified analysis state. The language model must not invent, reconstruct, or repair R code for the report.

The same deterministic quantities used by the application should be reproducible from the generated code. Tests should compare the recipe and rendered code with the calculations actually performed by WMFM.

### Statistical workflow rather than UI layout

The report should follow the statistical workflow:

1. load packages;
2. load data;
3. prepare data;
4. fit the model;
5. inspect and analyse the fitted model;
6. reproduce optional post-fit analyses;
7. record session information.

The report should not mirror the current arrangement of Shiny tabs. The interface is one way of interacting with the analysis, but it is not the analysis architecture.

### Capture state after fitting, render on demand

The core analysis recipe should be created or refreshed immediately after the user selects **Fit model** successfully.

The document itself should normally be written only when the user requests a download. This separates two responsibilities:

- capturing the authoritative analysis state at model-fitting time;
- rendering that state into a portable document at download time.

Post-fit actions, including plot choices, predictions, and contrasts, may update optional parts of the recipe before download.

### Separate analysis state from document format

The core architecture must not depend on Quarto. Quarto is the first document renderer, not the analysis representation.

The intended separation is:

```text
AnalysisRecipe
      |
      v
DocumentRenderer
      |
      +-- Quarto
      +-- R script
      +-- HTML
      +-- Word
      +-- PDF
```

Only the Quarto renderer is in the initial Stage 44 scope. Other renderers are possible future extensions.

### Include substantive analysis, not internal machinery

The report should include every substantive statistical result that WMFM routinely computes as part of the fitted analysis, whether or not the user opened the corresponding tab.

It should not include internal caches, prompt evidence, language-model scoring objects, UI-only state, or intermediate objects whose only purpose is application rendering.

## AnalysisRecipe

### Role

`AnalysisRecipe` is an internal deterministic object describing what WMFM did and what is required to reproduce it.

It should contain structured information, not a completed Quarto document and not copied console output. Renderers should turn the recipe into code and prose.

The initial implementation may use an S3 list with a validated class. A more elaborate class system should be introduced only if the developing behaviour justifies it.

### Provisional structure

A provisional shape is:

```r
analysisRecipe = list(
  schemaVersion = 1L,
  data = list(),
  preparation = list(),
  model = list(),
  summary = list(),
  anova = list(),
  confidenceIntervals = list(),
  diagnostics = list(),
  modelPlot = list(),
  predictions = list(),
  contrasts = list(),
  output = list()
)
```

This is an architectural guide rather than a frozen field specification. Stage 44.2 should audit existing objects before finalising names and contents.

### Responsibilities

The recipe should record enough information to:

- identify the data source;
- reproduce supported data loading;
- reproduce derived variables and transformations;
- reproduce factor conversion and reference levels;
- reproduce the model formula, family, link, and fitting call;
- reproduce confidence levels and interval methods;
- reproduce diagnostic and substantive plot settings;
- reproduce completed prediction requests where supported;
- reproduce completed contrast requests where supported;
- determine which document sections should appear;
- package uploaded data safely where required.

The recipe should not contain language-model explanations or require a language-model call to render the report.

## Recipe lifecycle

### Creation

After a successful model fit, WMFM should create a new core recipe from the authoritative fitted-model state.

A new fit invalidates post-fit components belonging to the previous model. Predictions, contrasts, and selected plot settings must not silently carry across incompatible fits.

### Updates

The recipe may be updated when the user performs a substantive post-fit action, including:

- selecting or changing model-plot options;
- requesting a deterministic prediction;
- defining or calculating a contrast;
- changing a confidence level or supported interval method.

Updates should be explicit and testable. They should not mutate unrelated recipe sections.

### Rendering

When the user requests the report, WMFM should:

1. validate the current recipe;
2. determine the appropriate download form;
3. render the current recipe into Quarto source;
4. include uploaded data or other required companion files;
5. return either a `.qmd` file or a ZIP archive.

The first implementation should not require the Shiny server to render HTML, Word, or PDF. The portable source document is the primary deliverable.

## Quarto as the first renderer

Quarto is preferred for new development because it provides a current document framework while retaining R execution through knitr.

The initial generated file should be named clearly, for example:

```text
wmfm_analysis.qmd
```

The document should contain ordinary, readable R code. It should avoid direct references to temporary Shiny objects or application-internal helper calls unless those calls are deliberately exported and form part of the supported reproducibility API.

## Proposed document structure

The initial Quarto report should use the following statistical order.

### Analysis overview

A brief generated introduction may identify the selected response, predictors, and model purpose. It should remain concise and should not depend on an LLM explanation.

### Load packages

Load only packages required by the generated analysis.

The renderer should avoid unnecessary package attachment and should use stable, student-readable code.

### Load the data

Reproduce the selected data source using one of the supported pathways described below.

### Prepare the data

Reproduce preparation required by the fitted model, including supported:

- derived variables;
- predictor transformations;
- response transformations;
- factor conversion;
- factor reference-level changes;
- deterministic recoding or validation steps that materially alter the analysis data.

This section is essential. A model call is not reproducible when it relies on invisible preparation performed inside the application.

### Fit the model

Generate a clear model-fitting call using the same formula, model family, and link as WMFM.

The generated object should use a stable student-facing name such as:

```r
modelFit = lm(...)
```

### Model summary

Reproduce the ordinary model summary, initially through the standard applicable method:

```r
summary(modelFit)
```

### Analysis of variance

Reproduce the ANOVA or analysis table that WMFM treats as the authoritative table for the fitted model.

Stage 44.2 must audit whether this is always the direct result of `anova(modelFit)` or whether existing pathways use different methods or term tests.

### Confidence intervals

Reproduce the intervals displayed or routinely computed by WMFM using the same confidence level, scale, and method.

The renderer must distinguish raw model coefficients from response-scale, odds-ratio, expected-count, or other interpreted intervals. It must not silently substitute a simpler interval calculation for the one used by WMFM.

### Diagnostic plots

Include the standard diagnostic plots appropriate to the fitted model. These are distinct from the substantive model plot.

The initial audit must determine:

- which diagnostics are always computed;
- which diagnostics are model-family specific;
- whether current plots depend on internal data structures or can be reproduced with ordinary R calls;
- which plot settings must be retained in the recipe.

### Model plot

Include the substantive fitted-model plot when a meaningful plot is available.

The generated code should reproduce the current WMFM plot settings rather than inventing a generic plot. This section is conditional.

### Predictions

Include deterministic prediction code for completed supported prediction requests. This includes the Stage 43 distinction between:

- confidence intervals for expected or mean responses;
- prediction intervals for individual linear-model outcomes;
- binomial probabilities or odds where requested;
- Poisson expected counts;
- supported transformed predictors and transformed responses.

Predictions are optional and may be added to the recipe after fitting.

### Contrasts

Include contrast code only when the user has defined or requested a contrast that WMFM has calculated.

Contrasts are not frozen at model-fitting time. They are dynamic post-fit analyses and should update the relevant recipe component.

The report must reproduce the actual contrast definition, confidence level, variance method, and interpretation scale. Stage 44 should not claim reproducibility until the renderer can do this faithfully.

### Session information

Include reproducibility metadata at the end, initially through:

```r
sessionInfo()
```

This section is developer-oriented but useful and conventional in a reproducible report.

## Always-present and conditional sections

### Initially always present

- analysis overview;
- load packages;
- load data;
- prepare data, even if it only states that no additional preparation was required;
- fit the model;
- model summary;
- analysis of variance, when defined for the fitted pathway;
- confidence intervals, when WMFM computes them routinely;
- diagnostic plots, when defined for the fitted pathway;
- session information.

### Conditional

- substantive model plot;
- deterministic predictions;
- contrasts;
- model-family-specific diagnostics or analyses;
- uploaded-data packaging instructions.

A section should decide whether it appears from recipe state, not from whether the user happened to visit a UI tab.

## Data-source handling

### Package datasets

Package datasets should be reproduced through explicit package and dataset identification. The rendered code should avoid relying on whichever objects happen to exist in the user's global environment.

A typical pattern may be:

```r
library(s20x)
data("Course", package = "s20x")
analysisData = Course
```

Stage 44.2 should audit how WMFM currently records package and dataset names and whether all supported package datasets can be loaded this way.

### Uploaded datasets

Temporary Shiny upload paths must never appear in the generated report.

For uploaded data, the preferred deliverable is a ZIP archive with relative paths, for example:

```text
wmfm_analysis/
|-- wmfm_analysis.qmd
`-- data/
    `-- original_filename.csv
```

The document should then use a relative path such as:

```r
analysisData = read.csv("data/original_filename.csv")
```

The renderer should preserve a safe version of the original filename where practical, but it must avoid exposing private absolute paths.

Stage 44.2 must audit the supported upload formats. CSV and delimited text are likely straightforward. RDA and other object-based formats require explicit decisions about object selection, naming, and safe packaging.

### Portability and self-containment

A report should not be described as self-contained unless every required input file and preparation step is included or otherwise available through a stable package dataset reference.

When complete self-containment is not possible, the generated document should state the remaining dependency accurately.

## Rendering architecture

Rendering should be modular. A provisional internal API may include functions such as:

```r
renderAnalysisRecipe()
renderDataSection()
renderPreparationSection()
renderModelSection()
renderSummarySection()
renderAnovaSection()
renderConfidenceIntervalSection()
renderDiagnosticSection()
renderModelPlotSection()
renderPredictionSection()
renderContrastSection()
```

These names are provisional. The important decision is that the Shiny server should not build the report through scattered string concatenation.

Each renderer should have a narrow responsibility:

- validate the relevant recipe component;
- determine whether its section is present;
- emit deterministic student-facing code;
- avoid unrelated application internals;
- provide testable output.

## User-interface decision

The initial user interface should be a **Download analysis** action near the fitted-model controls.

A new tab is not required for the first useful implementation. A later Reproducibility or Code tab may preview sections or display selected code, but it should use the same recipe and renderer rather than introduce a second code-generation pathway.

The download action should remain unavailable until a model has been fitted successfully.

## Testing strategy

All package tests must remain offline and deterministic.

Testing should operate at several levels.

### Recipe validation tests

Verify that recipe objects:

- contain required core fields;
- reject incomplete or incompatible state;
- reset post-fit analyses after a new model fit;
- preserve supported data-source metadata;
- update only the intended optional section.

### Section-rendering tests

Verify that each renderer:

- emits the expected code for representative models;
- omits unavailable optional sections;
- uses relative paths for uploaded data;
- uses the same formula, family, link, confidence level, and settings as the fitted analysis;
- does not expose temporary paths or internal Shiny object names.

### Behaviour correspondence tests

For representative analyses, evaluate or inspect the generated code and confirm that it corresponds to WMFM's deterministic calculation.

Tests should cover at least:

- a package dataset and ordinary linear model;
- a binomial model;
- a Poisson model;
- a transformed predictor;
- a transformed response;
- mean-response and individual linear-model predictions;
- an uploaded delimited file;
- a supported contrast when contrast rendering is implemented.

### End-to-end download tests

When UI download support is introduced, verify:

- `.qmd` download for package datasets;
- ZIP download for uploaded datasets;
- expected archive structure;
- absence of private temporary paths;
- stable operation without a real language-model call.

## Implementation roadmap

### Stage 44.2: existing-state audit and recipe core

- audit the current model-fit, data-source, preparation, summary, ANOVA, interval, diagnostic, plot, prediction, and contrast pathways;
- identify authoritative existing objects and missing reproducibility information;
- finalise the minimum `AnalysisRecipe` schema;
- implement construction, validation, and model-refit reset behaviour;
- add offline unit tests;
- do not yet expose a download action.

### Stage 44.3: data, preparation, and model rendering

- render package loading;
- render package-dataset loading;
- package uploaded delimited data safely;
- render supported preparation and transformations;
- render model-fitting code;
- add correspondence tests.

### Stage 44.4: core analysis sections

- render model summary;
- render the authoritative ANOVA or term table;
- render confidence intervals;
- render diagnostic plots;
- render substantive model plots when available;
- add representative linear, binomial, and Poisson tests.

### Stage 44.5: dynamic analyses

- add deterministic prediction sections;
- add contrast sections;
- ensure post-fit recipe updates are isolated and reset appropriately;
- cover transformed predictors, transformed responses, uncertainty types, and robust contrast settings.

### Stage 44.6: download integration

- add the **Download analysis** UI action;
- render `.qmd` files for package datasets;
- render ZIP archives for uploaded datasets;
- add end-to-end Shiny and archive-structure tests;
- document current limitations accurately.

The stage boundaries may be adjusted after the Stage 44.2 audit, but the architecture and separation of responsibilities should remain stable.

## Decisions made in Stage 44.1

Stage 44 will proceed on the following basis:

1. WMFM will generate a reproducible analysis document rather than only isolated code snippets.
2. The report will follow the statistical workflow rather than the UI tab layout.
3. A deterministic `AnalysisRecipe` will be captured after a successful model fit and updated by supported post-fit analyses.
4. The report will be rendered on demand rather than written repeatedly during ordinary app use.
5. Quarto will be the first renderer, but the recipe will remain output-format independent.
6. Package datasets will use explicit package references.
7. Uploaded data will use portable relative paths and will normally be packaged with the document in a ZIP archive.
8. A **Download analysis** action is preferred over a new tab for the initial interface.
9. Contrasts and predictions are dynamic optional sections, not immutable parts of the initial fit.
10. All generated code will be deterministic and tested against WMFM's authoritative calculations.

## Deferred decisions

The following require code-path audit in Stage 44.2:

- the exact recipe field schema and class implementation;
- the authoritative ANOVA or term-test method for each model pathway;
- the exact confidence-interval functions and scales to reproduce;
- the complete set of supported upload formats;
- the representation of derived-variable expressions;
- the best student-facing code for current diagnostic and model plots;
- whether any internal helpers should become exported reproducibility APIs;
- the minimum Quarto dependency and availability checks required by the application.

These are implementation decisions, not reasons to weaken the architecture established here.
