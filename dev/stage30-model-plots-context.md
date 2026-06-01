# Stage 30 - Model plots for fitted models

## Goal

Add a student-facing **Model plots** section to the WMFM app.

The purpose is to help students see what the fitted model is doing and notice obvious structure that the model may be missing. This stage should avoid turning the app into an assumption-checking or formal diagnostic-testing tool.

## Background

WMFM was designed for a course that is heavily oriented around the linear model. The surrounding teaching tradition often emphasises three linear-model assumptions:

1. The errors are independent.
2. The errors are normally distributed and centred around zero.
3. The variance is constant.

The app should not reinforce the idea that modelling is mainly about performing an assumption-checking ritual. In particular, avoid student-facing language that implies that a model is simply "valid" or "invalid" based on a plot.

Many people who have seen the app have nevertheless asked for diagnostic-style plots. This stage should respond to that need in a way that fits the app's teaching goals.

## Terminology

Use the label:

```text
Model plots
```

Do **not** use these labels in the student-facing UI:

```text
Model checking
Diagnostic plots
Assumption checks
Model validity
```

The phrase "model checking" is especially undesirable because it connects too strongly to existing teaching material that over-emphasises assumption checking.

## Pedagogical principle

The plots should be framed as visual summaries of model fit and model behaviour.

Preferred tone:

```text
These plots help you notice whether the fitted model is missing obvious structure.
```

Avoid tone like:

```text
These plots determine whether the model assumptions are satisfied.
```

The plots should help students ask:

- Are the fitted values broadly tracking the observed outcomes?
- Is the model systematically wrong in some region?
- Is there an obvious pattern left over after fitting the model?
- Are there unusual observations that might deserve closer thought?

They should not be presented as proof that a model is correct or incorrect.

## Suggested initial scope

Stage 30.1 should be deliberately small.

Add a new tab or section called:

```text
Model plots
```

The first implementation should include only two core plot types:

1. Observed vs fitted
2. Residuals vs fitted

Do not implement the full traditional base-R diagnostic suite in the first stage.

## Linear-model plots

For `lm` models, support:

### Observed vs fitted

Purpose:

- Show how closely fitted values track observed responses.
- Give students an intuitive sense of prediction error.
- Reinforce that a fitted model produces expected or predicted responses.

Teaching note should explain:

- Points close to the reference line indicate fitted values close to observed values.
- A strong pattern away from the line may suggest that the model is missing structure.
- The plot does not prove that the model is correct.

### Residuals vs fitted

Purpose:

- Show what is left over after the fitted model has been used.
- Help students notice systematic patterns in residuals.
- Provide a gentle entry point to ideas like nonlinearity and unequal spread without foregrounding assumption-checking language.

Teaching note should explain:

- Residuals scattered around zero with no obvious pattern are generally easier to reconcile with the fitted model.
- Curves, funnels, or clusters may suggest that the model is missing structure.
- This plot does not identify the correct alternative model by itself.

## GLM plots

GLMs should be handled cautiously.

There are few universally helpful regression diagnostic plots for introductory GLM teaching. The app should not introduce specialist GLM diagnostics that are difficult to explain or easy to misuse.

### Logistic regression

Support:

1. Observed outcome vs predicted probability
2. Deviance residuals vs fitted probability

For observed outcome vs predicted probability:

- Plot observed binary outcomes against fitted probabilities.
- Consider light jittering where useful.
- Make clear that binary outcomes will naturally appear at the two outcome levels.
- The plot should help students see how predictions vary across observations.

For residuals vs fitted probability:

- Use deviance residuals or another deterministic residual type chosen consistently.
- Explain that residuals are measured on a model-based scale, not simply observed minus fitted probability.
- Avoid detailed technical discussion unless placed in a developer note.

### Poisson regression

Support:

1. Observed count vs fitted count
2. Deviance residuals vs fitted count

For observed count vs fitted count:

- Show whether larger fitted counts tend to correspond to larger observed counts.
- Make clear that counts are discrete and can be noisy.

For residuals vs fitted count:

- Use deviance residuals or another deterministic residual type chosen consistently.
- Explain that the plot is intended to reveal obvious leftover patterns, not to certify the model.

## What not to include initially

Do not include these in Stage 30.1:

- Normal Q-Q plots
- Scale-location plots
- Residuals vs leverage
- Cook's distance
- Influence plots
- Half-normal plots
- Specialist GLM influence diagnostics
- Automatic pass/fail diagnostic rules

Reasons:

- Students often overinterpret normal Q-Q plots.
- Leverage and influence require substantial extra teaching.
- GLM diagnostics are harder to explain clearly.
- The app currently does not provide a full workflow for model revision, robust regression, variance modelling, or mixed models.
- Showing many plots risks making diagnostics seem more central than the research question.

These can be revisited in later stages if a clear teaching need emerges.

## UI requirements

Add a student-facing area named:

```text
Model plots
```

Possible placement:

- A new top-level tab beside Plot, Contrasts, and Model Explanation; or
- A section within the existing Plot tab if that is less disruptive.

The UI should:

- Let the student select the available plot type.
- Use model-aware labels.
- Show only plots that make sense for the fitted model.
- Include a short explanatory note for the selected plot.
- Avoid long text blocks.
- Avoid jargon where possible.

Suggested explanatory-note structure:

```text
What this plot shows
What to look for
What this plot cannot prove
```

## Plot data and implementation requirements

Keep plot generation deterministic and offline.

Prefer modular helper functions that prepare plot data separately from UI rendering.

Suggested helpers:

- buildModelPlotData()
- buildObservedFittedPlotData()
- buildResidualFittedPlotData()
- buildModelPlotTeachingNote()

The exact names can be adjusted to fit existing package style.

Tests should focus on deterministic plot-data helpers and teaching-note helpers rather than fragile image snapshots.

## Testing expectations

Add offline deterministic tests for:

- plot-data generation for `lm`
- plot-data generation for logistic regression
- plot-data generation for Poisson regression
- residual type selection
- fitted-value scale used for each model family
- teaching-note text includes the intended cautionary language
- unavailable plot types are not shown for unsupported model objects
- the UI contains "Model plots" and does not contain "Model checking" as a student-facing label

Avoid tests that require a real LLM, network access, or image snapshot comparisons.

## Development conventions

Continue to use:

- `=` assignment
- camelCase identifiers
- braces on all control structures
- roxygen2 documentation
- modular helper functions
- deterministic offline tests
- thin orchestration layers
- no real provider calls in tests

## Suggested first stage

Use:

```text
Stage 30.1
```

Suggested Stage 30.1 deliverables:

1. Add the Model plots UI.
2. Add observed-vs-fitted and residuals-vs-fitted plot data helpers.
3. Support `lm`, logistic regression, and Poisson regression.
4. Add short teaching notes.
5. Add deterministic tests for helper outputs and key UI labels.
6. Update `NEWS.md` with the successful package version including build number.

## Non-goals for Stage 30.1

Do not redesign model fitting.

Do not add automated diagnostic judgments.

Do not add LLM-generated diagnostic interpretations.

Do not add advanced model-revision advice.

Do not add a new model-selection workflow.

Do not use the phrase "model checking" in the student-facing UI.

Do not attempt to solve the separate issue where "explaining the explanation" does not work correctly when there is a follow-up question. That should be a separate later stage, possibly Stage 33.
