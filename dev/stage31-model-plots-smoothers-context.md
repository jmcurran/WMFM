# Stage 31 - Model plots refinement: smoothers and density-aware display

## Context

This work stream follows Stage 30, which added the student-facing **Model plots** tab to WMFM.

Stage 30 introduced:

- a new `Model plots` tab
- observed-vs-fitted plots
- residuals-vs-fitted plots
- support for linear models, logistic regression, and Poisson regression
- model-aware labels and scales
- a short help/info control with the text:

```text
These plots help you notice whether the fitted model is missing obvious structure.
```

Stage 30 deliberately avoided presenting the plots as formal diagnostic tests or assumption checks. That pedagogical framing should continue in Stage 31.

The project should now begin from `master` after the completed `model-plots` branch has been merged.

## Preferred stage number

Use:

```text
Stage 31
```

The first substage should be:

```text
Stage 31.1
```

## Main goal

Refine the existing Model plots feature without adding lots of new plots.

The most important improvement is to add an optional smoother to the linear-model Model plots, especially to help students notice systematic trends that the fitted model has not captured.

The goal is visual support for interpretation, not formal assumption testing.

## Strong preference

Do **not** add many new plot types.

The existing plot set should remain deliberately small:

- observed vs fitted
- residuals vs fitted

Stage 31 should improve these plots rather than expanding the app into a diagnostic-plot gallery.

## Pedagogical framing

Keep the existing language and teaching approach from Stage 30.

Preferred framing:

```text
These plots help you notice whether the fitted model is missing obvious structure.
```

Avoid student-facing language such as:

```text
Model checking
Diagnostic plots
Assumption checks
Model validity
Pass/fail diagnostics
```

The plots should help students ask:

- Are fitted values broadly tracking observed outcomes?
- Is the model systematically wrong in some region?
- Is there an obvious pattern left over after fitting the model?
- Are there unusual observations that might deserve closer thought?

They should not imply that a model is simply valid or invalid.

## Main feature idea: optional smoother for linear model plots

I am especially interested in adding a smoother to the linear model plots.

Candidate behaviour:

- For `lm` observed-vs-fitted plots, show the fitted reference line as before.
- Optionally overlay a smooth trend through the observed points.
- For `lm` residuals-vs-fitted plots, optionally overlay a smooth trend through the residuals.
- The smoother should help identify systematic patterns not captured by the fitted model.
- It should not be described as a formal test.

Possible implementation options:

1. Add a checkbox such as:

```text
Show smooth trend
```

2. Default the smoother to on for linear models, or default it off and let students turn it on.

3. Use a simple `geom_smooth()` layer with no confidence band, unless there is a strong reason otherwise.

4. Prefer deterministic, offline behaviour.

5. Keep the smoother visually subordinate to the main reference line and points.

Design question for Stage 31.1:

- Should the smoother be shown by default, or should it be optional and off by default?

My current instinct is that it should probably be visible enough to be useful, but not make the plots look too busy.

## Line and styling considerations

Stage 30 made the reference lines more visible, including thicker red lines for relevant plots.

Stage 31 should preserve that readability.

If a smoother is added:

- Do not make it look like another fitted model line unless that is the intended meaning.
- Use a distinct visual style from the reference line.
- Consider a dashed or subdued line style.
- Avoid overloading students with too many visual encodings.

## Density-aware plotting for large datasets

A future idea from Stage 30 was density-aware plotting for dense datasets, especially the `ggplot2::diamonds` examples.

Examples of possible approaches:

- hexbins
- 2D density
- alpha-aware point rendering

This is useful because dense data can make ordinary scatter plots hard to read.

However, this should probably **not** be the first Stage 31.1 task unless it is needed to make the smoother work well.

Reasons to defer or handle carefully:

- Hexbins may require a new dependency or extra explanation.
- Density displays may be harder for introductory students to interpret.
- The app should not drift into a complex diagnostic plotting interface.

A conservative approach would be:

- first improve alpha/point rendering for dense plots if needed
- only consider hexbins or 2D density in a later substage
- avoid adding another plot type unless there is a clear teaching need

## Additional GLM visualisations

Additional GLM-specific visualisations were also mentioned as a future possibility.

At this stage, do **not** prioritise adding more GLM plots.

GLM model plots should remain cautious and introductory. Specialist GLM diagnostics are easy to overinterpret and may require too much extra teaching.

## Integration into model explanations

Do **not** integrate model plots into the generated explanation as a general feature.

The model explanation should not routinely discuss plots.

A possible exception might be a serious trend or obvious missed structure that the model has not captured, but even that should be treated cautiously and probably not implemented in Stage 31.

Do not add LLM-generated diagnostic interpretations in this stage.

If this idea is revisited later, it should probably be a separate work stream with careful safeguards.

## Download/export of model plots

Download/export of model plots is a possible future feature.

This should not be the main focus of Stage 31.1 unless it is very low risk.

Possible future behaviour:

- download the current Model plot as PNG
- maybe later include plots in a student report

For now, prioritise smoothing and clarity over export.

## Suggested Stage 31.1 scope

A small first stage could include:

1. Add an optional smoother to linear-model observed-vs-fitted plots.
2. Add an optional smoother to linear-model residuals-vs-fitted plots.
3. Add concise student-facing text explaining that the smoother can help reveal broad patterns.
4. Ensure the smoother is not described as an assumption test.
5. Add deterministic tests for:
   - smoother option visibility for `lm` models
   - smoother option not shown, or safely ignored, for unsupported model types
   - plot object includes a smoother layer when requested
   - plot object omits the smoother layer when not requested
   - student-facing text does not use banned diagnostic language

## Possible Stage 31.2 scope

Only after Stage 31.1 is stable, consider density-aware display for large datasets.

Candidate work:

- tune point alpha for dense data
- detect large `n` and adjust point alpha automatically
- consider a student-friendly density option
- avoid adding hexbin unless dependency and explanation costs are acceptable

## Testing expectations

Continue the Stage 30 testing style:

- deterministic offline tests
- no image snapshots
- inspect plot objects and helper outputs rather than rendered images
- no real LLM calls
- avoid network access
- keep UI tests focused on labels and availability rather than fragile formatting

## Development conventions

Continue using my usual WMFM/R conventions:

- `=` for assignment
- camelCase identifiers
- braces on all control structures
- roxygen2 documentation
- modular helper functions
- deterministic offline tests
- thin orchestration layers
- no real provider calls in tests

Use the relevant skills:

- `r-development-style`
- `wmfm-stage-script-generator`

If a stage runner is generated, it should follow the existing WMFM staged workflow, including:

- version bump before validation
- build number incremented on every attempt
- strict tests
- strict check
- `NEWS.md` update after successful validation
- commit
- completed zip
- package build and install
- ChatGPT bundle when the helper is available

## Initial design questions for the new chat

Before implementing Stage 31.1, discuss:

1. Should the smoother be shown by default for linear models?
2. Should the smoother be controlled by a checkbox?
3. Should the smoother appear on both observed-vs-fitted and residuals-vs-fitted plots?
4. What line style should distinguish the smoother from the fitted/reference line?
5. Should density-aware plotting be deferred until after smoothing is stable?

## Current preference

My current preference is:

- Stage 31 should focus first on smoothers for linear model plots.
- I do not want lots more plots.
- I do not want model plots integrated into explanations, except possibly much later for very serious missed trends.
- Density-aware plotting is interesting, especially for the diamonds examples, but can probably wait until after the smoother design is clear.
