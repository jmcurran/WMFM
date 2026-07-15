# WMFM 1.1.0 development context: Stages 43 to 45

## Purpose

The released WMFM version is 1.0.4. The next substantial development line is 1.1.0, using four-part development versions of the form `1.1.0.xxx`. Stage 43 begins at `1.1.0.001`. The final three-digit component is incremented for every build attempt, including failed validation attempts.

The 1.1.0 development line contains three connected pieces of work identified after the conference presentation of WMFM.

## Stage 43: distinguish average-response inference from individual prediction

Natural-language questions do not always use statistical terminology precisely. For example:

- "Will I do well on the final exam if I attend class regularly and score 15/20 on the test?"
- "What mark would you predict I would get if I attended class regularly and scored 15/20 on the test?"

The first question is personally framed but ambiguous. It may refer to the expected mark for students with those characteristics or to the range of plausible outcomes for one student. WMFM should therefore report both:

1. the estimated mean response and its confidence interval; and
2. the individual prediction and its prediction interval.

The second question explicitly asks for an individual prediction. WMFM should lead with the predicted individual outcome and prediction interval, while still being able to show the corresponding mean-response estimate and confidence interval as an educational comparison.

Stage 43 should:

- classify prediction intent as `mean_response`, `individual_outcome`, or `ambiguous_personal`;
- retain an ambiguity indicator;
- compute both confidence and prediction intervals for supported personal ordinary-linear-model questions;
- preserve explicit mean-response and explicit prediction-interval requests;
- explain clearly that the point prediction may be the same while the two intervals answer different questions;
- keep all numerical calculations deterministic and prevent the language model from recomputing or inventing intervals;
- add offline tests for explicit individual prediction, ambiguous personal questions, and explicit average-response questions.

## Stage 44: generate the R code used for the analysis

Providing "the R code" is more complex than displaying the fitted model call. WMFM should eventually support several levels of reproducibility:

- the model-fitting call;
- code reproducing the result currently displayed;
- data preparation and derived-variable code;
- prediction, confidence interval, contrast, and plot code;
- an exportable analysis script.

The code must be generated deterministically from WMFM's actual analysis state rather than invented by the language model. The preferred architecture is an internal analysis-recipe object containing the data source, preparation steps, model formula and family, requested result, predictor settings, and relevant output settings. Separate renderers can then produce concise student-facing code or a fuller reproducible script.

Initial Stage 44 scope should include:

- a Code panel;
- model-fitting and summary code;
- code for mean-response and individual predictions introduced in Stage 43;
- copy and download actions;
- clear student-facing R rather than a literal dump of WMFM internals;
- tests showing that rendered code corresponds to the deterministic calculation.

Uploaded data, package datasets, factor levels, transformations, derived variables, and safe file paths need explicit design decisions before claiming that a downloaded script is completely self-contained.

## Stage 45: observation-level and residual-inspection questions

Some natural questions require examining observations relative to the fitted model rather than interpreting a mean effect. Examples include:

- "What is a good deal on a diamond?"
- "Which observations are unusually low relative to the fitted model?"
- "Which students performed better or worse than expected?"

For an ordinary linear model, the first bounded implementation should use observed values, fitted values, and residuals to identify unusually high or low observations. The result should include a meaningful observation identifier, observed response, fitted response, residual or ratio, and rank or percentile.

Important design issues include:

- raw versus standardised or studentised residuals;
- absolute residuals versus proportional differences;
- interpretation for log-response models;
- preserving useful identifier and descriptive columns;
- avoiding claims that a residual proves a bargain, anomaly, causal effect, or data error;
- deciding later whether conditional quantile regression is warranted.

The initial Stage 45 scope should remain with ordinary linear models and residual-based ranking. Quantile regression should be considered separately only after this simpler pathway is understood and tested.

## Shared architecture across Stages 43 to 45

All three stages should follow the same pattern:

1. classify the user's statistical target;
2. compute a deterministic result object;
3. render that object as a table, plot, explanation, and eventually R code;
4. let the language model explain only verified quantities supplied by WMFM.

This architecture keeps numerical work outside the language model and allows the code display introduced in Stage 44 to reproduce the same deterministic calculation used by the explanation.
