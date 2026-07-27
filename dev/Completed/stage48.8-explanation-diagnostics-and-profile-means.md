# Stage 48.8: explanation diagnostics and user-defined fitted means

Stage 48.8 adds two related improvements to the student explanation workspace.

## Developer diagnostic export

When developer mode is unlocked, the explanation tab now exposes a downloadable Markdown diagnostic report. The report contains the model formula, model class, analysed sample size, variable types and factor levels, the exact model summary, the student's exact explanation text, and the complete deterministic grading object as an R `dput()` representation. The export deliberately excludes the complete underlying dataset.

This report is intended to make unexpectedly low formative scores reproducible and diagnosable without guessing which rubric evidence was retained by the grading object.

## Fitted means at supplied covariate values

The fitted-mean dialog now uses the same profile-entry interaction as the individual-prediction dialog. Students can supply one or more values for every model covariate. Singleton values are broadcast across a common profile length, while incompatible non-singleton lengths are rejected.

For linear models, the inserted quantity is labelled as an expected value and its interval is a confidence interval for the mean response. This remains distinct from the individual-prediction tool, whose interval is a prediction interval for a future outcome.
