# Stage 32 - Minor deferred polish items

## Context

This context follows the completion of Stage 28, which added and stabilised log-log model handling for WMFM, including the Diamonds II, Diamonds III, and Diamonds IV examples.

Stage 28 is complete. The remaining items here are small deferred polish issues that were noticed during Stage 28 testing but were not worth extending that stage further.

Use the normal WMFM staged workflow:

- use the `wmfm-stage-script-generator` skill
- use the `r-development-style` skill
- create downloadable change zips for manual installation
- create downloadable stage runner scripts
- increment the package build version in the established format
- keep changes small and well-tested

Coding preferences remain:

- use `=` for assignment
- use camelCase identifiers
- use braces for all control structures
- use roxygen2 documentation where needed
- prefer modular helpers
- avoid stage numbers in permanent source, test, or function names

## Goal

Polish minor wording and display issues left over from the Stage 28 log-log workflow, without changing the modelling architecture.

This stage should not revisit the core log-log implementation unless a small bug is directly required to support the polish items below.

## Deferred polish items

### 1. Deterministic follow-up answer wording

Current behaviour is acceptable but slightly abrupt. Some deterministic appended follow-up answers begin with wording like:

```text
Yes. For these data, accounting for cut, color, clarity substantially improves the in-sample predictions compared with using weight alone.
```

Preferred direction:

- avoid starting deterministic follow-up answers with just `Yes.`
- restate the follow-up question in statistically natural language first
- then give the answer
- keep the answer concise
- avoid exposing technical diagnostics unless explicitly asked

Example preferred style:

```text
Accounting for cut, color, and clarity does substantially improve the in-sample predictions compared with using weight alone. This comparison describes how well the fitted models describe these observed diamonds, not performance on a separate test set or cross-validation.
```

This should apply generally to deterministic follow-up answers where possible, but the known motivating example is the Diamonds IV adjustment-comparison follow-up.

### 2. Final check on confidence interval wording

Stage 28.8.13.1 replaced compact `95% c.i.` wording with `95% confidence interval` in student-facing explanations and updated post-processing to catch lowercase `c.i.`.

This stage should verify that:

- deterministic formatters are not still generating `c.i.` for student-facing text
- post-processing catches common forms such as:
  - `95% c.i.`
  - `95% c.i.:`
  - `95% C.I.`
  - `95% CI`
- developer diagnostics may retain compact terms if appropriate, but student-facing explanations should use `95% confidence interval`

If the tests already cover this sufficiently, leave it alone.

### 3. Student-facing transformation wording

Some log-log explanations may still mention technical wording such as:

```text
on a log scale
```

This is sometimes acceptable, but the Stage 28 direction was:

- internally: `log-log model`
- externally: proportional-change language

Preferred student-facing wording:

```text
A percentage increase in diamond weight is associated with a percentage increase in expected price.
```

rather than emphasising the log transformation machinery.

This is a polish item only. Do not remove useful clarification where it is needed for correctness.

### 4. Follow-up placeholder styling check

Stage 28.8.13 added a shorter optional follow-up placeholder and lighter placeholder styling.

Check that:

- the placeholder is visually distinct from actual typed text
- the placeholder is not so specific that it looks like an example-loaded follow-up question
- tests no longer expect the old long placeholder text

Do not spend much time on this unless the current UI still makes the placeholder look like entered content.

### 5. Developer diagnostics accordion visual consistency

During Stage 28, the developer prompt diagnostics section appeared to behave like an accordion but did not visually look like the other accordion sections.

This may already be covered in the separate UI/provider polish context, but if this stage is being used for small UI polish, consider making the diagnostics section visually consistent.

Keep this small. Do not reorganise the whole developer panel in this stage.

## Things explicitly out of scope

Do not include the larger UI/provider persistence workstream here unless the user explicitly asks. That larger workstream includes:

- remembering the selected provider
- remembering the selected model
- remembering developer mode state
- hardening Ollama model discovery
- renaming provider test files that contain stage numbers
- broader developer-panel reorganisation

Do not include MathJax fitted-equation rendering here. That should be a separate stage because it touches equation rendering more broadly.

Do not include derived-variable provenance or transformation registry work here. That should also be a separate modelling infrastructure stage.

## Suggested first step

Start with the deterministic follow-up answer wording, especially the adjustment-comparison case.

A useful test target is the Diamonds IV follow-up:

```text
Does adjusting for cut, color, and clarity improve our predictions substantially?
```

Expected student-facing answer should:

- directly answer the follow-up
- not start with a bare `Yes.`
- say that accounting for cut, color, and clarity substantially improves the in-sample predictions compared with using weight alone
- state that this is an in-sample fitted-model comparison, not evidence from a separate test set or cross-validation
- not mention log-likelihood, likelihood-ratio test, deviance, AIC, adjusted R-squared, or residual standard error in the student-facing explanation

## Suggested validation

Run the usual WMFM validation workflow:

```bash
R -q -e "devtools::document()"
R -q -e "devtools::test()"
R -q -e "devtools::check()"
```

If this stage only changes tests and R source without roxygen changes, still run `devtools::document()` to confirm generated documentation is not stale.

## Completion criteria

Stage 32 can be considered complete when:

- all tests pass
- the Diamonds IV deterministic follow-up answer reads naturally
- student-facing explanations avoid unnecessary `c.i.` and technical transformation leakage
- no permanent source, test, or function names include stage numbers
- any UI changes are small and do not broaden the scope into provider persistence or equation rendering
