# WMFM Stage 26 initial audit: x-unit change follow-up explanations

## Recommendation

Use a local workflow for the audit and first implementation step. The codebase already contains most of the deterministic follow-up machinery needed for this stage, and the required changes are likely to be targeted R-package edits plus tests. Codex would be useful only if a later audit reveals a broad prompt/assembler redesign or widespread test failures across many unrelated files.

## Reasoning

Stage 26 is conceptually delicate, but technically close to existing code. The current archive already has:

- a deterministic follow-up classifier
- existing unit-change detection stubs
- LM and GLM deterministic prediction payload helpers
- deterministic follow-up answer assembly
- diagnostics JSON/UI plumbing
- packaged example support, including package-backed datasets from s20x

The risky part is not file volume; it is the design boundary: x-unit change requests must reframe the effect interpretation rather than append a prediction-style paragraph.

## Current archive inspected

Uploaded archive:

- stage25_10_completed.zip

Observed package version in DESCRIPTION:

- 0.2.1.18

Stage 26 should therefore reset/start at:

- 0.2.2.001

Because the user requested every build to receive a new build number regardless of success, the stage runner for this workstream should not use the previous WMFM default of bumping only after document/test/check passes. For Stage 26, the runner should bump to the next 0.2.2.xxx build number before validation/build attempts, or have a dedicated early version-bump step for each attempted build.

## Existing follow-up architecture

Key files observed:

- R/model-question-classifier.R
- R/model-question-prediction-lm.R
- R/model-question-prediction-glm.R
- R/model-question-followup-answer.R
- R/model-lm-explanation.R
- R/app-explanation-diagnostics-ui.R
- R/app-server-fit-model.R

The classifier already has an `alternative_unit_change` category and an `extractRequestedUnitChangeValues()` helper. This means Stage 26 should probably evolve the existing category rather than bolt on a completely separate pathway from scratch. However, the context asks for a category such as `unit_change_request`, so I recommend renaming or aliasing the classifier output to `unit_change_request` while preserving backward compatibility for any existing tests that mention `alternative_unit_change`.

## Current gap

The current deterministic enrichment path only enriches prediction categories:

- prediction_request
- prediction_interval_request

It returns unchanged payloads for other follow-up categories. Therefore an x-unit request can be classified but is not yet converted into a deterministic effect payload.

The explanation path then calls:

- `lmToExplanationPrompt(model)`
- `chat$chat(prompt)`
- `normaliseNumericExpressions(output)`
- `appendDeterministicFollowupAnswer(explanation = output, model = model)`

This is suitable for prediction follow-ups but not sufficient for unit-change reframing. The new unit-change payload should be available to prompt construction before the LLM call, and the deterministic appending function should explicitly avoid adding a prediction-style paragraph for unit-change requests.

## Suggested Stage 26 breakdown

### Stage 26.1: audit, version-rule setup, and diamond example

- Add the new package-backed example using `s20x::diamonds.df`.
- Use `price` as the response and carat weight as the numeric covariate, subject to the actual variable names in `diamonds.df`.
- Add a research question about how diamond price changes with carat weight.
- Add a follow-up question requesting a slope explanation for a 0.01 or 0.1 carat increase.
- Add tests that the example is discoverable and loads.
- Update the stage runner so the version begins at 0.2.2.001 and increments the build number on every build attempt.

### Stage 26.2: parser and resolver

- Rename or alias `alternative_unit_change` to `unit_change_request`.
- Extract requested unit size, including decimal values such as 0.01 and 0.1.
- Resolve the requested predictor conservatively against numeric model predictors.
- Return `needs_input` or `clarification_required` when the predictor is ambiguous or missing.

### Stage 26.3: deterministic effect computation

- For ordinary LM models, multiply the coefficient and coefficient confidence interval by the requested unit size.
- For log-link GLMs, exponentiate coefficient times requested unit size and transform the confidence interval the same way.
- For binomial logit models, report odds multipliers for the x-unit increase; do not invent probability changes unless deterministic baseline values are supplied.

### Stage 26.4: prompt payload and final assembly

- Add a prompt block containing the deterministic unit-change interpretation.
- Instruct the LLM to use the deterministic transformed quantity and avoid recomputing it.
- Ensure the final explanation uses the requested x-unit wording.
- Avoid appending a separate prediction-style follow-up paragraph.

### Stage 26.5: diagnostics

- Expose the raw follow-up, category, requested predictor, requested unit change, original effect, transformed effect, transformed confidence interval, model type, effect scale, and final prompt excerpt in developer diagnostics JSON.

### Stage 26.6: regression suite

- Add deterministic offline tests for classifier, parser, LM scaling, GLM scaling, final wording, non-appending behavior, diagnostics JSON, and preservation of existing prediction follow-up behavior.

## Local versus Codex

Recommended: local first.

Reasons:

- The implementation is close to existing deterministic helpers.
- You need careful manual UI/package testing with stage runners and downloadable zips.
- The versioning requirement is unusual and should be encoded deliberately in the generated script.
- The new diamonds example is small and should be easy to add locally.

Use Codex later only if:

- the prompt-construction files are much more entangled than expected,
- Stage 26 requires a broad rework of explanation assembly,
- or the local test suite reveals many unrelated regressions that need large-scale inspection.

## Immediate next local action

Create branch:

```bash
git checkout master
git pull --ff-only origin master
git checkout -b x-unit-followup
git push -u origin x-unit-followup
```

Then implement Stage 26.1 locally with a change zip and runner.

