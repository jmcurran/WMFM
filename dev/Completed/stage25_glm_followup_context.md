# WMFM Stage 25 context: GLM deterministic follow-up predictions

## Branch

Suggested branch:

```text
glm-followup
```

## Step 1: audit and branch setup

Before changing files:

1. Start from updated `master` after Stage 24 is complete and merged, unless explicitly directed otherwise.
2. Run the usual audit:
   - confirm the working tree is clean
   - confirm the current branch and latest commit
   - inspect existing LM follow-up prediction tests
   - inspect existing GLM prediction and explanation tests
   - identify the deterministic prediction helpers before editing
3. Check out the new branch and push it:

```bash
git checkout master
git pull --ff-only origin master
git checkout -b glm-followup
git push -u origin glm-followup
```

Use the WMFM stage workflow, downloadable change zips, downloadable stage runners, and incremental stage numbering beginning at Stage 25.

Use the `wmfm-stage-script-generator` skill and the `r-development-style` skill.

Do not use Codex unless explicitly requested.

## Current baseline

Stage 23 stabilized deterministic follow-up prediction handling for linear models.

Current working pieces include:

- bounded follow-up question handling
- deterministic follow-up classification
- model-aware factor resolution
- safe `needs_input` and `clarification_required` paths
- deterministic LM prediction payloads
- follow-up answers in separate paragraphs
- diagnostics JSON downloads with timestamped filenames
- developer diagnostics visibility

Stage 25 should extend this architecture to GLMs without destabilizing the linear-model path.

## Goal

Implement, test, and stabilize deterministic follow-up prediction support for GLM models.

The key principle is the same as for linear models:

1. parse the raw follow-up conservatively
2. resolve supplied predictor values against the fitted model
3. complete only safe/defaultable omitted predictors
4. compute deterministic fitted values and intervals on the correct interpretation scale
5. inject the deterministic payload into the explanation prompt
6. prevent the LLM from inventing or recomputing prediction values
7. append the deterministic answer in a separate paragraph

## Model types to cover

Prioritize common WMFM GLM paths:

- binomial/logit models
- count/log-link models if supported by the current package paths

For binomial/logit models:

- fitted values should be on the probability scale
- direct comparisons may use probability differences or odds multipliers only where deterministic support exists
- prediction intervals may not be available in the same way as LM individual prediction intervals
- confidence intervals should be labelled accurately

For count/log-link models:

- fitted values should be expected counts
- multiplicative effects should remain on the expected-count multiplier scale
- intervals should be back-transformed and described correctly

## Design constraints

Preserve all Stage 23 behaviour:

- LM follow-up examples must continue to pass.
- Follow-up paragraph separation must remain intact.
- Diagnostics JSON export must still work and include GLM payloads.
- Factor levels containing regex metacharacters must continue to resolve literally.
- Missing predictor handling must remain conservative.
- Cache-version expectations must not be changed.

Avoid broad prompt rewrites. Prefer deterministic payload assembly and targeted prompt blocks.

## Diagnostics requirements

Developer diagnostics should expose, for GLM follow-ups:

- raw follow-up text
- follow-up category
- supplied predictor values
- resolved predictor values
- completed predictor values
- deterministic status
- model type/family/link if available
- response scale
- prediction payload
- any warnings or needs-input reasons
- prompt excerpt

The JSON download should include these fields.

## Tests to add or update

Add deterministic offline tests for GLM follow-up prediction behaviour.

Suggested tests:

- binomial/logit prediction request with all required predictors supplied
- binomial/logit factor-level resolution
- binomial/logit missing predictor safe failure
- binomial/logit completed predictor path where deterministic defaults are valid
- count/log-link prediction request if supported
- diagnostics JSON includes GLM prediction payload
- prompt contains deterministic GLM follow-up block
- LLM-authored duplicate prediction sentences are removed or ignored
- LM follow-up tests remain unchanged and passing

Use existing fake providers, mocks, or deterministic helpers. Do not call real LLMs in tests.

## Testing expectations

Run the usual local validation through the stage script:

```r
devtools::document()
devtools::test(stop_on_failure = TRUE, stop_on_warning = TRUE)
devtools::check(args = c("--no-manual", "--ignore-vignettes"), error_on = "note")
```

## Deliverables

For each step in this stage, provide:

- a downloadable change zip
- a downloadable `run_stage25_x.sh` script
- a short change note
- a plain-text git commit message inside the script

The final stage script should build, install, archive, and create the usual ChatGPT bundle when the helper is available.
