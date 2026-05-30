# WMFM Stage 26 context: x-unit change follow-up explanations

## Branch

Suggested branch:

```text
x-unit-followup
```

## Step 1: audit and branch setup

Before changing files:

1. Start from updated `master` after Stage 25 is complete and merged, unless explicitly directed otherwise.
2. Run the usual audit:
   - confirm the working tree is clean
   - confirm the current branch and latest commit
   - inspect current follow-up classifier and parser tests
   - inspect explanation prompt/payload generation for numeric predictors
   - inspect post-processing rules for unit-change wording
3. Check out the new branch and push it:

```bash
git checkout master
git pull --ff-only origin master
git checkout -b x-unit-followup
git push -u origin x-unit-followup
```

Use the WMFM stage workflow, downloadable change zips, downloadable stage runners, and incremental stage numbering beginning at Stage 26.

Use the `wmfm-stage-script-generator` skill and the `r-development-style` skill.

Do not use Codex unless explicitly requested.

## Current baseline

Stage 23 stabilized prediction-style follow-up questions for linear models. Stage 25 is expected to extend deterministic follow-up support to GLMs before this stage begins.

Prediction-style follow-ups append a separate deterministic paragraph. An x-unit change request is different: it usually asks for the existing explanation to be reframed, not for an extra prediction answer.

Examples:

- "Can you explain the effect of test score in terms of a 5-point increase?"
- "What happens for a 10-unit increase in magnitude rather than a 1-unit increase?"
- "Can you phrase the slope as a change per 2 units of x?"

## Goal

Implement deterministic handling for x-unit change follow-up requests.

The key design issue is that these requests should generally rephrase the relevant numeric effect in the main explanation rather than append a new prediction paragraph.

The system should:

1. classify x-unit change requests separately from prediction requests
2. identify the numeric predictor and requested unit change
3. compute the deterministic transformed effect and interval
4. supply the transformed quantity to the LLM or final assembler
5. ensure the final explanation uses the requested unit-change wording
6. avoid adding a redundant separate paragraph unless the UI/design explicitly decides otherwise

## Important design distinction

Do not treat x-unit change requests as ordinary predictions.

Prediction request:

```text
If Test = 10 and Attend = Yes, what is my predicted Exam mark?
```

This should produce a separate deterministic follow-up paragraph.

X-unit change request:

```text
Can you explain the Test effect for a 5-point increase instead of a 1-point increase?
```

This should usually revise the relevant effect sentence, for example:

```text
A 5-point increase in the mid-term test score is associated with an average increase of about 17.5 points in the final exam mark.
```

For multiplicative GLM effects, this may require exponentiating the coefficient multiplied by the requested unit change.

## Model-specific behaviour

For linear models:

- multiply the slope and confidence interval by the requested unit change
- preserve response-scale interpretation
- avoid excessive precision

For binomial/logit models:

- odds multiplier for an x-unit increase should be computed deterministically when appropriate
- probability changes may depend on baseline values and should not be invented unless deterministic baseline values are supplied

For count/log-link models:

- expected-count multiplier for an x-unit increase should be computed as the one-unit multiplier raised to the x-unit change, or equivalently by exponentiating the scaled coefficient
- confidence intervals must be transformed on the same scale

## Parser requirements

The parser should be conservative.

It should detect:

- requested numeric unit size
- target predictor name or label
- whether the request is about rephrasing an effect rather than predicting an outcome

It should return `needs_input` or `clarification_required` if:

- the predictor is ambiguous
- multiple numeric predictors could match
- the requested unit size is missing or not numeric
- the model type cannot support the requested deterministic transformation safely

## Prompt and assembly requirements

Avoid contradictory prompt instructions.

The prompt/payload should include an explicit block such as:

```text
Student follow-up request:
...

Deterministic requested unit-change interpretation:
...

Instruction:
Use this deterministic unit-change interpretation when explaining the relevant effect.
Do not recompute it.
Do not also describe the same effect using the original one-unit wording unless needed for clarity.
```

The final explanation should not become verbose. It should replace or revise the relevant sentence where possible.

## Diagnostics requirements

Developer diagnostics should expose:

- raw follow-up text
- follow-up category, e.g. `unit_change_request`
- requested predictor
- requested unit change
- original one-unit effect if available
- transformed x-unit effect
- transformed confidence interval
- model type and response/effect scale
- final prompt excerpt

The timestamped diagnostics JSON download should include these fields.

## Tests to add or update

Add deterministic offline tests for:

- classifier detects x-unit requests separately from prediction requests
- parser extracts requested unit size and predictor
- ambiguous predictor request returns safe clarification
- LM slope and CI are scaled correctly
- GLM multiplicative effect and CI are scaled correctly where supported
- final explanation uses x-unit wording
- final explanation does not append an unnecessary prediction-style paragraph
- diagnostics JSON includes unit-change payload
- existing prediction follow-up tests remain unchanged and passing

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
- a downloadable `run_stage26_x.sh` script
- a short change note
- a plain-text git commit message inside the script

The final stage script should build, install, archive, and create the usual ChatGPT bundle when the helper is available.
