# WMFM Stage 37 clean restart context

## Purpose of this context

This context is for restarting WMFM Stage 37 cleanly.

The previous Stage 37 attempt on `adjustment-variables-ii` entered a mixed-history state after trying to restore provider/developer-mode UI files while also continuing adjustment-aware scoring work. That attempt was abandoned. Do not reuse the Stage 37.5 repair zips from that abandoned branch state.

The current baseline has been restored and verified with the expected full test count:

```text
[ FAIL 0 | WARN 0 | SKIP 0 | PASS 3631 ]
```

This is the healthy baseline for the current codebase.

## Repository and branch

Repository:

```text
jmcurran/WMFM
```

Current branch for this restarted work:

```text
adjustment-variables-ii
```

This branch should be treated as a fresh branch from the current clean `master` codebase.

The previous adjustment-variable stage is assumed to be complete and already present in the current codebase. Do not try to reapply old adjustment-variable implementation work from abandoned Stage 37 artifacts.

## Stage numbering and versions

This workstream is Stage 37.

Use incremental substages:

```text
37.1
37.2
37.3
...
```

If a repair is needed after a failed substage, use repair subnumbers:

```text
37.5.1
37.5.2
37.5.3
```

Do not use ambiguous names like `stage37_5_repair`.

Package version numbers for this Stage 37 workstream should use:

```text
0.3.1.xxx
```

where `xxx` starts at `001` and increments on every build attempt, regardless of whether validation succeeds.

Important version-display note:

- `DESCRIPTION` may contain a zero-padded build component such as `0.3.1.005`.
- R's `packageVersion()` may display this as `0.3.1.5`.
- If the UI must preserve the padded string, use the raw `Version:` field from `DESCRIPTION` or `utils::packageDescription("WMFM")$Version`, not `packageVersion()`.

## Required skills and conventions

Use the WMFM stage-script workflow and the R development style conventions.

Important R style rules:

- Use `=` for assignment.
- Use camelCase identifiers.
- Use braces on all control structures, even single-line bodies.
- Use roxygen2 documentation for package functions.
- Prefer `@importFrom` over `pkg::fun()` in package code unless there is a strong reason.
- Keep tests deterministic and offline.
- Do not call real LLM providers in tests.
- Keep app orchestration thin; avoid putting business logic directly in `app-server.R`.

Stage runner expectations:

- Generate a change zip and a runner script for each substage.
- Runner names should follow the substage exactly, for example:

```text
stage37_1_changes.zip
run_stage37_1.sh
stage37_1_completed.zip
```

- For repair substages:

```text
stage37_5_1_changes.zip
run_stage37_5_1.sh
stage37_5_1_completed.zip
```

- The runner must bump the final numeric version component before validation.
- The runner must run:

```r
devtools::document()
devtools::test(stop_on_failure = TRUE, stop_on_warning = TRUE)
devtools::check(args = c("--no-manual", "--ignore-vignettes", "--no-tests"), error_on = "note")
```

- The runner should run `scripts/checkRStringEscapes.R` when present.
- `scripts/checkRStringEscapes.R` should be preserved.
- ChatGPT bundle creation must be off by default. If supported at all, it should require an explicit switch and write to `chatgpt-bundles/`, not the project root.

## Lessons from the abandoned attempt

The abandoned Stage 37.5 attempt showed why this restart is needed.

Problems encountered:

- The branch appeared to have been based on the wrong source state.
- Provider/developer-mode UI files became mixed across different historical stages.
- Several source files and tests appeared as untracked restorations.
- Test count dropped to about 2612, while the healthy baseline is 3631.
- Multiple repair zips gradually restored files, but this created a risky mixed-history working tree.

Do not continue from that state.

Before committing future Stage 37 work, confirm:

```bash
git status --short
R -q -e "devtools::test(stop_on_failure = TRUE, stop_on_warning = TRUE)"
```

The expected full-suite count should remain close to:

```text
PASS 3631
```

Small changes in test count are possible if tests are deliberately added or removed, but a large drop must be treated as a release-blocking warning.

## Current Stage 37 goal

Stage 37 is about adjustment-aware explanation governance and scoring alignment.

The previous adjustment-variable implementation is assumed to be complete. The system now needs to align explanation and scoring behavior with the new adjustment-aware interpretation policy.

The central conceptual shift is:

```text
Old scoring assumption: more interpretation is always better.
New adjustment-aware assumption: only scientifically justified interpretation should appear.
```

The app should reward explanations that are restrained, research-question-focused, and correctly adjusted for background variables.

## Adjustment-aware interpretation policy

Allowed:

- Mentioning adjustment variables as adjusted-for context.
- Saying the model accounts for adjustment variables.
- High-level acknowledgement of interaction structure when needed.
- Restrained conclusions.
- Uncertainty-aware language.
- Focus on the research question.

Forbidden or strongly discouraged:

- Narrating adjustment-variable coefficients.
- Narrating adjustment-variable contrasts.
- Narrating adjustment-variable confidence intervals.
- Narrating adjustment-variable fitted means.
- Narrating adjustment-level predicted values.
- Turning adjustment variables into narrative conditioning axes.
- Interpreting picture-level or other adjustment-level cells when those variables are only controls.
- Explaining the regression table coefficient-by-coefficient instead of answering the research question.

Interaction policy:

```text
Adjustment variables may participate in high-level interaction summaries, but may not become narrative conditioning axes with level-specific estimates.
```

For example, if `picture` is an adjustment variable, avoid statements such as:

```text
For infant pictures...
For nude-female pictures...
At this picture level...
```

unless there is a deliberate policy decision to expose those comparisons.

## Motivating example

Use the `s20x::arousal.df` workflow as the key manual example.

Model:

```r
arousal ~ gender + picture + gender:picture
```

Adjustment variable:

```text
picture
```

Research question:

```text
Is there a difference in arousal levels for females and males?
```

A healthy restrained explanation should keep `picture` in the background, for example:

```text
Researchers asked whether arousal levels differ between females and males. In the analysis, arousal was measured while participants viewed a variety of pictures, and the statistical model was built to account for any differences that might arise from the particular pictures shown. After adjusting for these picture effects, the results do not indicate a clear difference in arousal between genders, suggesting that gender appears not to influence arousal in this setting.
```

This style is intentional. It should not be penalized simply because it avoids picture-level estimates or interaction-cell narration.

## Likely Stage 37 substages

### Stage 37.1: confirm baseline and add adjustment-aware scoring context

Goal:

- Ensure run records and scoring prompts can carry adjustment-variable metadata and primary-variable metadata.
- Preserve existing non-adjustment behavior.
- Avoid broad UI restoration work.

Possible changes:

- Ensure `buildWmfmRunRecord()` supports:
  - `adjustmentVariables`
  - `primaryVariables`
- Ensure `runExample()` forwards adjustment metadata from example specs into run records.
- Ensure roxygen docs document any new arguments.

Tests:

- Run-record tests for adjustment metadata.
- Example tests showing adjustment metadata reaches scoring context.

### Stage 37.2: adjustment-aware LLM scoring prompt policy

Goal:

- Teach the LLM grader not to penalize correct restraint.
- Keep behavior unchanged for non-adjustment workflows.

Prompt policy should include ideas such as:

- Reward correct adjusted-for framing.
- Do not penalize omission of adjustment-level details.
- Do not require picture-specific or other adjustment-level narratives.
- Do not require interaction cell-by-cell narration when interaction structure involves an adjustment variable.
- Reward avoiding forbidden adjustment-level narratives while still answering the research question.
- Do not reward coefficient-table recitation.

Tests should check policy invariants, not brittle full prompt snapshots.

### Stage 37.3: deterministic scoring alignment

Goal:

- Review deterministic metrics that may still penalize restrained adjustment-aware explanations.
- Distinguish "missing important interpretation" from "correctly suppressed adjustment-level interpretation."

Possible targets:

- Completeness scoring.
- Interaction coverage scoring.
- Numeric expression scoring.
- Comparison structure scoring.

Do not weaken scoring generally. Only adjust behavior where adjustment-aware restraint is scientifically correct.

### Stage 37.4: explanation governance cleanup

Goal:

- Centralize adjustment-aware interpretation policy where possible.
- Reduce contradictory prompt/test logic.
- Make allowed and forbidden narratives explicit.

Focus on policy invariants rather than exact prose.

### Stage 37.5: manual validation and UI smoke test

Goal:

- Manually retest the arousal example.
- Confirm provider/developer-mode UI still behaves exactly as expected in the current clean codebase.

Critical UI expectations:

- If `WMFM_SHOW_DEVELOPER_MODE=1` is in `.Renviron`, developer mode should expose the current intended developer controls.
- Do not regress the AI provider settings UI.
- Claude should remain selectable and respect the current provider configuration behavior.
- If the app stores provider preferences, do not override them unexpectedly.
- Preserve current tests and source files. A sudden drop from the expected test count is a release blocker.

## Important provider/developer-mode caution

The abandoned repair attempt caused confusion around provider/developer-mode UI. Do not assume old screenshots or old branch files are authoritative.

Use the current clean codebase as the source of truth.

If behavior appears wrong in the running app:

1. Check the current tests first.
2. Inspect the current source files.
3. Make the smallest targeted fix.
4. Avoid replacing entire app UI/server files from older bundles.

## Branch safety checks

Before beginning a new substage:

```bash
git status --short
git branch --show-current
git log --oneline --decorate -5
```

Expected branch:

```text
adjustment-variables-ii
```

Before committing a completed substage:

```bash
git status --short
find R -type f | sort | wc -l
find tests/testthat -type f | sort | wc -l
R -q -e "devtools::test(stop_on_failure = TRUE, stop_on_warning = TRUE)"
R -q -e "devtools::check(args = c('--no-manual', '--ignore-vignettes', '--no-tests'), error_on = 'note')"
```

If test count drops dramatically, stop and investigate.

Check for deleted tracked files:

```bash
git diff --name-status | grep '^D'
git status --short | grep '^ D'
```

## What not to reuse

Do not reuse these abandoned artifacts as a basis for future work:

- `stage37_5_repair_changes.zip`
- `stage37_5_1_changes.zip`
- `stage37_5_2_changes.zip`
- `stage37_5_3_changes.zip`
- `stage37_5_4_changes.zip`
- `stage37_5_5_changes.zip`
- `stage37_5_6_changes.zip`
- `stage37_5_7_changes.zip`
- `stage37_5_8_changes.zip`

They came from the mixed-history recovery attempt and should be treated as abandoned.

If any ideas from them are still needed, reimplement them carefully against the clean current codebase.

## Preferred immediate next step

Start fresh with Stage 37.1 or 37.2 depending on what is already present in the current clean codebase.

First inspect whether the current codebase already contains:

- adjustment metadata in run records,
- scoring prompt adjustment policy,
- `runExample()` forwarding of adjustment and primary variables,
- tests for adjustment-aware scoring prompt behavior.

Then make the smallest missing change.

Do not assume the abandoned Stage 37 branch changes are present unless confirmed in the clean current codebase.
