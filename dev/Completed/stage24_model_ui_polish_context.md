# WMFM Stage 24 context: model UI polish

## Branch

Suggested branch:

```text
model-ui-polish
```

## Step 1: audit and branch setup

Before changing files:

1. Start from updated `master` after `follow-questions` has been merged.
2. Run the usual audit:
   - confirm the working tree is clean
   - confirm the current branch and latest commit
   - run or inspect the relevant current tests if practical
   - identify the UI files involved before editing
3. Check out the new branch and push it:

```bash
git checkout master
git pull --ff-only origin master
git checkout -b model-ui-polish
git push -u origin model-ui-polish
```

Use the WMFM stage workflow, downloadable change zips, downloadable stage runners, and incremental stage numbering beginning at Stage 24.

Use the `wmfm-stage-script-generator` skill and the `r-development-style` skill.

Do not use Codex unless explicitly requested.

## Current baseline

The `follow-questions` branch has just completed the Stage 23 follow-up stabilization work.

The current follow-up behaviour is broadly working:

- deterministic linear-model follow-up prediction works
- follow-up prediction payloads resolve supplied factor and numeric predictors correctly
- missing/defaulted predictor handling is conservative
- follow-up answers appear in a separate paragraph
- diagnostics JSON export works with timestamped filenames
- anchored factor-comparison narration has been restored in main explanations
- compact confidence interval formatting is being post-processed

The current explanation output is usable enough to move to interface polish.

## Goal

Polish the UI layout to accommodate the extra follow-up-question workflow and developer diagnostics without making the model tab feel cramped or confusing.

This stage is about UI layout, spacing, visibility, and usability. It is not about changing the deterministic follow-up parser, prediction engine, or prompt semantics unless a small wiring issue is discovered while polishing the UI.

## Likely areas to inspect

Likely files include, but are not limited to:

- app UI files for the model/explanation tab
- follow-up question input controls
- explanation output panel
- developer diagnostics panel
- helper text around follow-up questions
- CSS or UI helper modules used by the app

## Desired changes

Consider the following UI improvements:

- Give the follow-up question input enough width and vertical space.
- Make it clear that the follow-up question is optional.
- Make it clear when the follow-up answer will be appended to the main explanation.
- Keep the main explanation readable when a follow-up answer is present.
- Improve spacing between:
  - research question controls
  - follow-up question controls
  - generated explanation
  - diagnostics controls
- Make developer diagnostics easier to inspect without overwhelming student-mode UI.
- Ensure diagnostics JSON download remains visible and clearly labelled in developer mode.
- Keep student mode clean.

## Constraints

Preserve all Stage 23 functionality:

- Do not weaken deterministic follow-up prediction handling.
- Do not change LM prediction values or intervals.
- Do not collapse follow-up paragraph boundaries.
- Do not remove timestamped diagnostics JSON downloads.
- Do not alter cache-version behaviour.
- Do not remove anchored factor-comparison narration.

Follow James's R package style:

- use `=` for assignment
- use camelCase identifiers
- use braces for all control structures
- use roxygen2 documentation where needed
- prefer `@importFrom` over `pkg::fun()` in package code
- keep tests deterministic and offline

## Testing expectations

Add or update tests for UI structure where appropriate:

- follow-up question control is present
- explanation output still renders
- developer diagnostics controls remain available in developer mode
- diagnostics JSON download control remains present
- student-mode UI does not expose developer diagnostics

Run the usual local validation through the stage script:

```r
devtools::document()
devtools::test(stop_on_failure = TRUE, stop_on_warning = TRUE)
devtools::check(args = c("--no-manual", "--ignore-vignettes"), error_on = "note")
```

## Deliverables

For each step in this stage, provide:

- a downloadable change zip
- a downloadable `run_stage24_x.sh` script
- a short change note
- a plain-text git commit message inside the script

The final stage script should build, install, archive, and create the usual ChatGPT bundle when the helper is available.
