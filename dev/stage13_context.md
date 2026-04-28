# WMFM Stage 13 context

## Project

I am working on WMFM, an R package/app that helps students fit statistical models and receive plain-language model explanations.

Please use my usual WMFM conventions:

- Use `=` for assignment in R.
- Use camelCase identifiers.
- Use braces for all control structures.
- Use roxygen2 documentation where relevant.
- Prefer targeted, minimal changes.
- Preserve existing tests unless there is a strong reason to change them.
- Provide changed files as a downloadable zip when files are modified.
- Provide a `run_stage13_N.sh` script using my `wmfm-stage-script-generator` workflow for each implementation step.
- Use my `r-development-style` and `wmfm-stage-script-generator` skills.

## Current state

Stage 12 has been a long prompt-guidance and GLM explanation refinement stream.

By the end of Stage 12.8, the branch `stage12_8_prompt-guidance-refactor` should be merged back into `master`, tagged, and followed by a new branch for Stage 13.

Suggested Stage 13 branch name:

```bash
git checkout -b stage13_example-suite-hardening
```

## What Stage 12 completed

Stage 12 addressed a series of prompt, payload, explanation, and validation issues, including:

- shared research-question guidance, replacing duplicated wording in prompt builders
- preservation of exact research-question prompt contract wording
- logistic two-factor interaction prompt payloads
- probability rows for logistic group combinations
- conditional odds-ratio rows
- interaction contrasts as ratios of odds ratios with confidence intervals
- cautious handling of additive logistic comparisons where the odds-ratio confidence interval includes 1
- stronger prompt guidance so secondary, unclear odds-ratio comparisons are either omitted or described cautiously
- deterministic validation refinement so odds-ratio confidence interval values are not incorrectly treated as raw decimal odds
- confirmation that development-only workflow scripts should not be required by package tests
- line-ending normalization for `.Rproj` files via `.gitattributes`

The most recent developer-feedback examples suggested that the additive logistic issue is now acceptable: unclear attendance differences are described cautiously rather than as established effects.

## Stage 13 goal

Stage 13 is about moving from reactive prompt repair to systematic example-suite hardening.

The key aim is to run the full example set, identify any remaining explanation or validation failures, and turn recurring failures into stable regression coverage or prompt-payload checks.

This should be done carefully and incrementally. Avoid redesigning the whole prompt system unless the example sweep clearly shows a repeated structural problem.

## Stage 13.1: former Stage 12.9 close-out sweep

Treat the old proposed Stage 12.9 as Stage 13.1.

Goal:

- Run the full example set.
- Review the resulting developer-feedback JSON files.
- Decide whether each problem is:
  - a prompt guidance issue
  - a missing deterministic prompt-payload quantity
  - a sentence tagging or validation issue
  - a genuinely acceptable explanation that should not be changed
  - a future-stage design issue

Coverage to confirm:

- logistic anchors
- logistic one-factor comparisons
- logistic additive numeric + factor models
- logistic numeric-by-factor interactions
- logistic two-factor interactions, if available
- Poisson numeric-by-factor interactions
- intercept-only wording and tagging
- cautious interpretation where confidence intervals include the relevant null value
- typical-case fitted values and confidence intervals on the response scale
- final answer wording that answers the research question without overclaiming

Expected Stage 13.1 deliverables if code changes are needed:

- `stage13_1_changes.zip`
- `run_stage13_1.sh`

If no code changes are needed, produce a short close-out summary and a recommended next Stage 13.2.

## Stage 13.2 and later: example-suite hardening

Likely follow-up stages may include:

1. Build a clearer fixture strategy for example feedback JSON files.
2. Add tests that check prompt payload content without depending on real LLM responses.
3. Add tests for recurring sentence-tagging and validation issues found in the example sweep.
4. Separate package behavior tests from development workflow scripts and manual workflow helpers.
5. Create a compact example-regression checklist so future stages can quickly tell whether a change affects explanation quality.

Do not add all of this at once. Choose the next small, testable step based on the Stage 13.1 sweep.

## Important boundary

Stage 13 should not become another broad prompt rewrite. Prefer this order:

1. Inspect actual example output.
2. Identify a repeated or high-value issue.
3. Make the smallest deterministic or prompt-guidance change that addresses it.
4. Add focused tests.
5. Re-run examples.

## Workflow expectations

For each implementation step:

- Work from the uploaded current codebase or ChatGPT bundle.
- Use the current Stage 13 branch, not `master`.
- Generate replacement files only for files that changed.
- Bundle changed files in a zip.
- Generate the matching stage script.
- Include a plain-text git commit message in the script.
- Keep tests offline and deterministic.
- Do not rely on real LLM calls in tests.
- Avoid adding development-only scripts as required package test fixtures.

## Suggested opening message for the new chat

Use this in the new chat:

```text
I want to start WMFM Stage 13 using the attached context and current codebase. Please treat the former Stage 12.9 close-out sweep as Stage 13.1. I have checked out a new branch called stage13_example-suite-hardening. Please use my r-development-style and wmfm-stage-script-generator skills.
```

Upload:

- this context file
- a fresh codebase archive or ChatGPT bundle from the Stage 13 branch
