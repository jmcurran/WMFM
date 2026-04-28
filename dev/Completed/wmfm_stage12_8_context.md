# WMFM Stage 12.8 context

## Project

I am working on WMFM, an R package/app that helps students fit statistical models and receive plain-language model explanations.

Please use my usual WMFM conventions:

- Use `=` for assignment in R.
- Use camelCase identifiers.
- Use braces for all control structures.
- Use roxygen2 documentation where relevant.
- Prefer targeted, minimal changes.
- Preserve existing tests unless there is a strong reason to change them.
- Provide changed files as a downloadable zip.
- Provide a `run_stage12_8.sh` script using my `wmfm-stage-script-generator` workflow.
- Use my `r-development-style` and `wmfm-stage-script-generator` skills.

## Current state

Stage 12.7 is complete.

The recent work fixed the logistic two-factor interaction prompt payload. The prompt now includes:

- probability rows for each group combination
- conditional odds-ratio rows
- an interaction contrast as a ratio of odds ratios with a confidence interval
- tests ensuring those quantities appear in the prompt

The final 12.7 tests and `devtools::check()` are passing.

There was a post-12.7 issue around research-question guidance wording. The same or similar wording existed in both `prompt-core.R` and `prompt-explain.R`, and the tests were sensitive to exact strings. This exposed a duplication/maintenance risk.

I have already fixed the minor inline `""` formatting issue that appeared earlier.

## Stage 12.8 goal

Start Stage 12.8 by refactoring duplicated research-question guidance so the same prompt contract text is defined in one place and reused wherever it is needed.

The first step should be:

1. Locate all repeated research-question guidance strings, especially the exact contract sentence:

   `Start with a short opening paragraph that briefly restates the research question in clear, natural language.`

2. Create a shared helper for the research-question guidance lines, for example something like:

   `getResearchQuestionGuidanceLines()`

   The exact function name can be chosen to fit the package style.

3. Update prompt builders so they call the shared helper instead of duplicating strings.

4. Preserve the existing prompt contract wording exactly, so existing tests keep passing.

5. Add or update tests to ensure both prompt paths use the shared research-question guidance.

6. Keep the change narrow. Do not redesign the prompt system in this first step.

## Follow-up checks for Stage 12.8

After the first refactor, continue with targeted checks only if appropriate:

- Confirm no duplicated research-question guidance remains.
- Confirm `makeChatgptBundle.R` deletes its temporary directory after creating the zip.
- Confirm `mkBundle.sh` uses `HEAD` as the default base for ordinary stage bundles.
- Confirm future stage scripts use broad enough `git add` coverage so new `R/`, `man/`, `tests/`, `scripts/`, `NAMESPACE`, and `DESCRIPTION` changes are not left untracked.
- Audit prompt payload coverage for related model types:
  - logistic one-factor
  - logistic numeric-by-factor
  - Poisson two-factor interaction
  - Poisson numeric-by-factor

## Branching requirement

Do this work on a new branch, not directly on `master`.

Suggested commands before starting the new chat:

```bash
git status
git checkout master
git pull --ff-only
git checkout -b stage12_8_prompt-guidance-refactor
```

If I am already on the correct current branch instead of `master`, use the equivalent branch creation command from that branch:

```bash
git checkout -b stage12_8_prompt-guidance-refactor
```

## Codebase archive for the new chat

After creating the branch, make a clean archive of the current codebase to upload to the new chat.

Recommended command:

```bash
git archive --format=zip --output=wmfm_stage12_8_start.zip HEAD
```

Then upload:

- `wmfm_stage12_8_start.zip`
- this context file

## Expected deliverables from the new chat

For the first Stage 12.8 step, generate:

- `stage12_8_changes.zip`
- `run_stage12_8.sh`

The stage script should:

- optionally install `stage12_8_changes.zip` with `--install-files`
- run `devtools::document()`
- run strict tests
- run strict check
- bump DESCRIPTION only after checks pass
- commit all relevant package files using broad `git add` coverage
- create `stage12_8_completed.zip`
- build and install the exact built tarball
- optionally create a ChatGPT bundle after success, using the bundle helper as a child process rather than sourcing it
