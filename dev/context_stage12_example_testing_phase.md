# WMFM example testing phase after Stages 9, 10, and developer-mode protection

## Context

I am working on an R package/app called WMFM. It helps students fit statistical models and receive plain-language explanations of fitted model results.

A large amount of explanation-pipeline work has just been completed.

The current branch/state should include:

- Stage 9 infrastructure work
- Stage 10 prompt and explanation integration work
- Stage 11.1 developer-mode password protection work

The next task is to rerun and review my 19 example models/explanations.

## Recently completed Stage 9 work

Stage 9 focused on deterministic infrastructure rather than directly changing student-facing explanations.

It added or refined:

- explanation model profiles
- deterministic formatting helpers
- explanation anchor helpers
- model-aware explanation rule profiles
- sentence schema metadata
- deterministic validation and quality flags
- developer-feedback and developer-mode UI surfacing

Important Stage 9 design decisions:

- Multi-role tagging is required.
- Legacy `claimTags` must remain backward-compatible.
- Expanded Stage 9 roles belong in `roles`, not by changing the legacy meaning of `claimTags`.
- Student-facing explanations should remain clean.
- Stage 9 metadata is primarily for developer support and future prompt control.
- Deterministic layers should compute, format, and validate whenever possible.
- The LLM should describe, not decide.

## Recently completed Stage 10 work

Stage 10 connected the Stage 9 deterministic infrastructure to the explanation-generation path.

Completed Stage 10 steps:

### Stage 10.1 - Audit current prompt inputs

- Audited current prompt construction.
- Identified where raw or partially processed model quantities entered the explanation prompt.
- Mapped likely injection points for Stage 9 formatted quantities and metadata.

### Stage 10.2 - Feed formatted quantities into prompts

- Added formatted Stage 9 prompt quantities to the prompt path.
- Kept raw values available for audit/developer metadata.
- Preserved backward-compatible audit flags such as coefficient-table and confidence-interval inclusion.

### Stage 10.3 - Use model-aware explanation skeletons

- Added deterministic skeleton guidance to prompts.
- Skeleton selection is based on model profile and rule-profile metadata.
- The LLM receives ordered explanation structure but still writes student-facing prose naturally.

### Stage 10.4 - Improve response-scale control

- Added response-scale control guidance based on model profile metadata.
- Reduced log-scale, log-odds, log-count, and coefficient-scale leakage.
- Added tests around logistic/binomial response-scale guidance.
- Used `STATS20x.txt` / course-style data for a non-separated logistic example when needed.

### Stage 10.5 - Improve comparison control

- Added prompt guidance to avoid exhaustive treatment/group comparison lists unless required.
- Encouraged global summaries or targeted comparisons where appropriate.
- Goal: prevent comparison explosion in multi-factor or treatment-heavy models.

### Stage 10.6 - Improve interaction explanations

- Added explicit within-group-then-compare guidance for interactions.
- The intended pattern is:
  - describe the effect within group/value A
  - describe the effect within group/value B
  - compare those effects directly
  - answer the research question
- Avoid coefficient decomposition and avoid using the phrase `interaction term` in student-facing explanations.

### Stage 10.7 - Use validation feedback to guard prompts

- Added validation/prompt-guard feedback to identify likely prompt failures.
- Initial guard targets include:
  - technical scale leakage
  - raw coefficients
  - odds displayed as ordinary decimals
  - vague effect wording for numeric predictors
  - missing answer
  - missing uncertainty where required
  - excessive comparisons
  - interaction not compared
- The first implementation should flag and surface issues, not automatically regenerate explanations.

## Recently completed Stage 11.1 work

A small UI/security change was added before rerunning examples.

Goal:

- Password-protect the settings-tab developer-mode checkbox.
- Prevent students or ordinary users from accessing developer diagnostics and example/debug controls without a password.

Expected design:

- The active developer-mode control is on the Settings tab.
- Attempting to enable developer mode should require a password.
- If no password hash is configured, developer mode should remain locked.
- If the password is wrong, developer mode should remain locked.
- If the password is correct, developer mode can be unlocked.
- There should be an obvious way to lock developer mode again.

Password configuration:

```r
hash = WMFM::makeDeveloperModePasswordHash("your-password-here")
cat(hash)
```

Then add the hash to `.Renviron`:

```text
WMFM_DEVELOPER_MODE_PASSWORD_HASH=the_hash_you_generated
```

Restart R/RStudio after editing `.Renviron`.

## Standing coding and workflow conventions

Use my WMFM/R conventions:

- use `=` for assignment
- use camelCase identifiers
- always use braces for control structures
- use styleR-style formatting
- prefer `|>` over magrittr
- use roxygen2 documentation
- prefer `@importFrom` rather than `pkg::fun()` where appropriate
- do not manually edit `NAMESPACE`
- keep tests deterministic and offline
- do not make real LLM calls in tests
- use fake providers, existing prompt helpers, mocks, or fixtures for tests
- provide replacement files or zips rather than long copy-paste patches
- provide plain-text ASCII git commit messages
- if a generated stage script is needed, number it according to the current stage/substage
- if a stage fails and we patch it, keep incrementing the substage number and accumulate commit-message notes

## Current task

The next phase is example testing.

I want to rerun my 19 example models/explanations and inspect whether the Stage 9 and Stage 10 changes improved explanation behaviour without breaking existing strong behaviour.

Before reviewing examples, confirm the package is healthy on the current branch/state:

```r
options(warn = 2)
devtools::test(stop_on_failure = TRUE, stop_on_warning = TRUE)
devtools::check(args = c("--no-manual", "--ignore-vignettes"), error_on = "note")
```

Then run the 19 examples using my usual example workflow.

## What to look for in the 19 examples

Review each generated explanation for these issues.

### Positive behaviours expected after Stage 10

The examples should show:

- cleaner prompt-driven explanations
- fewer raw coefficients
- fewer log-scale, log-odds, or log-count phrases
- fitted values using formatted quantities where available
- uncertainty kept close to the estimate it belongs to
- fewer excessive treatment/group comparisons
- interactions explained using within-group-then-compare structure
- developer diagnostics present only in developer-mode/debug surfaces
- no developer metadata leaking into student-facing explanation text
- clear answer sentences when a research question is provided

### Problems to flag

Flag any explanation that includes:

- raw coefficient-scale interpretation
- log-odds leakage
- log-count leakage
- unexplained transformed response scale
- `interaction term` language in the student explanation
- coefficient-by-coefficient decomposition
- exhaustive treatment comparisons that are not needed
- confidence intervals separated from the estimate they describe
- odds written as ordinary decimals where odds formatting should be used
- probabilities and odds mixed confusingly
- missing uncertainty when uncertainty is available and relevant
- missing final answer to the research question
- overconfident language such as `proves`, `confirms`, or `demonstrates`
- student-facing references to internal metadata, validation flags, skeleton ids, claim tags, or developer diagnostics

## Suggested review table

For each of the 19 examples, record:

```text
Example number:
Model/formula:
Research question, if any:
Outcome type/scale:
Main explanation quality:
Scale handling:
Formatted quantities used well? yes/no/partial
Uncertainty handled well? yes/no/partial
Comparison control okay? yes/no/not applicable
Interaction handling okay? yes/no/not applicable
Developer metadata leaked? yes/no
Overall verdict: pass / minor issue / needs fix
Notes:
```

## Recommended workflow for this chat

1. Confirm the current codebase and test/check state.
2. Run or inspect the first few examples.
3. Categorise failures by type rather than fixing each example independently.
4. Only make code changes once we see a repeated pattern.
5. Keep fixes narrow and reversible.
6. If fixes are needed, create the next numbered stage/substage with a change zip and stage script.

## Non-goals

Do not redesign:

- `explanationAudit`
- scoring or grading
- bad explanation generation
- claim-evidence internals beyond what is needed to fix example failures
- the whole prompt architecture

Do not remove legacy fields or break developer-feedback objects.

Do not change the student-facing UI layout unless a specific example review shows that it is necessary.

## First thing to do in the new chat

Start by asking me to provide either:

- the 19 example outputs, or
- the script/workflow I use to run the 19 examples, or
- the current codebase if it is not already attached.

Then help me review the examples systematically.
