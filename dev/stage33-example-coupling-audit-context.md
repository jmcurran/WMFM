# WMFM Stage 33 context: audit example-specific coupling

## Purpose

This stage is an audit and cleanup stage for the WMFM codebase.

The concern is that production code and prompts should not be engineered to give better answers for specific built-in examples. Built-in examples are useful for tests, documentation, demonstrations, and regression checks, but production logic should be driven by model structure, variable metadata, deterministic payloads, and general interpretation rules.

This stage should begin as an audit-only stage. Do not refactor until the audit has produced a readable list of specific items to inspect.

## Branch and workflow

Create a new branch for this stage, for example:

```bash
git status --short
git checkout master
git pull --ff-only origin master
git checkout -b example-coupling-audit
git push -u origin example-coupling-audit
git status --short
```

Use the usual WMFM workflow:

- Use `r-development-style`.
- Use `wmfm-stage-script-generator` for any delivered patch scripts.
- Stage number starts at 33.1.
- Version should continue from the current master version using the usual build-number policy.
- Each build attempt increments the build number regardless of success.
- Record completed stage notes in `NEWS.md`.
- Prefer small, reviewable changes.
- Do not use Codex unless explicitly requested.

## Core rule for this stage

Tests may be example-specific.

Production code and prompts should be model-structure-specific, metadata-specific, or payload-specific.

Acceptable places for example-specific names include:

- `tests/`
- example data definitions
- documentation
- `NEWS.md`
- example labels shown in the app
- snapshots or fixtures whose purpose is explicitly regression testing

Suspicious places for example-specific names include:

- `R/` production logic
- prompt construction code
- post-processing code
- explanation rules
- deterministic interpretation helpers
- app server logic that branches on example names or example variable names

The audit should not automatically treat every occurrence as wrong. It should classify each occurrence by context.

## First task: run the example-coupling audit command

Run this from the repository root and save the output:

```bash
{
  echo "# WMFM Stage 33 example-coupling audit"
  echo
  echo "Generated at: $(date)"
  echo "Branch: $(git branch --show-current)"
  echo

  echo "## Git status"
  echo
  echo '```text'
  git status --short
  echo '```'
  echo

  echo "## Suspicious example terms in production-ish files"
  echo
  echo '```text'
  grep -RniE "Diamonds|diamonds|carat|price|cut|color|clarity|earthquake|quake|magnitude|Locn|Freq|oyster|Course|Pass|Fail" \
    R inst DESCRIPTION \
    --exclude-dir=.git \
    --exclude="*.Rd" \
    --exclude="NEWS.md" 2>/dev/null || true
  echo '```'
  echo

  echo "## Same terms in tests, examples, docs for comparison"
  echo
  echo '```text'
  grep -RniE "Diamonds|diamonds|carat|price|cut|color|clarity|earthquake|quake|magnitude|Locn|Freq|oyster|Course|Pass|Fail" \
    tests examples man vignettes NEWS.md \
    --exclude-dir=.git 2>/dev/null || true
  echo '```'
  echo

  echo "## Candidate production files to inspect first"
  echo
  echo '```text'
  grep -RliE "Diamonds|diamonds|carat|price|cut|color|clarity|earthquake|quake|magnitude|Locn|Freq|oyster|Course|Pass|Fail" \
    R inst DESCRIPTION \
    --exclude-dir=.git \
    --exclude="*.Rd" \
    --exclude="NEWS.md" 2>/dev/null | sort || true
  echo '```'
} > stage33_example_coupling_audit.md

cat stage33_example_coupling_audit.md
```

Upload or paste `stage33_example_coupling_audit.md` into the chat before any code changes are made.

## Required audit output

After reviewing the command output, produce a readable markdown document named:

```text
stage33_example_coupling_review.md
```

The document should contain:

1. A short summary of the audit result.
2. A table of production-code hits, classified as:
   - `OK`
   - `Review`
   - `Likely problem`
3. A table of test/docs/example hits, classified separately.
4. A recommended order of inspection.
5. A recommendation about whether Stage 33 should proceed to code changes.

Suggested table columns:

```text
Priority | File | Line or area | Term(s) | Classification | Reason | Recommended action
```

## Classification guidance

### OK

Examples:

- Built-in example labels.
- Example datasets or example metadata.
- Tests that use Diamonds, earthquakes, courses, or other teaching examples as fixtures.
- Documentation that describes example workflows.
- NEWS entries that mention completed example work.

### Review

Examples:

- Prompt text mentioning a real example variable when it may only be illustrative.
- Test helpers that may have leaked into production helper files.
- General rules that include example-specific nouns in otherwise reusable logic.

### Likely problem

Examples:

- Production code checking for `carat`, `price`, `diamonds`, `cut`, `color`, or `clarity`.
- Production code rewriting text specifically about `log(carat)` or `log(price)`.
- Prompt rules that refer to Diamonds IV or another built-in example as the desired target.
- App logic that gives special handling to a named example outside the example-loading layer.

## Recommended inspection order

Start with the highest-risk production areas:

1. `R/model-explanation-cleanText.R`
   - Check post-processing rules for example-specific text rewrites.
   - Replace example-specific rewrites with generic model-structure rewrites.

2. `R/model-explanationRules.R`
   - Check prompt/explanation rules for references to built-in examples.
   - Rules should be framed generically, e.g. log-log percentage interpretation, not Diamonds wording.

3. `R/model-question-unit-change.R`
   - Check that unit-change wording uses predictor metadata and units rather than hard-coded example units.

4. `R/model-question-adjustment-comparison.R`
   - Check that adjustment comparison wording is general and not tied to Diamonds IV.

5. Any app example-loading files
   - Confirm example-specific labels and default research questions are isolated to example setup.

6. Tests
   - Keep example-based tests where useful, but ensure tests assert general principles rather than forcing production code to match example-specific prose.

## Desired design direction

Where production code needs to make interpretation choices, it should rely on generic metadata, such as:

- model structure, e.g. `log_log`, `log_linear`, `linear_log`
- response transformation
- predictor transformation
- response name and predictor name passed as metadata
- effect scale, e.g. response difference, response multiplier, odds ratio
- deterministic interpretation payloads
- factor adjustment metadata

Avoid production logic that relies on specific values such as:

- `diamonds`
- `Diamonds IV`
- `carat`
- `price`
- `cut`
- `color`
- `clarity`
- `earthquake`
- `Course`
- `Pass` / `Fail`

If a term appears because it is a variable name supplied by the user/model object and is passed through metadata, that can be acceptable. The audit should distinguish metadata-driven use from hard-coded special handling.

## Stage 33.1 likely deliverable

The first deliverable should probably be audit-only:

- `stage33_example_coupling_review.md`
- no code changes unless an obviously harmful hard-coded rule is found and the user explicitly agrees to fix it immediately

If code changes are needed later, use Stage 33.2 and later for focused repairs.

## Manual validation examples

Use the built-in examples as regression checks, not as implementation targets.

Useful examples include:

- Diamonds III: `log(price) ~ log(carat)`
- Diamonds IV: `log(price) ~ log(carat) + cut + color + clarity`
- earthquake examples involving count/log-link interpretation
- logistic examples involving Pass/Fail probabilities and odds

Validation should ask:

- Does the explanation remain generally correct?
- Is the wording student-facing?
- Is the behavior driven by model structure rather than the example name?
- Would the same code behave sensibly for another dataset with the same model structure?
