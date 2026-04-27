# WMFM Stage 12 Continuation Context

## Project

I am working on the WMFM R package/app. It helps students fit statistical models and receive plain-language model explanations.

This chat has been focused on **Stage 12: example-testing and targeted explanation fixes**. The work is diagnostic and iterative: run real examples, inspect the generated developer-feedback JSON, identify recurring failure modes, then make small staged fixes.

Important project conventions:

- Use R style with `=` for assignment.
- Use camelCase identifiers.
- Always use braces.
- Prefer roxygen2 documentation.
- Prefer `@importFrom` over `pkg::fun()` in package code.
- Generated work should usually include:
  - a changes zip,
  - a run-stage shell script,
  - a clear git commit message.
- Stage scripts must work with my wrapper script `scripts/runStage.sh`.

## Important wrapper script convention

I use a local wrapper:

```bash
scripts/runStage.sh STAGE [DOWNLOADS_DIR]
```

For example:

```bash
scripts/runStage.sh 12_2_8_3 ~/Downloads
```

The wrapper expects these files in the downloads directory:

```text
run_stage12_2_8_3.sh
stage12_2_8_3_changes.zip
```

It runs the stage script like this:

```bash
"$scriptPath" -if -cz "$changesZip"
```

Therefore, all generated stage scripts must support:

```text
-if / --install-files
-cz / --changes-zip
-sn / --start-number
```

`-sn` is important because I often need to restart from a later numbered step after a failure.

## Standard stage script shape

The correct standard script should have **9 steps**, not 8:

```text
[1/9] Installing generated change files
[2/9] Documenting package
[3/9] Running strict test suite
[4/9] Running R CMD check with notes as failures
[5/9] Bumping patch version
[6/9] Documenting after version bump
[7/9] Committing stage changes
[8/9] Creating completed source zip
[9/9] Building and installing package
```

The stage script should also:

- extract change zips into a temporary directory outside the package root, not `.stage*_changes` inside the package root;
- remove any `.stage*_changes` directories before documentation/check/commit;
- avoid committing staging directories;
- support `-sn N` to skip completed steps.

A previous script accidentally created and committed hidden staging directories:

```text
.stage12_2_8_changes
.stage12_2_8_2_changes
```

These caused R CMD check NOTE and then got accidentally committed. This should not happen again.

## Current Stage 12 status

Earlier Stage 12 fixes addressed:

### 12.2.1
Logistic numeric predictor anchor fix.

Core issue:
- `Pass ~ Assign` had been interpreted at assignment score 0, producing nonsensical near-zero pass probabilities.

Outcome:
- Anchor now uses a meaningful mean value for logistic numeric predictors.
- Fitted values are now interpreted at the mean rather than at 0.

### 12.2.2
Probability-first logistic response-scale control.

Core rule:
- Fitted values should be probabilities.
- Effects/comparisons may use odds multipliers or odds ratios.
- Do not mix raw odds, probability, and odds ratios in an uncontrolled reasoning chain.

### 12.2.3
Logistic factor comparison logic.

Core issue:
- `Pass ~ Attend` previously used separate group odds and bad CI-overlap reasoning.

Outcome:
- Now uses probabilities for group fitted values and direct odds ratios for comparisons.
- CI-overlap reasoning was removed.

### 12.2.4
Final answer sentence strengthening.

Core issue:
- Explanations often ended with generic statements rather than a direct answer.

Outcome:
- Prompt now asks for a clear final answer cue and key estimate/comparison.

### 12.2.5
Rounding, CI wording, answer-tag polish.

Core issues:
- anchors and R-squared had inconsistent rounding;
- CI wording sometimes sounded Bayesian or probabilistic;
- some estimate+CI sentences were not classified properly.

### 12.2.6
Intercept-only model explanation repair.

Core issue:
- `Exam ~ 1` produced generic model-mechanics text and omitted numeric answer/CI.

Outcome:
- Prompt now forces intercept-only explanations to use the supplied estimate and confidence interval.
- R-squared/model-fit filler is suppressed for intercept-only models.

### 12.2.7
Intercept-only inferential framing and tag refinement.

Core discussion:
- A sentence like `The estimate is X, with a 95% CI from L to U` should usually be both a point estimate/typical case and uncertainty.
- We decided **not** to add a new `pointEstimate` tag yet.
- Instead, allow such sentences to carry both `typicalCase` and `uncertainty`.

### 12.2.8
Intercept-only pruning, CI wording, and answer tagging.

This was the current final polish area.

Important teaching preference from me:

A good model answer for `Exam ~ 1` is:

```text
The research question is `What is the average mark achieved in the final exam for this course?' Using our data, we estimate this value to be 53 marks, with a 95% confidence interval of 50 to 56 marks. Overall, we can be 95% confident the true value lies somewhere in this range.
```

Important distinction:

- Avoid: `the true value is likely between 50 and 56`
- Prefer/allow: `we can be 95% confident the true value lies somewhere in this range`
- Do not say: `there is a 95% chance/probability`

Also important:
- The final answer sentence in the latest intercept-only example was still not classified as `answer`.
- The final estimate+CI sentence should be tagged as:
  - `typicalCase`
  - `uncertainty`
  - `answer`

## Latest issue before context handoff

The latest work was around **Stage 12.2.8.3**.

A generated script for 12.2.8.3 did not follow the standard stage-script contract. It had:
- 8 steps instead of 9,
- no `-sn` support,
- unreliable build/install behaviour,
- and earlier scripts caused hidden staging directories to be committed.

A corrected replacement script was generated as:

```text
run_stage12_2_8_3_fixed.sh
```

It supports:

```text
-if
-cz
-sn
```

and has the 9-stage structure.

To resume from the commit stage, use:

```bash
bash ~/Downloads/run_stage12_2_8_3_fixed.sh -cz ~/Downloads/stage12_2_8_3_changes.zip -sn 7
```

However, because the earlier bad script had already committed hidden `.stage...` directories, the user may need to clean/amend the commit.

Suggested cleanup if hidden directories were committed:

```bash
git rm -r .stage12_2_8_changes .stage12_2_8_2_changes
git commit --amend --no-edit
git status
```

Then rerun the build/install stage using the fixed script if needed:

```bash
bash ~/Downloads/run_stage12_2_8_3_fixed.sh -cz ~/Downloads/stage12_2_8_3_changes.zip -sn 8
```

or:

```bash
bash ~/Downloads/run_stage12_2_8_3_fixed.sh -cz ~/Downloads/stage12_2_8_3_changes.zip -sn 9
```

depending on where the previous script stopped.

## Current diagnostic example status

The main examples tested were:

### `Pass ~ Assign`
Originally broken due to bad anchor at 0.
Now much better:
- mean anchor used,
- probability baseline sensible,
- odds multiplier used for one-unit assignment effect.

Remaining watch item:
- avoid over-strong CI language such as `guaranteed`.

### `Pass ~ Attend`
Now strong:
- uses group probabilities,
- uses direct odds ratio,
- avoids CI-overlap reasoning.

Remaining issue mostly classification polish:
- probability + CI sentences sometimes need both `typicalCase` and `uncertainty`.

### `Exam ~ Test`
Now strong:
- anchor rounded to 11.6,
- slope effect clear,
- CI wording improved.

R-squared:
- We decided **not** to include R-squared by default unless the research question asks about explanatory power/model fit.
- For a question like `How does final exam mark change as test mark changes?`, R-squared distracts from the slope and is not needed.

### `Exam ~ 1`
Still the most delicate case.
Desired output is concise and inferential:
- state the research question,
- give estimate and CI once,
- use confidence wording,
- avoid model-mechanics filler,
- avoid standalone CI explanations,
- final estimate+CI sentence should be tagged as `answer`.

## File/function areas touched frequently

Likely files involved:

```text
R/model-explanationRules.R
R/prompt-explain.R
R/model-explanationClaimTagDetectors.R
R/model-explanationFormat.R
R/model-numericReference.R
R/prompt-formattedQuantities.R
R/prompt-validationGuard.R

tests/testthat/test-buildExplanationRuleProfile.R
tests/testthat/test-explanation-claim-tags.R
tests/testthat/test-explanation-final-answer-tagging.R
tests/testthat/test-formatExplanationQuantity.R
tests/testthat/test-model-numeric-anchor-prompting.R
tests/testthat/test-prompt-explain-research-question.R
tests/testthat/test-prompt-explanation-skeleton.R
tests/testthat/test-prompt-formatted-quantities.R
```

## Known generated documentation

A man page appeared:

```text
man/addLogisticTwoLevelFactorComparisonRow.Rd
```

This was expected after earlier roxygen changes, but check that it is still intended before committing.

## What to do next in a new chat

1. First, resolve any git damage from hidden staging directories if they were committed.
2. Verify the 12.2.8.3 changes are committed cleanly without `.stage*_changes`.
3. Ensure the package builds and installs using a standard 9-step script.
4. Rerun the intercept-only diagnostic example:

```text
Exam ~ 1
```

Check for:
- a concise answer,
- estimate = 53,
- CI = 50 to 56,
- wording such as `we can be 95% confident`,
- no `likely` CI wording,
- no repeated estimate+CI sentences,
- no filler like `the analysis combined all 146 marks`,
- final sentence tagged as `answer`.

5. If the intercept-only output is good, resume the remaining 19-example Stage 12 review.
6. For future stage scripts, always follow the standard 9-step stage-script structure and support:
   - `-if`
   - `-cz`
   - `-sn`

## Important caution for the assistant

Do not generate placeholder files. Earlier in the prior chat, placeholder zips/scripts were accidentally generated and caused wasted time. Always generate real patch files, and verify:

- zip contents contain only package-relative paths such as `R/...` and `tests/testthat/...`;
- no top-level `README.txt`;
- no `.stage*_changes` in the zip;
- script supports the wrapper flags;
- script passes shell syntax validation if possible;
- script has all 9 stages and builds/installs at the end.

