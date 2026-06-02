# WMFM Stage 33.1 example-coupling review

## Summary

The Stage 33 audit was run on branch `example-coupling-audit` and reported a clean working tree apart from the newly generated `stage33_example_coupling_audit.md` file.

The audit found many hits, but most are false positives caused by broad terms such as `pass`, `fail`, `clarity`, `magnitude`, `color`, and `freq`. The highest-risk production hits are concentrated in a small number of files where example-flavoured language appears in prompt rules, text post-processing, follow-up classification, or semantic factor matching.

The most important finding is that there are several review-level production-code patterns that appear to have grown out of the earthquake, course, or diamonds examples. I do not recommend broad refactoring yet. I recommend a focused Stage 33.2 pass over the specific review items below.

No code changes were made for this review.

## Production-code hits

| Priority | File | Line or area | Term(s) | Classification | Reason | Recommended action |
|---:|---|---|---|---|---|---|
| 1 | `R/model-explanation-cleanText.R` | `postProcessUnitChangePhrasing()` around lines 294-330 | `one-magnitude`, `magnitude` | Review | The rewrite is generic in intent but hard-codes the variable noun `magnitude`. This may be acceptable for common language, but it is also exactly tied to the earthquake examples. | Replace with metadata-aware or variable-name-aware unit-change phrasing where possible. At minimum, make the rule apply to any detected predictor/unit phrase, not only magnitude. |
| 2 | `R/model-explanation-cleanText.R` | `postProcessSurfaceIssueRules()` around lines 1108-1109 | `oneMagnitudeChange` | Review | Diagnostic naming and matching are earthquake-flavoured. It is not necessarily harmful, but it encourages example-specific surface checks. | Generalise the rule name and pattern to unit-change wording, retaining tests for magnitude as one fixture only. |
| 3 | `R/model-question-prediction-lm.R` | `matchSemanticNamedFactorLevel()` around lines 554-563 | `Locn`, `Washington`, `California`, `SC`, `WA` | Likely problem | Production semantic matching maps location words to factor levels for the quake example. This is useful behaviour, but it is hard-coded to a built-in example's geography and variable naming. | Replace with a general alias mechanism or require aliases to come from data/context/example metadata. If this must remain, isolate it as example metadata rather than production prediction logic. |
| 4 | `R/model-question-classifier.R` | expected prediction condition around line 189 | `score`, `mark`, `attendance`, `magnitude`, `washington`, `california` | Review | Follow-up classifier includes example-domain nouns. Some terms are broadly educational, but `washington` and `california` are clearly quake-example specific. | Split generic prediction cues from domain/example aliases. Remove geographic example terms from production classifier unless supplied as metadata. |
| 5 | `R/model-question-classifier.R` | unit-change regexes around lines 127 and 320-321 | `point`, `mark`, `carat`, `magnitude` | Review | These are units/nouns from examples. They may be useful as natural-language cues, but `carat` and `magnitude` are example-specific. | Prefer generic numeric-unit parsing that accepts arbitrary words near the numeric change, then resolves to predictors/units using model metadata. |
| 6 | `R/prompt-equation.R` | equation examples around lines 68-121 | `Pass`, `Attend`, `Test`, `No`, `Yes` | Review | Prompt examples use course/logistic variable names. They are illustrative rather than branching logic, but prompt examples can bias output toward a built-in example. | Consider replacing with symbolic placeholders throughout, or use neutral pseudo-variable names such as `Outcome`, `Group`, and `X`. |
| 7 | `R/prompt-core.R` | prompt rules around lines 75-76 | `course`, `s20x` | OK | This is a defensive prompt rule telling the model not to infer background from abbreviated dataset names. It reduces example coupling rather than creating it. | Keep. |
| 8 | `R/prompt-core.R` | expected-value wording around line 129 | `oysters` | Review | The oyster wording is an example in a production prompt rule. It is probably harmless, but examples in prompts can anchor output. | Replace with a neutral example such as `expected count of the response` or remove the parenthetical example. |
| 9 | `R/prompt-explain.R` | intercept-only rules around line 200 | `course data` | OK | This is a general anti-overclaiming rule for intercept-only models and normalises user research-question wording. | Keep unless later testing shows it distorts non-course examples. |
| 10 | `R/prompt-explain.R` | `normaliseInterceptOnlyResearchQuestion()` around lines 540-541 | `course data`, `course dataset` | Review | This is production text rewriting tied to course examples. It may be useful, but it is hard-coded to one teaching dataset family. | Consider moving to research-question sanitisation rules that avoid sample-only wording generically, rather than course-specific rewriting. |
| 11 | `R/model-question-adjustment-comparison.R` | result field `comparisonType = "adjusted_vs_weight_only_log_log"` | `weight` | Review | The computation is model-structure driven, but the comparison type name assumes a weight-only reduced model, which reflects the diamonds examples. | Rename to a neutral type such as `adjusted_vs_primary_log_log` or `adjusted_vs_log_predictor_only_log_log`. |
| 12 | `R/model-logLog.R` | roxygen example around line 91 | `log(price)` | OK | Documentation example only; no special handling of price. | Keep. |
| 13 | `R/model-glm-notation.R`, `R/model-binomial-outcomes.R`, `R/model-ci-data.R`, `R/model-equation-mean.R` | binomial outcome naming | `Pass`, `Fail`, `failure` | OK | These hits are metadata-driven labels or generic success/failure terminology. They are not evidence of example-specific branching. | Keep. |
| 14 | scoring and grading files including `R/scoring-*.R`, `R/methods-score-*.R`, `R/methods-summary-*.R` | scoring fields | `overallPass`, `clarityScore`, `clarityAdequate`, `fail` | OK | These are rubric/scoring field names or ordinary error/failure wording, not example coupling. | Keep. |
| 15 | `R/examples-run.R` | example transforms and aliases | `course.df`, `diamonds`, `cut`, `color`, `clarity` | OK | This is the correct layer for built-in example setup and transformations. | Keep example-specific logic isolated here. |
| 16 | `R/app-server-data-load-helpers.R` | placeholder around line 131 | `student`, `pass`, `score`, `attendance` | OK | User-facing placeholder for data context entry; illustrative rather than production model logic. | Keep or optionally make the placeholder more neutral later. |
| 17 | `R/text-describeField.R` | field descriptions and examples | `earthquake`, `exam mark`, `clarity` | OK | These are documentation/help descriptions and metric descriptions, not production branching. | Keep. |
| 18 | `R/utils-runRecords.R` | claim-pattern extraction | `cut`, `more frequent` | Review | This appears to be scoring/evidence extraction, not explanation generation. Still worth checking because lexical detectors can encode example-specific expectations. | Inspect only if Stage 33 expands into scoring-coupling cleanup. Not first priority. |
| 19 | `R/scoring-semanticEvidence.R` | semantic evidence extraction around `passing` | `study`, `study effort`, `odds`, `passing` | Review | Scoring logic may contain course/logistic example cues. It does not directly drive student explanations, but can affect grading diagnostics. | Defer unless the project wants scoring rubric generalisation in this stage. |
| 20 | UI, plotting, password, API wrapper, and progress files | many broad terms | `pass`, `fail`, `color`, `freq`, `clarity` | OK | Mostly ordinary English, CSS, plotting aesthetics, password handling, error handling, or pass/fail scoring fields. | No action. |

## Test, docs, examples, and fixture hits

| Priority | File or area | Term(s) | Classification | Reason | Recommended action |
|---:|---|---|---|---|---|
| 1 | `tests/testthat/test-log-log-models.R` | `Diamonds`, `price`, `carat`, `cut`, `color`, `clarity` | OK | Example-specific tests are allowed. These are useful regression fixtures for log-log and adjustment workflows. | Keep, but ensure future assertions test general principles rather than forcing specific prose. |
| 2 | `tests/testthat/test-unit-change-examples.R` | `Diamonds`, `Quakes`, `carat`, `magnitude` | OK | Tests verify built-in example loading and deterministic follow-up setup. | Keep. |
| 3 | `tests/testthat/test-glm-followup-predictions.R` and `test-glm-followup-odds-predictions.R` | `Pass`, `Assign`, `Attend`, `Test`, `Magnitude`, `Locn`, `WA` | OK | These are fixture-driven tests for prediction follow-up handling. | Keep; add non-example aliases if production logic is generalised. |
| 4 | `tests/testthat/test-model-question-classifier.R` | `earthquake`, `Magnitude`, `Locn`, `Carat` | OK | Classifier fixtures intentionally use example questions. | Keep, but add generic variable/unit tests if classifier rules are changed. |
| 5 | `tests/testthat/test-buildModelConfidenceIntervalData.R` and prompt formatted quantity tests | `Pass`, `Freq`, `Magnitude`, `Locn`, `Attend`, `Score` | OK | Tests verify deterministic CI labels and prompt payloads. | Keep. |
| 6 | `tests/testthat/test-buildExplanationClaimEvidenceMap.R`, `test-explanation-claim-tags.R`, `test-explanation-final-answer-tagging.R`, `test-explanation-structural-answer-selection.R` | `earthquake`, `magnitude`, `SC`, `WA` | OK / Review | These are regression fixtures, but they may mirror wording from production post-processing. | Keep for now; after generalising production code, adjust assertions to test structural claims rather than exact earthquake prose. |
| 7 | `inst/extdata/examples/*` | all example names and variables | OK | Built-in examples are explicitly allowed locations for example-specific content. | Keep. |
| 8 | `inst/extdata/developer-scoring/*` | example explanations and scoring exports | OK | These are historical/exported developer fixtures rather than active production logic. | Keep unless package size or stale fixture policy becomes a separate issue. |
| 9 | `man/`, `vignettes/`, `NEWS.md` | example names and variables | OK | Documentation and release notes may mention examples. | Keep. |

## Recommended inspection order

1. `R/model-question-prediction-lm.R`
   - Highest priority because it appears to hard-code semantic factor-level aliases for the earthquake example.
   - Target: replace `Locn`/Washington/California special handling with metadata-supplied aliases or a more general factor-level matching approach.

2. `R/model-question-classifier.R`
   - Review prediction and unit-change cues.
   - Target: remove geographic example words from production classification and generalise unit-name parsing.

3. `R/model-explanation-cleanText.R`
   - Review post-processing rules for `one-magnitude` wording.
   - Target: make unit-change post-processing generic and metadata-driven.

4. `R/model-question-adjustment-comparison.R`
   - Rename `adjusted_vs_weight_only_log_log` to a neutral comparison type.
   - This is low risk and likely safe as a small cleanup.

5. `R/prompt-equation.R`
   - Replace course/logistic concrete examples with neutral placeholders if practical.
   - This is prompt-only, but should be done carefully because prompt examples may have been stabilising equation output.

6. `R/prompt-core.R` and `R/prompt-explain.R`
   - Review illustrative examples and course-specific normalisation.
   - Lower priority than code branching, but worth cleaning if Stage 33 continues.

7. Tests
   - Keep example fixtures, but add generalised tests beside them after production rules are generalised.

## Recommendation

Stage 33 should proceed to code changes, but not as a broad cleanup. I recommend Stage 33.2 as a focused repair stage with these goals:

- Remove hard-coded earthquake location aliasing from `R/model-question-prediction-lm.R` or move it into explicit example metadata.
- Generalise follow-up classifier unit and prediction cues in `R/model-question-classifier.R`.
- Generalise `one-magnitude` text post-processing in `R/model-explanation-cleanText.R`.
- Rename the log-log adjustment comparison type in `R/model-question-adjustment-comparison.R` so it no longer assumes a weight-only reduced model.

I do not recommend changing tests, examples, documentation, or developer-scoring fixtures in Stage 33.1. Those are acceptable places for example-specific terms.

## Versioning note

For subsequent build scripts in this stage, use version series `0.2.8.xxx`, starting with `0.2.8.001`, and increment the final build number on every build attempt regardless of success.
