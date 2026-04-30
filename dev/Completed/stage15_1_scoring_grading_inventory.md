# Stage 15.1 scoring and grading inventory

## Purpose

Stage 15.1 is an exploratory compatibility review of the current WMFM scoring and grading system after the explanation pipeline gained deterministic audit data, post-processing, diagnostics, claim/evidence mapping, and student-facing teaching summaries.

This stage intentionally avoids functional rewrites. Its main output is a map of the current APIs, classes, run-record fields, scoring paths, tests, and recommended follow-up stages.

## Current public API surface

### `grade()`

- Generic: `grade(x, ...)` in `R/api-grade.R`.
- Main method: `grade.wmfmModel()` in `R/methods-grade-wmfmModel.R`.
- Current target: one or more user-supplied explanations for a fitted `wmfmModel`.
- Returns:
  - `wmfmGrade` for one explanation.
  - `wmfmGradeListObj` for multiple explanations.
- Supports `method = "deterministic"`, `"llm"`, or `"both"`.
- Supports `autoScore`, with deprecated alias `score`.
- Supports repeated LLM grading via `nLlm`.
- Includes a non-interactive large-job guard through `confirmLargeLlmJob` and `maxLlmJobsWithoutConfirmation`.

### `score()`

- Generic: `score(x, ...)` in `R/api-score.R`.
- Main methods:
  - `score.wmfmGrade()` in `R/methods-score-wmfmGrade.R`.
  - `score.wmfmGradeListObj()` in `R/methods-score-wmfmGradeListObj.R`.
  - `score.wmfmRuns()` in `R/methods-score-wmfmRuns.R`.
- Current targets:
  - A single grading object.
  - A batch grading object.
  - Repeated model explanation runs.
- Deterministic scoring delegates to `scoreWmfmRunRecordsCore()`.
- LLM scoring delegates to `scoreWmfmRunWithLlm()` and `scoreWmfmRunsWithLlm()`.

## Current core objects

### `wmfmGrade`

Constructor: `newWmfmGrade()` in `R/class-wmfmGrade.R`.

Current fields:

- `model`: original `wmfmModel` object.
- `input`: candidate explanation and optional model answer.
- `scoreScale`: displayed mark scale.
- `records`: student and optional model-answer run records.
- `scores`: method-level score outputs, mark, overall score, and metric summary.
- `feedback`: method-level feedback, where marks were lost, strengths, weaknesses, missing elements, and model-answer comparison.
- `meta`: creation and scoring metadata.

Assessment:

- The object already stores the fitted model, so it is a natural place to add audit-aware and diagnostics-aware scoring without breaking the existing text-only surface.
- The `records` slot currently carries text-derived claims and model context, but not enough explicit linkage to explanation-audit, diagnostic, claim/evidence, or teaching-summary objects.
- The `scores` and `feedback` structures are still compatible with a revised rubric, provided new dimensions are added carefully.

### `wmfmGradeListObj`

Constructor: `newWmfmGradeListObj()` in `R/class-wmfmGradeListObj.R`.

Current fields:

- `model`: original `wmfmModel` object.
- `inputs`: supplied explanations and optional model answer.
- `grades`: named list of `wmfmGrade` objects.
- `meta`: batch metadata, including method, `nLlm`, total LLM calls, elapsed time, mean time per explanation, mean time per LLM call, large-job guard settings, and scoring metadata.

Assessment:

- The class name matches the preferred convention.
- Batch timing and large-job guard fields are still useful.
- Future revisions should keep the core object as a list of `wmfmGrade` objects and avoid flattening too early.

### `wmfmScores`

Constructor: `newWmfmScores()` in `R/class-wmfmScores.R`.

Current fields:

- `exampleName`.
- `package`.
- `runIds`.
- `methods`.
- `scores`.
- `meta`.

Assessment:

- This object is aligned to `wmfmRuns`, not directly to `wmfmGrade`.
- It remains useful for repeated generated-explanation scoring, comparison, plotting, and diagnosis.
- It should probably remain separate from `wmfmGrade` and `wmfmGradeListObj`, but the rubric dimensions should be shared where possible.

## Current run-record schema

The main run-record builder is `buildWmfmRunRecord()` in `R/utils-runRecords.R`.

It stores three broad groups:

1. Metadata and model context.
2. Raw explanation text and text summaries.
3. Extracted claim variables.

Notable fields include:

- model context: `modelType`, `modelFamily`, `linkFunction`, `formula`, `equationsText`, `interactionTerms`, `hasInteractionTerms`, `nInteractionTerms`, `interactionMinPValue`, `interactionAlpha`.
- explanation text: `explanationText`, `normalizedExplanation`, `explanationPresent`, `wordCount`, `sentenceCount`.
- text-derived claims: `ciMention`, `percentLanguageMention`, `referenceGroupMention`, `interactionMention`, `uncertaintyMention`, `usesInferentialLanguage`, `usesDescriptiveOnlyLanguage`, `overclaimDetected`, `underclaimDetected`, `conditionalLanguageMention`, `comparisonLanguageMention`, `outcomeMention`, `predictorMention`, `effectDirectionClaim`, `effectScaleClaim`, `interactionSubstantiveClaim`, `inferentialRegister`, `uncertaintyTypeClaim`.

Assessment:

- The run-record design correctly separates raw/extracted text evidence from judged scoring fields.
- It is still heavily text-derived and heuristic.
- It does not yet clearly expose the newer deterministic evidence sources such as audit anchors, post-processing diagnostics, claim/evidence objects, and teaching summaries.

## Current deterministic scoring path

The deterministic core is `scoreWmfmRunRecordsCore()` in `R/scoring-runRecords-core.R`, wrapped by `scoreWmfmRepeatedRuns()`.

Current deterministic dimensions:

- `factualScore`.
- `inferenceScore`.
- `completenessScore`.
- `clarityScore`.
- `calibrationScore`.
- `overallScore`.
- `overallPass`.

Current judged fields include:

- `effectDirectionCorrect`.
- `effectScaleAppropriate`.
- `referenceGroupHandledCorrectly`.
- `interactionCoverageAdequate`.
- `interactionSubstantiveCorrect`.
- `uncertaintyHandlingAppropriate`.
- `inferentialRegisterAppropriate`.
- `mainEffectCoverageAdequate`.
- `referenceGroupCoverageAdequate`.
- `clarityAdequate`.
- `numericExpressionAdequate`.
- `comparisonStructureClear`.
- `fatalFlawDetected`.

Assessment:

- The broad dimension split is still useful and should be retained.
- The deterministic path is mostly operating from final text and heuristic extracted claim variables.
- The next alignment step should add optional deterministic evidence from the explanation pipeline rather than replacing the current text-only path.

## Current LLM scoring path

Main files:

- `R/scoring-llm.R`.
- `R/prompt-score.R`.

Current behavior:

- `scoreWmfmRunWithLlm()` obtains a JSON scoring response from a chat provider.
- `parseWmfmScoringJson()` parses the response.
- `validateWmfmParsedScores()` validates required fields and bounds.
- `applyWmfmLlmScoresToRecord()` converts parsed fields into a score record.
- Repeated LLM grading is guarded and timed.

Assessment:

- The LLM boundary is explicit and has validation.
- Tests should continue to use fake providers only.
- There appear to be two prompt-building locations: `R/scoring-llm.R` and `R/prompt-score.R`. This should be reviewed in Stage 15.4 or earlier because duplicate definitions can obscure which prompt is actually used.
- LLM grading should remain optional and bounded, not the primary source of correctness.

## Existing test coverage relevant to Stage 15

The current code base contains broad scoring and grading coverage, including tests for:

- `grade.wmfmModel()`.
- grade feedback.
- method selection.
- multiple explanations.
- LLM large-job guard.
- repeated LLM grading.
- `score.wmfmRuns()`.
- `scoreWmfmRunRecordsCore()`.
- LLM run scoring helpers.
- parsed LLM score validation.
- score comparisons, plotting, and diagnosis.
- explanation audit, claim/evidence, and diagnostics.

Assessment:

- The test suite already gives a useful safety net.
- Stage 15.2 should add tests that prove deterministic scoring can consume audit/diagnostic/evidence inputs when available while preserving old text-only behavior.
- LLM tests should remain fake-provider only.

## Compatibility findings

### Still aligned

- `wmfmGrade` and `wmfmGradeListObj` match the preferred object naming convention.
- Deterministic scoring already has distinct correctness, inference, completeness, clarity, and calibration dimensions.
- Run records avoid embedding judged scores directly at creation time.
- Repeated LLM grading has timing and large-job safeguards.
- Batch grading stores timing and LLM-call metadata.

### Needs alignment

- The grading API is not yet explicit about whether it is grading raw LLM text, final displayed text, before/after text, student text, or explanation-plus-evidence.
- Deterministic scoring does not yet make direct use of the newer explanation audit, diagnostics, claim/evidence, or teaching-summary objects.
- Surface-language diagnostics can probably inform clarity/style feedback, but should not be treated as proof of statistical correctness.
- Audit and claim/evidence objects should become optional evidence inputs for correctness and completeness checks.
- LLM prompts should be reviewed for duplication and for boundaries around when LLM judgement is allowed.

## Recommended Stage 15 sequence

### Stage 15.2: deterministic evidence alignment

Add optional evidence inputs to the run-record or grading path. The first implementation should be conservative:

- preserve text-only grading compatibility;
- include audit/diagnostic/evidence slots only when available;
- add deterministic checks for evidence presence and required numeric anchors;
- let diagnostics affect clarity/style feedback, not numerical correctness by themselves;
- add offline tests.

### Stage 15.3: rubric and object cleanup

Review the `scores`, `feedback`, and `meta` structures in `wmfmGrade`, `wmfmGradeListObj`, and `wmfmScores`.

Likely changes:

- document which fields are stable public outputs;
- keep internal evidence fields out of normal print output;
- improve summaries around rubric dimensions;
- avoid removing fields until compatibility impact is clear.

### Stage 15.4: LLM grading boundary review

Review LLM prompts, provider calls, duplicate prompt definitions, fake-provider tests, repeated grading behaviour, timing, and large-job guards.

### Stage 15.5: pipeline integration

Decide whether grading should accept `wmfmModel` directly as an evidence-rich object, likely using:

- final displayed explanation text;
- explanation audit;
- diagnostics;
- claim/evidence map;
- teaching summary components;
- fallback text-only behavior for older inputs.

## Proposed design rule for the rest of Stage 15

Use deterministic evidence first. Treat LLM grading as optional support for pedagogy and clarity, not as the authority for numerical or statistical correctness.
