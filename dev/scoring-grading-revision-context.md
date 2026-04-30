# WMFM context: scoring and grading revision after explanation-system review

## Purpose

This context is for a new WMFM work stream focused on revisiting the scoring and grading functions in light of the recent explanation-system work.

The recent explanation stages added more deterministic structure around explanation generation, including post-processing, diagnostics, and before/after review helpers. That work changes what it means to score or grade an explanation, because the explanation text is no longer just raw LLM prose. It is now the product of a more structured pipeline.

## Recommended branch

Suggested branch name:

```bash
git checkout -b scoring-grading-revision
```

If a different branch name is used, keep the stage scripts and context aligned with that branch.

## Background

WMFM has been evolving from a mostly LLM-generated explanation system toward a more deterministic and auditable explanation pipeline.

Completed or recent explanation-related work includes:

- deterministic explanation audit data attached to fitted models
- student-facing teaching summaries
- claim-to-evidence mapping
- explanation surface post-processing
- diagnostics for undesirable explanation-language patterns
- before/after diagnostic helpers for developer review
- conservative deterministic rules for removing or flagging model-mechanism leakage, awkward unit-change wording, verbal fractions, and multiplier confidence-interval phrasing

The scoring and grading system should now be reviewed against this newer architecture.

## Core question

The main question is:

> Are the current scoring and grading functions still assessing the right object, using the right evidence, now that explanations have deterministic audit data, post-processing, diagnostics, and claim-to-evidence support?

## Why revision is likely needed

The earlier scoring/grading work may have been designed when explanation quality was mostly judged from final text alone.

That may no longer be sufficient. The final displayed explanation is now influenced by multiple layers:

1. model fitting and derived quantities
2. deterministic audit payloads
3. prompt construction
4. LLM explanation generation
5. post-processing
6. surface-language diagnostics
7. claim/evidence mapping and teaching summary components

A grading function that only inspects final prose may miss important information about whether the explanation is correct, traceable, pedagogically useful, or merely well-worded.

## Guiding principle

Do not let the grader become another unconstrained LLM layer that guesses whether the explanation is good.

Prefer a hybrid structure:

- deterministic evidence first
- explicit rubric dimensions
- clear separation between correctness, completeness, clarity, and style
- optional LLM judgement only where deterministic evidence is insufficient
- offline tests for deterministic paths
- mocked or fake providers for LLM paths

## Initial goals

The first stage of this work should be exploratory and stabilising, not a large rewrite.

Likely first tasks:

1. Inspect the current scoring and grading functions.
2. Identify their inputs, outputs, classes, and tests.
3. Map current score categories to the newer explanation pipeline.
4. Identify any redundant or outdated fields in score objects and run records.
5. Decide whether scoring should operate on:
   - final explanation text only
   - final text plus audit object
   - final text plus diagnostics
   - claim/evidence objects
   - teaching summary objects
   - the full `wmfmModel` object
6. Identify what should be deterministic versus LLM-assisted.

## Specific issues to re-examine

### 1. What is being graded?

Possible targets:

- raw LLM explanation before post-processing
- final displayed explanation after post-processing
- before/after pair
- explanation plus audit data
- user-written student explanation
- model-generated explanation
- a batch of explanations

The API should be explicit about which of these is being graded.

### 2. Correctness versus surface quality

The new post-processing layer improves surface quality, but correctness should not be inferred from polished language.

Separate dimensions may include:

- numerical correctness
- interpretation correctness
- scale correctness
- confidence-interval interpretation
- comparison/contrast correctness
- use of model-derived quantities rather than invented values
- pedagogical clarity
- forbidden or undesirable phrasing
- completeness relative to the fitted model

### 3. Use of explanation diagnostics

The new diagnostic helpers should probably contribute to grading, but carefully.

Possible role:

- flag surface-language issues
- reduce clarity/style score
- provide feedback to students or developers
- avoid treating surface diagnostics as proof of statistical incorrectness

### 4. Use of audit and claim evidence

The audit and claim/evidence layers can provide a stronger basis for deterministic grading.

Potential uses:

- check whether required numerical anchors are mentioned
- check whether confidence intervals are interpreted on the correct scale
- check whether factor comparisons and interaction quantities are represented
- check whether the explanation contains model-derived quantities actually present in the audit payload

### 5. LLM grading boundaries

LLM grading may still be useful, especially for pedagogy and clarity, but it should be bounded.

Possible rules:

- never use real LLM calls in tests
- provide fake providers or mocks
- keep deterministic checks available as the default where possible
- make repeated LLM grading opt-in and clearly bounded
- preserve timing/progress information for expensive repeated grading
- keep safety checks for large grading jobs

### 6. Object design

Revisit classes and naming.

Known preference:

- use `wmfmGrade` for one explanation
- use `wmfmGradeListObj` for multiple explanations
- avoid class names that differ only by one letter
- avoid flattening core objects into data frames too early
- provide `print()` and `summary()` methods where useful

### 7. Stored fields and run records

There may be fields in repeated runs, scores, or grading records that are no longer needed.

Review whether all stored fields still serve a purpose after the explanation-system changes.

Questions:

- Which fields are essential for reproducibility?
- Which fields are only intermediate debug data?
- Which fields duplicate information now available in audit or diagnostics objects?
- Which fields should be exposed in summaries versus kept internal?

## Suggested staged plan

### Stage 15.1: inventory and compatibility review

- Inspect existing scoring and grading files and tests.
- Produce a concise map of current APIs, classes, and object fields.
- Identify which pieces are still aligned with the new explanation architecture.
- Avoid code changes unless small documentation or test-discovery fixes are needed.

### Stage 15.2: deterministic scoring alignment

- Add or adjust deterministic checks so scoring can use explanation diagnostics and audit/evidence objects where available.
- Preserve backward compatibility for older inputs.
- Add offline tests.

### Stage 15.3: grading object cleanup

- Review `wmfmGrade` and `wmfmGradeListObj` structures.
- Remove or deprecate redundant fields only if safe.
- Improve print/summary output to reflect the new rubric dimensions.

### Stage 15.4: LLM grading boundary review

- Review LLM grading prompts and provider interfaces.
- Ensure tests use fake providers only.
- Ensure repeated grading retains progress and timing behaviour.
- Add safeguards for large repeated grading requests.

### Stage 15.5: integration with explanation pipeline

- Decide whether grading should accept `wmfmModel` objects directly.
- If so, use final explanation text plus audit/diagnostic/evidence components.
- Keep existing text-only grading compatibility where sensible.

## Coding and workflow preferences

Use the normal WMFM workflow:

- R style: `=` for assignment, camelCase identifiers, braces for all control structures
- use roxygen2 documentation
- prefer `@importFrom` over `pkg::fun()` in package code where appropriate
- tests must run offline
- no real LLM calls in tests
- use fake providers, existing mocks, or deterministic helpers for LLM boundaries
- provide changes as downloadable zips
- provide strict stage scripts
- stage scripts should accumulate commit notes across substeps
- if a substep fails, use numbering like `15.1.1`, `15.1.2`, etc. until resolved

## Important design caution

Do not start by adding more LLM capability.

Start by understanding whether the current scoring and grading system still reflects the now more deterministic explanation pipeline. The most valuable first outcome is a clear map of what should remain, what should be revised, and where deterministic evidence should replace heuristic text judgement.

## Expected first prompt for the new chat

A useful opening prompt would be:

```text
I want to start the scoring and grading revision work for WMFM. Please use the attached context and the latest code base. Start at Stage 15.1. The goal of the first stage is to inventory the current scoring and grading APIs, tests, classes, object fields, and how they should change now that the explanation system has deterministic audit, post-processing, diagnostics, and claim/evidence layers. Please use my r-development-style and wmfm-stage-script-generator skills.
```
