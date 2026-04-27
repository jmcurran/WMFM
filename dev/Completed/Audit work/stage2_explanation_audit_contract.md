# Explanation-audit contract for WMFM

## Title
Frozen contract for the deterministic explanation-audit subsystem

## Purpose
This document defines the required contract for the deterministic explanation-audit subsystem in WMFM.

The contract is intended to:
- freeze what the audit object must contain
- separate deterministic R-side evidence from later student-facing presentation
- define what later rebuild streams may safely assume
- provide a stable target for implementation and regression tests

This contract is based on the current `exp-to-exp-iv` implementation as represented in the attached Stage 4.6 codebase, but it tightens that implementation into an explicit set of guarantees.

## Core object contract

### Object identity
The explanation audit must:
- be an object of class `wmfmExplanationAudit`
- be list-based and inspectable in ordinary R
- be created deterministically from the fitted model and existing deterministic helper outputs
- not require a live LLM call to exist or to be correct

### Storage contract
The explanation audit must:
- be built during `runModel()`
- be attached to the returned `wmfmModel` object as `explanationAudit`
- be available to runtime and app helpers without needing chat access
- act as the primary stored audit record for the fitted model

### Determinism contract
The explanation audit must only depend on:
- the fitted model object
- deterministic model metadata and attributes already attached on the R side
- deterministic helper outputs such as numeric anchors, CI summaries, fitted-value support quantities, reference levels, and phrasing rules

The audit must not depend on:
- live LLM output
- hidden reasoning
- transient Shiny session state
- non-deterministic runtime sampling

## Top-level structure contract
The audit object must contain the following top-level fields.

### 1. `transparencyNote`
Type:
- length-1 character

Purpose:
- states clearly that the audit contains deterministic explanation inputs and evidence
- explicitly avoids claiming access to hidden chain-of-thought

Required guarantee:
- must always be present

### 2. `overview`
Type:
- named list

Required fields:
- `response`: length-1 character
- `predictors`: character vector
- `nObservations`: length-1 numeric or integer
- `modelFamily`: length-1 character
- `link`: length-1 character
- `hasDatasetContext`: length-1 logical
- `hasResearchQuestion`: length-1 logical

Purpose:
- summarises the fitted-model context needed to interpret the rest of the audit

Required guarantee:
- must always be present

### 3. `promptInputs`
Type:
- named list

Required fields:
- `response`: length-1 character
- `responseNounPhrase`: length-1 character
- `predictors`: character vector
- `datasetContextUsed`: length-1 logical
- `datasetName`: length-1 character or `NA_character_`
- `researchQuestionUsed`: length-1 logical
- `researchQuestion`: length-1 character or `NA_character_`
- `coefficientTableIncluded`: length-1 logical
- `confidenceIntervalsIncluded`: length-1 logical
- `precomputedBaselineValuesIncluded`: length-1 logical
- `numericAnchorRuleIncluded`: length-1 logical
- `nObservations`: length-1 numeric or integer

Purpose:
- records the structured ingredients made available to the explanation workflow

Required guarantee:
- must always be present

### 4. `promptRules`
Type:
- character vector

Purpose:
- records the main deterministic prompt-side instruction themes that govern explanation construction

Required guarantee:
- must always be present
- must include the numeric-anchor rule
- must include the student-facing interpretation-scale rule
- may include a research-question rule when a research question is present

### 5. `interpretationScale`
Type:
- named list

Required fields:
- `responseExpression`: length-1 character
- `responseTransform`: length-1 character
- `fittedValueScale`: length-1 character
- `effectScale`: length-1 character
- `backTransformation`: length-1 character
- `explanationScaleNote`: length-1 character

Purpose:
- records the scale on which fitted values and effects should be discussed
- records any supported back-transformation behaviour

Required guarantee:
- must always be present
- must reflect model-family-specific behaviour where supported

### 6. `numericAnchor`
Type:
- named list

Required fields:
- `numericReference`: length-1 character
- `note`: length-1 character
- `table`: data frame

Purpose:
- records how numeric baseline/reference values were chosen for interpretation

Required guarantees:
- must always be present
- `table` may be empty when there are no numeric predictors
- when numeric predictors exist, `table` must contain these columns:
  - `predictor`
  - `observedRange`
  - `anchor`
  - `reason`
- the anchor rule must be deterministic and data-aware

### 7. `referenceLevels`
Type:
- data frame

Purpose:
- records the reference level used for each factor predictor

Required guarantees:
- must always be present
- may be empty when there are no factor predictors
- when non-empty, it must contain these columns:
  - `predictor`
  - `referenceLevel`
  - `levels`

### 8. `confidenceIntervals`
Type:
- named list

Required fields:
- `level`: length-1 numeric
- `mode`: length-1 character or `NA_character_`
- `note`: length-1 character or `NA_character_`
- `teachingNote`: length-1 character or `NA_character_`
- `displayedScales`: character vector

Purpose:
- records how CI information is being represented and on which displayed scales

Required guarantees:
- must always be present
- must not require the full CI table to remain attached directly here
- must support empty-CI cases gracefully

### 9. `baselineEvidence`
Type:
- data frame

Purpose:
- stores the subset of deterministic CI-derived evidence corresponding to baseline quantities

Required guarantees:
- must always be present
- may be empty if no such evidence exists
- when non-empty, columns should be drawn from the supported evidence schema:
  - `ciSection`
  - `quantity`
  - `estimate`
  - `lower`
  - `upper`
  - `scale`
  - `displayScale`

### 10. `effectEvidence`
Type:
- data frame

Purpose:
- stores the subset of deterministic CI-derived evidence corresponding to effects and contrasts

Required guarantees:
- must always be present
- may be empty if no such evidence exists
- when non-empty, columns should be drawn from the supported evidence schema:
  - `ciSection`
  - `quantity`
  - `estimate`
  - `lower`
  - `upper`
  - `scale`
  - `displayScale`

### 11. `coefficientTable`
Type:
- data frame

Purpose:
- stores the deterministic coefficient summary made available to the explanation workflow

Required guarantees:
- must always be present
- must contain a `term` column
- numeric columns may be rounded for readability, but rounding must be deterministic
- known perfect-fit warnings may be muffled during table creation, but the resulting table must still be deterministic

### 12. `rawPromptIngredients`
Type:
- named list

Required fields:
- `languageContract`: length-1 character
- `numericAnchorPrompt`: length-1 character
- `anchoredBaselinePrompt`: length-1 character

Purpose:
- stores structured prompt-support fragments that are still useful for inspection and later teaching layers

Required guarantees:
- must always be present
- contents are prompt-support artefacts, not hidden reasoning

## Behavioural invariants

### Invariant A: presence after `runModel()`
Any successful `runModel()` call that returns a `wmfmModel` must also return a valid `wmfmExplanationAudit` in `out$explanationAudit`.

### Invariant B: offline construction
`buildModelExplanationAudit()` and any app/runtime wrapper that exposes the audit must work without chat access.

### Invariant C: stable empty cases
Where a section can be empty because the model does not contain the relevant structure, the section must still be present in the audit with the correct type.

Examples:
- no factor predictors -> `referenceLevels` is an empty data frame
- no numeric predictors -> `numericAnchor$table` is an empty data frame
- no available CI table -> `baselineEvidence` and `effectEvidence` may be empty, while `confidenceIntervals` still exists

### Invariant D: no UI-only coupling
The core audit object must remain usable outside the app. App-facing helper code may reshape or display it, but must not redefine the audit contract.

### Invariant E: no hidden-thought claims
The audit must not be described as hidden reasoning, private chain-of-thought, or internal deliberation. It is an inspectable record of deterministic explanation inputs and evidence.

## Minimum supported model coverage
The audit contract must support at least:
- linear models fit with `lm()`
- binomial logistic models fit with `glm(..., family = binomial(link = "logit"))`
- Poisson log-link models fit with `glm(..., family = poisson(link = "log"))`

The audit may support additional model cases, but the contract should only test what WMFM explicitly supports.

## Printing and inspection contract
`print.wmfmExplanationAudit()` must:
- exist
- return the object invisibly
- provide readable developer-facing output
- avoid relying on Shiny or HTML rendering
- remain deterministic

Minimum print coverage should include:
- title/header
- transparency note
- interpretation-scale summary
- numeric-anchor summary when present
- reference-level summary when present

Longer-term print improvements are allowed, but they must not change the underlying object contract.

## Recommended regression tests
The contract implies the following minimum regression coverage.

### Construction tests
- `buildModelExplanationAudit()` returns class `wmfmExplanationAudit`
- all required top-level fields are present
- required subsection fields are present
- correct empty-shape behaviour for missing numeric or factor predictors

### Model-family tests
- linear model returns response-scale additive interpretation metadata
- logistic model returns odds/probability interpretation metadata
- Poisson model returns expected-count / multiplier interpretation metadata

### Numeric-anchor tests
- when 0 lies inside the observed range, the anchor rule uses 0
- when 0 lies outside the observed range, the anchor rule uses the sample mean
- numeric-anchor table columns and reasons are present and stable

### Reference-level tests
- factor predictors appear in `referenceLevels`
- the baseline level matches the first factor level used by the model frame

### CI/evidence tests
- `confidenceIntervals` exists even when CI data are limited
- `baselineEvidence` and `effectEvidence` have stable supported columns
- displayed scales reflect the relevant model family where applicable

### Runtime/storage tests
- `runModel()` stores `explanationAudit` on the returned `wmfmModel`
- app/runtime audit helpers work without live chat access

### Print tests
- `print.wmfmExplanationAudit()` returns invisibly
- printed output includes key section headings or summary text without error

## Current implementation versus contract
The current codebase already satisfies much of this contract:
- classed audit object exists
- `runModel()` storage exists
- runtime/app access exists
- several deterministic tests already exist

The main remaining work implied by this contract is:
- expand the regression tests so they check the full contract, not just a few sentinel fields
- decide whether print support should remain minimal or become more informative
- confirm there is no duplicated or conflicting audit recomputation path in app/runtime helpers
- document this contract somewhere durable in the package workflow or developer docs

## Handoff assumptions for later rebuild streams
Later work on teaching summaries or student-facing explanation panels may assume:
- the audit object is deterministic
- the audit object is present on the fitted `wmfmModel`
- numeric anchors, reference levels, CI metadata, coefficient summaries, and prompt-support fragments are available without a live LLM call

Later work must not assume:
- that the print method is the student-facing UI
- that prompt-support fragments represent hidden reasoning
- that every future teaching field already belongs in the core audit contract

## Deferred questions
These questions are intentionally left open for later stages:
- whether additional claim-evidence fields belong in the core audit object or only in adjacent subsystems
- whether teaching-summary material should be attached directly to the model or computed from the audit on demand
- whether a richer summary/print method should expose more subsections by default
