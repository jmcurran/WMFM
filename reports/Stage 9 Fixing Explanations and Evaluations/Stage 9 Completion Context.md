# WMFM Stage 9 Completion Context

## Branch

Completed branch: `explanation-fix`

Recommended next branch: `prompt-explanation-integration`

## Purpose of Stage 9

Stage 9 was the explanation debugging fix-strategy and infrastructure stage.

The goal was not to rewrite the student-facing explanation prompt immediately. Instead, the goal was to build deterministic infrastructure that makes later prompt and explanation changes safer, more auditable, and less dependent on LLM behaviour.

Stage 9 followed the design decisions established earlier in the workflow:

- use deterministic control where possible
- keep the LLM responsible for phrasing, not computation
- preserve existing audit, scoring, grading, and bad-explanation systems
- extend rather than replace existing claim-evidence machinery
- keep student-facing output unchanged unless explicitly targeted later

## Completed Implementation Stages

### Stage 9.1 - Explanation model profile metadata

Added a deterministic model-profile layer.

Main additions:

- `buildExplanationModelProfile()`
- model-family detection
- model-structure detection
- predictor-type detection
- interaction detection
- model-scale and interpretation-scale metadata
- transformed-response metadata
- model-profile storage on `wmfmModel` objects

Important refinements:

- Stage 9.1.1 fixed predictor classification for formula terms.
- Stage 9.1.2 fixed the `use.names` placement bug in predictor parsing.

Design intent:

- provide a central source of truth for model-aware explanation behaviour
- avoid duplicating model-type logic across prompts, audits, and UI

### Stage 9.2 - Deterministic formatting helpers

Added deterministic formatting helpers for explanation quantities.

Main additions:

- `formatExplanationNumber()`
- `formatExplanationCi()`
- `formatExplanationProbability()`
- `formatExplanationOdds()`
- `formatExplanationMultiplier()`
- `formatExplanationAnchor()`
- `formatExplanationQuantity()`

Rules implemented:

- default two significant figures
- consistent estimate and confidence-interval rounding
- probability formatting
- odds formatting, including `1:N` for very small odds
- multiplier formatting
- anchor display formatting

Design intent:

- stop leaving numeric formatting decisions to the LLM
- reduce scale leakage and awkward decimal presentation

### Stage 9.3 - Explanation anchor policy helpers

Added deterministic helpers for student-facing interpretation anchors.

Main additions:

- `chooseExplanationAnchor()`
- anchor metadata helpers
- tests for typical anchors, zero anchors, and display formatting

Rules implemented:

- do not use zero merely because zero is in range
- prefer meaningful or typical values
- keep raw computational anchor values separate from displayed anchor values
- record anchor reasons for auditability

Design intent:

- make numeric-predictor interpretations pedagogically useful
- prepare for later integration with audit and prompts

### Stage 9.4 - Model-aware explanation rule metadata

Added deterministic model-aware rule metadata and skeleton selection.

Main additions:

- `buildExplanationRuleProfile()`
- explanation skeleton selection
- model-aware rule metadata

Implemented concepts:

- response-scale guidance
- comparison-scope guidance
- interaction explanation structure
- model-aware skeletons for later prompt work

Design intent:

- prepare for deterministic prompt skeletons
- prevent one generic explanation template being used for all model types

### Stage 9.5 - Sentence schema and multi-role tagging extension

Extended the claim-evidence system with Stage 9 sentence-schema fields.

Main additions:

- `R/model-explanationSentenceSchema.R`
- `primaryRole`
- `roles`
- `qualityFlags`
- `supportMapIds`

Important refinements:

- Stage 9.5.1 fixed role-alias compatibility.
- Stage 9.5.2 preserved legacy `claimTags` while placing expanded Stage 9 roles in the new `roles` field.

Key design decision:

- `claimTags` remains backward-compatible.
- Stage 9 roles are additive metadata, not a replacement for existing tagging.
- Multi-role tagging is required.

Design intent:

- support richer diagnostics without breaking scoring, grading, teaching summaries, or existing claim-evidence consumers

### Stage 9.6 - Deterministic validation and quality flags

Added deterministic validation helpers for explanation quality diagnostics.

Main additions:

- `R/model-explanationValidation.R`
- sentence-level quality-flag helpers
- map-level quality-flag support via attributes

Implemented checks include:

- technical language leakage
- raw coefficient leakage
- odds shown as plain decimals
- vague effect wording
- map-level diagnostics

Important refinement:

- Stage 9.6.1 preserved the existing top-level claim-evidence map contract by moving map-level quality flags to an attribute.
- Empty sentence-level `qualityFlags` are now `character(0)`, not `NULL`.

Design intent:

- provide deterministic post-generation checks
- avoid breaking existing object contracts

### Stage 9.7 - Developer feedback integration

Integrated Stage 9 metadata into developer-facing reports and UI diagnostics.

Main additions:

- developer feedback report includes model-profile and rule-profile summaries
- sentence records include:
  - `primaryRole`
  - `roles`
  - `qualityFlags`
  - `supportMapIds`
- developer-mode UI shows Stage 9 sentence diagnostics

Important constraint:

- student-facing explanation display remains unchanged

Design intent:

- make the new Stage 9 infrastructure visible to developers
- prepare for Stage 10 prompt and explanation integration

## Final State

Stage 9 has completed the infrastructure layer for explanation repair.

The codebase now has deterministic support for:

- model profiling
- explanation quantity formatting
- anchor selection and display
- model-aware rule profiling
- multi-role sentence schema metadata
- quality flags
- developer-facing diagnostics

The main student-facing explanation generation prompt has not yet been rewritten. This was deliberate.

## Known Non-Goals from Stage 9

Stage 9 did not attempt to:

- rewrite the main explanation prompt
- change the student-facing explanation text
- alter scoring or grading semantics
- replace the explanation audit system
- replace the claim-evidence system
- fully enforce prompt skeletons
- implement regeneration on failed validation

These are candidates for later stages.

## Recommended Stage 10 Focus

Recommended next workflow:

`Stage 10 - Prompt and explanation integration`

Suggested branch:

`prompt-explanation-integration`

Recommended Stage 10 goals:

1. Feed deterministic formatted quantities into the explanation prompt.
2. Use model profiles and rule profiles to select explanation skeletons.
3. Reduce GLM scale leakage in generated explanations.
4. Improve interaction explanations using within-group effects followed by comparison of effects.
5. Use anchor metadata in student-facing explanations.
6. Keep strong existing Stage 8 examples working.
7. Use deterministic quality flags to guide prompt repair and developer feedback.

Recommended first Stage 10 implementation step:

- connect `buildExplanationRuleProfile()` and formatted explanation quantities to prompt construction without changing the visible explanation style more than necessary.

## Merge and Tag Plan

Completed branch:

`explanation-fix`

Next branch:

`prompt-explanation-integration`

Recommended merge plan:

1. Tag the completed `explanation-fix` branch state.
2. Merge `explanation-fix` into `master`.
3. Tag the merged `master` state.
4. Push branch, master, and both tags.
5. Create and push `prompt-explanation-integration` from updated `master`.

