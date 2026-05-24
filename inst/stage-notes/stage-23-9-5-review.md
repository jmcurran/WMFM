# Stage 23.9.5: Full Stage 23 pre-polish code review

## Scope reviewed
This review covers Stage 23 implementation commits from **23.1 through 23.9.4** (inclusive), with emphasis on:

- research question vs follow-up separation
- classifier consistency
- prompt guardrail consistency
- prediction vs prediction-interval terminology
- deterministic computation boundaries
- brittle parsing/regex risk
- duplicated prompt wording
- unsupported-request handling
- version-history consistency
- full-stage tests
- whether Stage 23.10 should be polish-only

## Executive decision
**Recommendation:** Stage 23.10 should **not** be polish-only. It should include a small, bounded fix set for correctness/consistency before polish freeze.

## Findings

### 1) Research question vs follow-up separation
**Status:** Mostly strong, with one boundary risk.

- Separation architecture is clear: research question is handled through dedicated context and follow-up is classified independently.  
- `buildResearchQuestionPredictionPayload()` can deterministically convert prediction-shaped research questions into structured payloads, maintaining bounded behavior.  
- In `lmToExplanationPrompt()`, when no explicit follow-up is present, prediction-shaped research questions may become the active follow-up payload, which is intentional but should be explicitly documented as precedence behavior.

**Risk:** this fallback precedence could be mistaken for a user-entered follow-up pathway unless documented in Stage notes/tests.

### 2) Classifier consistency
**Status:** Good progress, but category handling is asymmetric.

- Classifier ordering correctly prioritizes ambiguous multi-unit rejection before later category returns.
- `prediction interval` is detected before broader `prediction` detection, reducing false downgrades.
- However, only a subset of categories map to deterministic control lines; prediction categories rely on other pathways and are therefore less visibly centralized.

**Suggested Stage 23.10 fix:** add an explicit no-op or delegating control entry for prediction categories to make mapping completeness auditable.

### 3) Prompt guardrail consistency
**Status:** Strong but verbose and duplicated in places.

- Guardrails in prompt core are extensive and align with WMFM constraints.
- Follow-up control block avoids raw follow-up text as instructions.
- Some guardrail ideas are repeated across prompt sections with slight phrasing variation; this increases regression risk when one copy changes.

**Suggested Stage 23.10 fix:** consolidate repeated guardrail sentences into reusable helper fragments where feasible.

### 4) Prediction vs prediction-interval terminology
**Status:** Mostly consistent with one wording mismatch.

- Deterministic payload distinguishes `mean_response_prediction` vs `individual_prediction_interval`.
- Non-lm warning text still references “Stage 23.7 only supports … prediction follow-ups”, which is historically accurate for that function’s introduction but stale at Stage 23.9.x.

**Suggested Stage 23.10 fix:** replace stage-number-specific warning strings with capability-based wording (e.g., “This pathway currently supports ordinary lm only”).

### 5) Deterministic computation boundaries
**Status:** Good.

- Deterministic computation owns predictor parsing, validation, and `predict()` calls.
- Prompt control text explicitly blocks extra model-side computation when unsupported.

**Residual risk:** `extractPredictionAssignmentPairs()` grammar remains intentionally narrow (`name = value` only), which is safe but may feel brittle for ordinary student phrasing.

### 6) Brittle parsing / regexes
**Status:** Main risk area.

- Classifier and prediction parsing rely on regex-heavy heuristics.
- Unit-change extraction handles many common forms but can still miss natural language variants.
- Assignment parsing does not support quoted strings with spaces, inequality phrasing, or alternate separators (e.g., `x: 5`).

**Suggested Stage 23.10 fix (bounded):**
1. add tests for near-miss phrasings and false positives,
2. add explicit “clarification needed” reason codes for parseable intent but unparseable values,
3. avoid expanding grammar aggressively at this stage.

### 7) Duplicated prompt wording
**Status:** Present.

- Similar confidence-interval and multiplicative-effect cautions appear in multiple prompt blocks.
- Duplication increases maintenance burden and raises risk of internal inconsistency.

**Suggested Stage 23.10 fix:** perform low-risk deduplication in helper builders without semantic changes.

### 8) Unsupported-request handling
**Status:** Good and safer than earlier stages.

- Unsupported follow-ups are withheld from direct instruction channels.
- Ambiguous multi-unit requests now deterministically reject with clarification message.

**Remaining gap:** unsupported reasons are not fully normalized across classifier, prediction validator, and prompt rendering paths.

### 9) Version-history consistency
**Status:** Mixed.

- `DESCRIPTION` version bumping appears consistent through iterative stages.
- Some user-facing/internal warnings encode specific stage numbers, which can age quickly and conflict with current version history.

**Suggested Stage 23.10 fix:** scrub stale stage-number strings from runtime messages.

### 10) Tests across full Stage 23
**Status:** Broad and meaningful.

- Stage introduced/updated tests across classifier behavior, deterministic prediction, research-question prediction behavior, prompt controls, and app reactive state.
- Remaining opportunity is targeted edge-case tests around regex brittleness and unsupported reason-code normalization.

## Recommended Stage 23.10 content

### Include these fixes (not polish-only)
1. **Message consistency pass** for stale stage-number wording in runtime warnings.
2. **Reason-code normalization** for unsupported/needs-input branches.
3. **Small dedup pass** for repeated guardrail sentence fragments.
4. **Edge-case tests** for parser/regex near misses and ambiguous wording.

### Keep out of Stage 23.10
- broad grammar redesign for free-form predictor extraction,
- classifier ontology expansion beyond current bounded categories,
- any non-lm prediction feature work.

## Release gate recommendation
Proceed to final Stage 23 polish only **after** the bounded fixes above land and tests pass.
