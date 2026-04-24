# Stage 1 audit inventory and gap report

## Context alignment

The attached rebuild brief assumes the audit trail may need to be rebuilt from missing branch history. After inspecting the current codebase, the explanation-audit layer is already present in substantial form, and later-stage pieces from the old branch are also present.

This means the right next step is not a blind rebuild. It is a verification-and-contract pass followed by targeted repair only where the current implementation is thinner or less stable than the rebuild brief expects.

## What already exists in the current codebase

### 1. Core deterministic audit builder exists

File:
- `R/model-explanationAudit.R`

Implemented pieces include:
- `buildModelExplanationAudit()`
- coefficient-table construction
- overview section
- prompt-input metadata section
- prompt-rule summary section
- interpretation-scale section
- numeric-anchor section
- reference-level section
- confidence-interval section
- baseline-evidence table
- effect-evidence table
- raw prompt ingredient capture

The audit object is classed as `wmfmExplanationAudit`.

### 2. Audit attachment to model output already exists

File:
- `R/api-runModel.R`
- `R/class-wmfmModel.R`

Observed behavior:
- `runModel()` builds `explanationAudit = buildModelExplanationAudit(model = model)`
- the returned `wmfmModel` stores `explanationAudit`
- the `wmfmModel` constructor already has an `explanationAudit` field

### 3. Runtime/app audit access already exists

Files:
- `R/app-equation-runtime.R`
- `R/app-server.R`

Observed behavior:
- app runtime has `buildAppExplanationAudit()`
- app model-output builder includes `explanationAudit`
- server logic stores the audit in runtime values

### 4. Developer print support already exists

File:
- `R/methods-print-wmfmExplanationAudit.R`

Observed behavior:
- a print method exists
- it currently prints:
  - transparency note
  - interpretation scale
  - numeric anchors
  - reference levels

### 5. Deterministic tests already exist

Files:
- `tests/testthat/test-buildModelExplanationAudit.R`
- `tests/testthat/test-explanation-audit-runtime.R`

Observed coverage includes:
- audit object class
- transparency note presence
- research-question flag usage
- numeric-anchor table presence
- reference-level table presence
- baseline evidence presence
- coefficient-table presence
- logistic-model interpretation-scale checks
- app runtime audit creation without chat access
- `runModel()` storage of audit on `wmfmModel`

### 6. Later-stream pieces are already present too

Files:
- `R/model-explanationTeachingSummary.R`
- `R/app-explanationAudit-uiHelpers.R`
- `R/app-explanationTutor.R`
- `R/model-explanationClaimEvidence.R`
- `R/model-explanationClaimTagDetectors.R`
- related tests

This is important because the rebuild brief treats teaching summary and student-facing UI as later streams, but the current codebase already contains those layers.

## Main conclusion from the inventory

The codebase appears to have already absorbed much of the lost-branch functionality.

So the practical question is no longer:
- "How do we rebuild the explanation audit from scratch?"

It is now:
- "Is the current audit contract complete, stable, and aligned with the intended deterministic foundation?"

## Gaps and risks still worth addressing

### Gap 1. No explicit written audit contract in code

The brief calls for a contract-level definition of what the audit must contain and what invariants must hold.

The implementation already implies a contract, but it is not yet frozen in one obvious place. That creates a risk that later UI or teaching-summary work starts depending on fields informally.

Recommended next step:
- define and document the canonical audit sections and invariants before changing code further

### Gap 2. Print support is present but shallow

The print method only surfaces a subset of the audit.

It does not currently summarize:
- overview
- prompt inputs
- confidence-interval metadata
- baseline/effect evidence counts
- raw prompt ingredient availability

This is not a core logic bug, but it is weaker than the rebuild brief's developer-inspection goal.

Recommended next step:
- review whether `print.wmfmExplanationAudit()` should become a fuller but still stable inspection view

### Gap 3. Tests are present but still fairly shape-oriented

Existing tests confirm the audit exists and contains several expected pieces, which is good. But they are still relatively light on contract-level invariants.

Potential missing tests:
- exact required top-level section names
- stronger checks on the structure of `overview`
- stronger checks on `promptInputs`
- stronger checks on `confidenceIntervals`
- stronger checks on numeric-anchor semantics when zero is in range versus out of range
- stronger checks on reference-level semantics for factor predictors
- stronger checks that audit generation stays offline and deterministic across supported model families

Recommended next step:
- add contract tests after the contract is frozen

### Gap 4. App runtime may duplicate access paths rather than treat model storage as the single source of truth

The codebase has both:
- audit storage on `wmfmModel`
- app-side helper `buildAppExplanationAudit(model = model)`

That is not necessarily wrong, but it raises a design question from the brief:
- should the audit always be built once in `runModel()` and then reused?
- or is app-side recomputation still intentional and safe?

Recommended next step:
- inspect whether any runtime path recomputes the audit unnecessarily instead of using the model-attached version when available

### Gap 5. Later layers may already depend on the current audit shape

Because teaching summary, tutor, and claim-evidence layers already exist, changing the audit shape now could have ripple effects.

Recommended next step:
- freeze the audit contract first
- then check downstream dependencies before any refactor of field names or structure

## Suggested refined execution plan

### Stage 1A. Freeze the current audit contract

Document the audit object as it should exist now:
- required top-level fields
- required nested fields
- invariants
- deterministic guarantees
- what is developer-facing versus student-facing

### Stage 1B. Dependency map

Trace which files consume which audit fields, especially:
- teaching summary
- claim-evidence map
- tutor helper
- app UI helpers
- print methods
- tests

### Stage 1C. Tightening pass

Only after the contract is frozen:
- improve print support if needed
- remove redundant recomputation if present
- strengthen tests to match the contract

## Recommended immediate next move

Do not start rebuilding the audit builder yet.

Instead, the next high-value task is:
- define the audit contract explicitly from the current implementation
- identify downstream consumers of each field
- then decide whether any field set needs repair, expansion, or pruning

## Bottom line

The current codebase already contains:
- deterministic audit construction
- attachment to `wmfmModel`
- runtime access
- print support
- tests
- teaching-summary and UI layers built on top

So this context should now be treated as:
- verification and stabilization of an existing audit subsystem

rather than:
- recovery of a missing subsystem from scratch
