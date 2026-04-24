# Stage 4.6.5 design note: teaching-summary recovery audit

## Purpose of this stage

This stage inspects the attached Stage 4.6.4 code snapshot against the teaching-summary rebuild context and recovers the intended feature behaviour in current-architecture terms.

The attached rebuild context says the old `explaining-explanations` branch should be treated as design evidence rather than merged directly, and that the goal is a student-facing teaching summary that explains how to read the generated model explanation in plain language. The same context also lays out the six rebuild stages that now need to be renumbered from 4.6.5 onward.

## Main finding

The attached `stage4.6.4_completed.zip` snapshot already contains a substantial teaching-summary implementation in the current architecture.

This means the safe interpretation is not "the feature is completely missing". Instead, the current code base appears to already contain the rebuilt feature, at least in one recent snapshot. The immediate task is therefore to verify scope, contract, and fit, then decide whether the user's current working tree is missing these pieces or whether later drift has partially displaced them.

## What is present in the snapshot

### Core builder

The file `R/model-explanationTeachingSummary.R` defines a deterministic builder:

- `buildExplanationTeachingSummary(audit, model, researchQuestion = NULL)`

It returns a `wmfmExplanationTeachingSummary` object with these fields:

- `dataDescription`
- `interpretationScale`
- `baselineChoice`
- `xChangeDescription`
- `mainEffectDescription`
- `uncertaintySummary`
- `evidenceTable`
- `researchQuestionLink`

The builder consumes:

- the deterministic explanation audit
- the fitted model
- an optional research question, with fallback to `attr(model, "wmfm_research_question")`

This is fully aligned with the rebuild context's design guardrail that the teaching summary should consume current explanation-oriented information rather than recreate a second explanation-analysis system.

### UI rendering helpers

The snapshot includes student-facing UI rendering helpers. The app calls:

- `renderExplanationTeachingSummaryUi(teachingSummary)`
- `renderTeachingSummaryText(...)`

This indicates the teaching summary is rendered as a structured UI section rather than just dumped as raw text.

### App integration

The snapshot wires the teaching summary into `R/app-server.R`.

The explanation area:

- still shows the main model explanation when present
- shows the heading `How this explanation was constructed`
- renders the teaching summary below the explanation
- optionally shows claim-evidence UI after that
- then offers a separate accordion for an optional tutor-style AI explanation

This ordering is educationally sensible:

1. deterministic explanation
2. deterministic teaching summary
3. deterministic claim-evidence trace
4. optional conversational AI helper

### Optional tutor-style layer

The file `R/app-explanationTutor.R` does not replace the teaching summary. Instead it builds a constrained AI prompt from the already-constructed teaching summary.

That is an important architectural decision because it keeps the student-facing explanatory core deterministic and uses AI only as an optional elaboration layer.

### Tests

The snapshot includes dedicated tests such as:

- `test-buildExplanationTeachingSummary.R`
- app UI tests that reference the teaching summary and tutor ordering

The tests confirm that the intended contract is stable and student-facing, and that the tutor-style explanation is downstream of the deterministic summary rather than upstream of it.

## Recovered intended behaviour

Based on the snapshot, the intended teaching-summary feature in current terms is:

- Build a deterministic, student-facing explanation companion from the explanation audit and model metadata.
- Explain what data are being used, what interpretation scale is being used, what starting point or baseline is being used, what change is being described, what the main fitted claim is, and how uncertainty is being expressed.
- Keep the wording plain-language and educational rather than developer-facing.
- Connect the explanation back to the research question when one is available.
- Present a compact evidence table so students can see the main ingredients used to construct the explanation without exposing raw internals.
- Keep any optional AI tutor layer constrained by the deterministic teaching summary instead of letting it invent new reasoning.

## Current data contract recovered from the snapshot

The recovered contract for the deterministic teaching-summary generator is:

### Inputs

Required:

- `audit`: a deterministic explanation audit object
- `model`: a fitted `lm` or `glm` object

Optional:

- `researchQuestion`: a length-1 character string, otherwise taken from model attributes when available

Additional model-derived context:

- response name from the model formula or model frame
- predictor names split into numeric and factor predictors
- response noun phrase from model attributes when available

### Output

An object of class `wmfmExplanationTeachingSummary` with:

- six scalar text fields for the main teaching narrative
- an `evidenceTable` data frame with sections and summaries
- a `researchQuestionLink` text field

### Design properties

- deterministic
- plain language
- suitable for direct app display
- safe to pass into later UI helpers
- safe to use as the grounding source for optional AI tutor text

## Important architectural implications

1. The teaching summary is not merely a UI decoration.
   It is a structured, model-derived explanatory object.

2. The explanation audit is the main deterministic source of truth.
   This is consistent with the user's note that the earlier audit work is now complete.

3. The optional AI tutor layer already follows the right dependency direction.
   It depends on the deterministic teaching summary, not the other way around.

4. Claim-evidence features and teaching-summary features are related but distinct.
   The teaching summary helps students understand the explanation at a high level, whereas the claim-evidence layer traces where parts of the explanation came from.

## Risks still worth checking in the user's real working tree

Because the attached snapshot already contains the feature, the remaining risks are mostly integration risks rather than pure implementation risks:

- the user's current branch may not actually include the same files as the snapshot
- later edits may have broken or displaced the UI section
- later edits may have changed the explanation audit contract in ways the teaching-summary builder no longer matches
- tutor-style features may still be present even if the deterministic panel has drifted
- old claim-evidence dependencies may still need cleanup or clearer separation

## Recommendation for the next stage

Proceed to Stage 4.6.6 as a verification-and-contract stage rather than a greenfield design stage.

Specifically:

- compare the user's current working tree against the Stage 4.6.4 snapshot
- verify whether the teaching-summary files are present now
- confirm the current `wmfmModel` and audit contract still match the recovered builder inputs
- identify any drift between current app wiring and the recovered design
- only after that decide whether 4.6.7 should be a small repair stage or a deeper rewrite stage

## Practical interpretation

At this point, the safest reading is:

- the rebuild was likely already done in a recent code state
- the immediate job is to recover, verify, and if necessary re-land that work cleanly in the user's current branch
- there is no evidence from this snapshot that the correct next action is to recreate the feature from scratch
