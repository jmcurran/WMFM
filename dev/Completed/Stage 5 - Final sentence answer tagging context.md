# Stage 5 Context: Ensure the final sentence explicitly answers the research question

## Context

I am working on an R package/app called WMFM that fits statistical models and generates plain-language explanations for students.

Stages 4.1 to 4.6 redesigned the explanation-evidence system so that:

- explanation sentences are split deterministically
- each sentence can carry multiple roles via `claimTags`
- student-facing role notes are stored in `supportNotes`
- the UI now shows multiple sentence roles when appropriate
- backward compatibility with legacy `claimType` was preserved

This work is now complete.

## New issue discovered after Stage 4

During manual testing of the student-facing explanation-evidence panel, I found a consistent weakness:

> The final sentence of the explanation is intended, by prompt design, to be an attempt to answer the research question.
> But the current deterministic tagging system does not reliably mark the final sentence as an `answer`.

In other words:

- the prompt explicitly instructs the model that the final sentence should try to answer the research question
- but the deterministic sentence-role logic is still trying to infer `answer` mostly from wording
- so closing takeaway sentences are sometimes tagged only as `effect`, `comparison`, or other roles
- this causes the student-facing panel to miss an important teaching signal

## Concrete example

A final sentence such as:

> On average, the model shows that students who attend class regularly and score higher on the mid-term test tend to receive higher final exam marks; the evidence is about averages and does not predict individual outcomes.

is currently being treated mainly as an effect sentence, even though it is clearly functioning as the closing attempt to answer the research question.

This is especially problematic because the final sentence position is not incidental here:
it is part of the generation contract.

## Key design decision for Stage 5

The final sentence should not have to earn the `answer` role from phrase matching alone.

Instead, sentence position should be part of the deterministic logic.

That suggests a rule like:

- if a sentence is the final sentence in a non-empty explanation, it should receive the `answer` tag
- it may also retain other tags such as `effect`, `comparison`, or `uncertainty`
- the student-facing notes for the final sentence should include a plain-language note that it helps answer the research question

This should be treated as a deterministic positional rule, not only a wording heuristic.

## Goal for this stage

Strengthen the sentence-role system so that the final sentence reliably carries the `answer` role in line with the explanation prompt contract.

This should improve:

- fidelity to the intended explanation structure
- student-facing clarity in the evidence panel
- robustness against wording variation in closing sentences

## Requirements

### 1. Deterministic

- no LLM-based classification
- rule-based and reproducible
- easy to test

### 2. Preserve multi-tag behavior

The final sentence may still also be:

- `effect`
- `comparison`
- `uncertainty`
- or other appropriate roles

So this stage should add or guarantee `answer`, not replace the other valid tags.

### 3. Student-facing clarity

The UI should clearly communicate that the final sentence helps answer the research question.

### 4. Backward compatibility

- keep `claimTags` as the primary representation
- keep derived `claimType` compatibility behavior where it still matters
- avoid unnecessary redesign of the completed Stage 4 system

### 5. Keep scope tight

This stage is only about the final-sentence `answer` rule and any necessary supporting tests/UI notes.
Do not reopen the full multi-tag redesign.

## Proposed tasks

### Task 5.1: Review current final-sentence answer logic

Inspect the current detector path and confirm:

- where `answer` is currently being assigned
- whether answer detection is still too dependent on wording
- how final sentence position is already represented, if at all

Deliverable:
- targeted code changes only if needed
- no broad redesign

### Task 5.2: Add deterministic final-sentence answer tagging

Update the tagging logic so that:

- the final sentence in a non-empty explanation always receives `answer`
- other valid tags are preserved
- empty or malformed edge cases are handled safely

This is the core logic change for Stage 5.

### Task 5.3: Decide compatibility behavior for derived `claimType`

Because `claimType` is still retained for backward compatibility, decide how it should behave when the final sentence has both:

- `answer`
- and one or more other tags such as `effect`

The decision should be explicit and tested.

### Task 5.4: Update student-facing support notes or UI wording if needed

Ensure the final sentence visibly includes the idea that it:

- helps answer the research question

This may require either:

- note generation updates
- UI rendering updates
- or both

But keep the change minimal and consistent with the completed Stage 4 design.

### Task 5.5: Add targeted tests

Add deterministic offline tests for cases such as:

- final sentence receives `answer` even without explicit summary wording
- final sentence can carry `answer + effect`
- final sentence can carry `answer + comparison`
- final sentence can carry `answer + uncertainty`
- earlier sentences do not gain `answer` just because they contain similar language
- final sentence handling behaves sensibly for one-sentence explanations

### Task 5.6: Manual check examples

Add or document a few compact examples that can be used for manual testing in the app, especially where the final sentence:

- is clearly a closing takeaway
- lacks phrases like "in summary" or "overall"
- still ought to be marked as answering the research question

## Non-goals

- do not redesign the audit system
- do not redesign the full Stage 4 multi-tag framework
- do not introduce LLM-based sentence classification
- do not broaden this into general prompt redesign unless strictly necessary

## Suggested workflow expectations

As in the previous stages:

- provide downloadable change-set zip files
- provide a stage script for each step
- keep stage numbering as:
  - Stage 5.1
  - Stage 5.2
  - Stage 5.3
  - etc.
- preserve commit messages inside the generated WMFM stage scripts
- continue using the user's R development style and WMFM stage-script workflow

## Suggested chat title

Final sentence answer tagging - Stage 5
