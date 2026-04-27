# WMFM Context: Explanation cleanup and answer selection refinement (Stage 6)

## Context

I am working on an R package/app called WMFM that generates plain-language explanations of fitted statistical models and provides a sentence-by-sentence support map.

The explanation system is complete through Stage 5.7.1, including:

- deterministic sentence splitting
- multi-tag sentence roles via `claimTags`
- a structural rule for assigning the `answer` role (last substantive conclusion)
- student-facing support notes and UI panels

During manual checks, a remaining issue has been identified:

> The LLM sometimes inserts header-like tokens such as "Answer", "Answer:", or similar at the start of sentences.

These are formatting artifacts, not reliable semantic signals, and they interfere with deterministic logic for:

- sentence splitting
- role detection
- answer selection
- UI display

## Goal for this stage

Introduce a deterministic cleanup step that removes LLM formatting leakage (e.g. "Answer" prefixes) before sentence processing, and ensure that:

- sentence tagging remains structural and deterministic
- `answer` selection does not depend on leaked tokens
- displayed explanations are clean and student-friendly

This stage should be tight in scope and not reopen the broader tagging system.

## Key design principle

Separate concerns:

1. Surface cleanup (new in this stage)
   - remove formatting artifacts introduced by the LLM
   - purely string-level, deterministic transformations

2. Structural tagging (already implemented)
   - assign roles based on sentence meaning and position
   - do not rely on tokens like "Answer"

Clean first, then analyse.

## Problem examples

### Example 1

"Answer Therefore, on average, students ..."

Issues:
- "Answer" is not meaningful content
- it disrupts natural reading
- it may mislead tagging logic

### Example 2

Cases where:
- a true answer sentence exists earlier
- a later disclaimer appears
- the system must choose the correct sentence as `answer` independent of any leaked tokens

## Requirements

### 1. Deterministic cleanup

Implement a function such as:

cleanExplanationText = function(text) { ... }

It should:

- remove leading tokens like:
  - "Answer"
  - "Answer:"
  - "Answer -"
- handle case variations:
  - "answer", "ANSWER"
- preserve the rest of the sentence

Examples:

- "Answer Therefore, ..." -> "Therefore, ..."
- "Answer: On average, ..." -> "On average, ..."

### 2. Placement in pipeline

The cleanup must occur:

- after LLM generation
- before sentence splitting
- before claim tagging
- before support mapping

Do not scatter cleanup logic across multiple functions.

### 3. Do not rely on "Answer" for tagging

After cleanup:

- `answer` role must be determined by:
  - structural rules (Stage 5.7)
  - sentence content (effect/comparison/uncertainty)
- never by presence of "Answer" token

### 4. Preserve meaning

Cleanup must:

- not alter numeric values
- not change statistical meaning
- only remove formatting artifacts

### 5. Keep scope tight

Do not:

- redesign sentence tagging
- modify audit logic
- introduce LLM-based rewriting
- expand into full post-processing layer yet

## Suggested tasks

### Task 5.7.2.1: Implement cleanup function

- Create cleanExplanationText()
- Add unit tests for:
  - removal of "Answer" variants
  - preservation of normal sentences
  - no unintended string changes

### Task 5.7.2.2: Integrate into pipeline

- Identify where explanation text first enters deterministic processing
- Apply cleanup once at that boundary

### Task 5.7.2.3: Verify interaction with tagging

- Confirm:
  - answer selection still works correctly
  - disclaimer cases still behave correctly
  - no regression in existing tests

### Task 5.7.2.4: Minimal UI check

- Ensure displayed sentences no longer show "Answer"
- No UI redesign required

## Testing requirements

Add deterministic offline tests for:

- "Answer" prefix removal
- "Answer:" prefix removal
- mixed-case variants
- sentences that should remain unchanged
- end-to-end behaviour:
  - cleaned text flows into sentence splitting correctly

## Non-goals

- full explanation surface post-processing (separate future stage)
- readability improvements beyond artifact removal
- new tagging categories
- performance optimisation

## Summary

This stage introduces a clean separation:

- remove LLM formatting leakage deterministically
- keep explanation analysis purely structural

This improves:

- robustness of tagging
- clarity of UI output
- reliability of downstream logic

## Suggested chat title

Explanation cleanup and answer selection refinement (Stage 6)

## Numbering

I recognise that the instructions in this stage number things like 5.7.2.1, 5.7.2.2 and so on. I want to 
make this Stage 6, and so the tasks should just be 6.1, 6.2, ... etc. We will move to a third tier of numbering
if there are tests to correct or bugs to fix


## First instruction for new chat

Please ask the user to load the latest code base before starting implementation.
