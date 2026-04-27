# WMFM Context: Explanation Surface Post-Processing (Stage 4 Candidate)

## Context

In the current WMFM workflow:

- Stage 1: Deterministic explanation audit (complete)
- Stage 2: Student-facing teaching summary (complete)
- Stage 3: Claim-to-evidence mapping (complete)

We have identified a new class of issues in the generated explanations that are **not about correctness**, but about **surface quality, readability, and pedagogical clarity**.

These include:

- Undesirable statistical phrasing (e.g. "linear model", "interaction term")
- Awkward or unnatural phrasing (e.g. "one-magnitude rise")
- Use of forbidden numeric language (e.g. "one-third", "three-quarters")
- Overly long sentences combining multiple ideas
- Poor pedagogical flow
- Leakage of model-mechanism language (e.g. "coefficient", "intercept")

These issues are:

- systematic
- predictable
- not reliably controlled via prompt engineering

They should not be solved inside the claim-to-evidence mapping layer.

---

## Goal

Introduce a deterministic post-processing layer for explanation text that improves:

- readability
- pedagogical clarity
- sentence structure
- consistency of phrasing
- adherence to language rules

This should operate:

- after explanation generation
- before display

---

## Key Design Principle

This follows the established WMFM philosophy:

- Numeric correctness was moved out of the LLM into deterministic logic
- Now, surface language and phrasing control should also move out of the LLM

Do not rely solely on prompt engineering for enforcing:

- phrasing rules
- numeric expression rules
- pedagogical language constraints

---

## Proposed Function

postProcessExplanationText = function(text, audit) { ... }

Inputs:
- text: raw explanation text from the LLM
- audit: explanation audit object (optional use)

Output:
- cleaned, structured, student-friendly explanation text

---

## Transformation Categories

### 1. Remove model-mechanism language

Students should not be exposed to internal model structure.

Forbidden phrases include:

- linear model
- regression model
- fitted model
- interaction term
- coefficient
- intercept
- slope
- parameter

Strategy:

- remove phrase, or
- replace with observable behaviour

Example:

Before:
"The interaction term makes the drop steeper in Washington."

After:
"The decrease with magnitude is steeper in Washington than in California."

---

### 2. Fix unit-change phrasing

Undesirable patterns:

- one-magnitude rise
- one-unit increase in X (context-dependent)
- a one X increase

Replace with:

- an increase of one in magnitude
- if magnitude increases by one
- for each one-unit increase in magnitude

Example:

Before:
"A one-magnitude rise multiplies the expected count..."

After:
"If the magnitude increases by one, the expected count is multiplied by..."

---

### 3. Remove verbal fractions

Forbidden:

- one-third
- two-thirds
- three-quarters
- half (in numeric contexts)

Replace with:

- percentages (preferred), e.g. about 33%, about 75%
- or decimals if appropriate

Example:

Before:
"...only one-third of what it was before."

After:
"...only about 33% of what it was before."

---

### 4. Split long sentences

Separate combined ideas such as:

- anchor description
- prediction
- uncertainty

Example:

Before:
"For a student whose score is..., the model predicts..., with values between..."

After:
"For a student with ..., the expected value is .... This estimate could plausibly lie between ... and ...."

---

### 5. Improve sentence openings

Avoid subordinate-first constructions.

Undesirable:
"In a model that predicts..., the relationship is..."

Preferred:
"The relationship between the variables is..."

---

### 6. Preserve meaning and numbers

This layer must:

- not alter numeric values
- not change statistical meaning
- not introduce new claims
- only restructure wording

---

## Implementation Strategy

Deterministic rules only:

- use regex-based transformations
- avoid LLM rewriting
- apply rules sequentially

Local transformations:

- prefer small, safe substitutions
- avoid complex sentence rewriting
- accept slightly repetitive phrasing over risk

Transformation table:

forbiddenPattern                replacementStrategy
---------------------------------------------------
one-magnitude rise             increase of one in magnitude
one-third                      about 33%
three-quarters                 about 75%
interaction term               remove and rephrase sentence
coefficient                    remove or replace with "change in"

---

## Testing Requirements

Tests must verify:

- forbidden phrases are removed
- unit-change phrasing is corrected
- verbal fractions are eliminated
- long sentences are split
- numeric values remain unchanged
- output is deterministic

Constraints:

- tests run offline
- no LLM calls

---

## Relationship to Existing Stages

This is a new stage:

- Stage 1: Audit (correctness)
- Stage 2: Teaching summary (structure)
- Stage 3: Claim mapping (traceability)
- Stage 4: Explanation surface control (this stage)

This stage:

- does not modify audit data
- does not affect claim mapping
- only improves final rendered text

---

## Key Insight

The LLM is:

- good at generating meaning
- unreliable at enforcing strict language rules

Therefore:

Surface-level language control must be deterministic.

---

## Future Extensions

- readability scoring
- sentence-length heuristics
- configurable verbosity levels
- UI display modes (simple vs detailed)

---

## Summary

We introduce a deterministic post-processing layer that:

- improves readability and pedagogy
- enforces language rules
- removes technical leakage
- standardises phrasing

This aligns with the WMFM philosophy:

move critical behaviour out of the LLM and into deterministic, testable code
