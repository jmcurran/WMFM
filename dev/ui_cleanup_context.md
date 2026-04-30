# WMFM context: UI cleanup and consistency pass

## Purpose

This work stream focuses on **small, targeted UI improvements** across the WMFM app.

The goal is not to add new features, but to:

> Improve clarity, consistency, and polish of the existing interface.

This is a **low-risk, high-impact refinement stage**.

---

## Background

Recent stages have introduced substantial backend improvements:

- deterministic explanation pipeline
- audit objects
- diagnostics
- grading and scoring infrastructure
- improved explanation structure

However, the UI may now:

- expose outdated wording
- present inconsistent terminology
- contain minor layout or clarity issues
- not fully reflect new backend capabilities

---

## Guiding principles

- Do not redesign the UI
- Do not introduce major new features
- Prefer small, safe, incremental improvements
- Maintain backward compatibility
- Keep changes easy to test manually

---

## Core areas to review

### 1. Terminology consistency

Check for consistent use of:

- “model explanation”
- “teaching summary”
- “confidence interval”
- “effect” vs “change”
- “odds” vs “probability”

Avoid:

- mixed terminology for the same concept
- overly technical language for student-facing text

---

### 2. Label clarity

Review:

- tab names
- section headings
- button labels
- tooltips

Questions:

- Is the purpose immediately clear?
- Would a student understand this without instruction?

---

### 3. Output readability

Check:

- spacing between sections
- alignment of tables
- line breaks in explanations
- readability of long text blocks

Look for:

- dense or hard-to-read output
- inconsistent formatting between components

---

### 4. Consistency across tabs

Ensure alignment between:

- Model tab
- Explanation tab
- Confidence interval tab
- Any diagnostic or grading outputs

Focus on:

- consistent ordering of information
- consistent naming of quantities
- consistent presentation of numbers

---

### 5. Handling of edge cases

Check UI behaviour when:

- models fail to fit
- data is missing
- explanations are unavailable
- grading is not applicable

Ensure:

- clear, helpful messages
- no cryptic errors shown to users

---

### 6. Integration of new backend features

Review whether UI reflects:

- explanation audit (if surfaced)
- diagnostics (if surfaced)
- grading outputs (if visible)

Avoid exposing raw internal objects directly.

---

## Suggested workflow

### Step 1: UI walkthrough

- manually run several models
- note friction points
- capture screenshots if useful

### Step 2: classify issues

Group into:

- wording issues
- layout issues
- consistency issues
- missing feedback/messages

### Step 3: implement small fixes

- keep each fix minimal
- avoid bundling unrelated changes
- prefer iterative commits

---

## Coding considerations

- follow existing Shiny/app structure
- keep server logic unchanged where possible
- avoid touching model or explanation pipeline
- update only UI-layer code unless necessary

---

## Testing approach

- manual testing is primary
- ensure no regressions in:
  - model fitting
  - explanation generation
  - grading/scoring

Optional:

- snapshot tests for UI text if useful

---

## Non-goals

- no redesign of layout
- no new major UI components
- no new modelling features
- no changes to statistical logic

---

## Examples of acceptable changes

- rename confusing labels
- improve tooltip wording
- fix spacing or alignment
- standardise terminology
- improve error messages

---

## Next stage goal

Produce a clean, consistent UI with:

- clear terminology
- readable outputs
- minimal friction for users

without introducing new complexity.
