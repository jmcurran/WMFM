# Explaining the Explanation – Stage 3 Redesign (Multi-Tag Sentence Roles)

## Context

I am working on an R package/app called WMFM that fits statistical models and generates plain-language explanations for students.

In previous stages:

* I built a **deterministic explanation audit system** that records:

  * numeric anchors
  * baseline fitted values
  * coefficients and effect translations
  * uncertainty information
  * scale transformations
* This audit is:

  * created during `runModel()`
  * attached to the `wmfmModel` object
  * available in the app

Then I implemented a **student-facing panel**:

> "Where each part of the explanation came from"

This panel:

* splits the explanation into sentences
* maps each sentence to supporting evidence
* assigns a **single claim type** such as:

  * researchQuestion
  * baseline
  * mainEffect
  * uncertainty
  * comparison
  * answer

---

## Problem with Current Approach

The current system forces **each sentence into exactly one category** using deterministic rules.

This is causing increasing problems:

### 1. Sentences are not single-purpose

Many sentences naturally combine roles:

* A sentence may describe:

  * a typical case **and** uncertainty
* Or:

  * an effect **and** a comparison
* Or:

  * a comparison **and** the final answer

### 2. Small wording changes break classification

* Minor rephrasing changes the assigned class
* Deterministic rules become fragile and hard to maintain
* Test failures keep emerging as edge cases expand

### 3. The classification layer is the weak point

What is working well:

* audit system
* evidence inventory
* UI structure
* sentence splitting

What is brittle:

* forcing a **single-label classification**

---

## Goal for This Stage

Redesign the sentence classification system to be:

* more robust
* more faithful to how explanations are written
* still deterministic and testable
* still transparent for students

---

## Proposed Design Shift

### Move from:

**Single label per sentence**

### To:

**Multi-tag sentence roles**

Each sentence can have multiple tags such as:

* researchQuestion
* typicalCase (baseline)
* effect (mainEffect)
* uncertainty
* comparison
* answer

---

## Example

Instead of:

Sentence 3 → "mainEffect"

We might have:

Sentence 3 →

* effect = TRUE
* uncertainty = TRUE

---

## UI Implications

Instead of a single explanation note:

"This sentence explains how the response changes as the predictor increases."

We can show:

* "This sentence describes how the response changes."
* "It also includes uncertainty about this estimate."

This better reflects the actual language.

---

## Requirements

### 1. Deterministic (no LLMs)

* Must still be rule-based
* Must be reproducible and testable

### 2. Backward compatibility (initially)

* Existing pipeline should still run
* Transition path from `claimType` to `claimTags`

### 3. Testability

* Unit tests must validate:

  * tag detection
  * multi-tag combinations
  * edge cases

### 4. Student-facing clarity

* Output must remain:

  * simple
  * non-technical
  * educational

---

## Tasks

### Task 1: Data Structure Redesign

Replace:

* `claimType` (single string)

With:

* `claimTags` (named logical vector or character vector)

Example:

```r
c("effect", "uncertainty")
```

---

### Task 2: Tag Detection Functions

Refactor current helpers into tag detectors:

* `detectResearchQuestion()`
* `detectTypicalCase()`
* `detectEffect()`
* `detectUncertainty()`
* `detectComparison()`
* `detectAnswer()`

Each returns TRUE/FALSE.

---

### Task 3: Mapping Logic

Replace:

```r
classifyExplanationClaimType()
```

With:

```r
detectExplanationClaimTags()
```

---

### Task 4: UI Update

Update:

* `renderExplanationClaimEvidenceUi()`

So it:

* displays multiple roles when present
* keeps wording simple

---

### Task 5: Support Notes

Replace rigid notes with composable ones:

* typicalCase → "describes a typical case"
* effect → "explains how the response changes"
* uncertainty → "shows uncertainty in the estimate"
* answer → "helps answer the research question"

---

### Task 6: Tests

Add tests for:

* multi-tag detection
* overlapping roles
* edge cases (e.g. CI + effect + comparison)

---

## Non-Goals

* Do NOT redesign the audit system
* Do NOT introduce LLM-based classification
* Do NOT expose internal structures directly to students

---

## Key Design Principle

> The goal is not to perfectly interpret language.
> The goal is to **give students a helpful, honest guide** to what each sentence is doing.

---

## Deliverables

* Updated R functions (downloadable)
* Updated tests
* Stage script
* Git commit messages

---

## Suggested Chat Title

"Explaining the Explanation Stage 3 Redesign – Multi-Tag System"
