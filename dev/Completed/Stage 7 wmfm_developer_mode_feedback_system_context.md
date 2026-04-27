# WMFM Context: Developer Mode Feedback and Explanation Auditing (New Work Stream)

## Context

You are working on the WMFM R package/app, which:

- fits statistical models
- generates plain-language explanations
- produces a sentence-by-sentence support map
- includes deterministic explanation audit objects

The explanation system is now structurally stable through Stage 6 (cleanup, tagging, answer selection, and UI display fixes).

However, there is currently **no structured way to capture human feedback** on whether the explanation and its sentence-level support are correct.

This limits:

- systematic debugging of explanation quality
- identification of recurring failure modes
- ability to provide high-quality feedback to the LLM
- iterative improvement of deterministic logic

---

## Goal of this work stream

Introduce a **Developer Mode** in the app that allows structured, sentence-level feedback on explanations.

This mode is:

- for developers (not students)
- optional and toggleable
- focused on **diagnosing explanation correctness**

The output of this system will be a **JSON report file** that can be:

- reviewed manually
- shared with ChatGPT for debugging
- used for future automated analysis

---

## High-level design

### Developer mode toggle

Add a developer mode switch (UI-level):

- default: OFF
- when ON:
  - additional controls appear in the explanation panel

---

### Sentence-level feedback UI

For each sentence in the explanation support map:

- display existing information (unchanged):
  - sentence text
  - claim tags
  - support information

- add:

1. Checkbox:
   - label: "Mark as incorrect"
   - default: unchecked

2. Conditional text input:
   - appears only when checkbox is checked
   - label: "Describe the issue"
   - free text

---

### Report generation

At the bottom of the panel:

- button: "Save report to file"

When clicked:

- collect all feedback
- construct a structured object
- export to JSON

---

## JSON report structure

The JSON output should include:

### 1. Top-level metadata

- timestamp
- model type (e.g. lm, glm)
- model formula
- response variable
- predictors

### 2. Context

- research question (if available)
- dataset summary (lightweight)

### 3. Explanation

- full explanation text (cleaned version)

### 4. Sentence-level records

For each sentence:

- sentenceId (index)
- sentenceText
- claimTags
- isMarkedIncorrect (TRUE/FALSE)
- userComment (string or null)

### 5. Optional audit linkage (future-proofing)

- reference to explanationAudit object (if useful)

---

## Design principles

### 1. Strict separation of concerns

- UI collects feedback
- backend builds structured report
- no LLM involvement in this stage

### 2. Deterministic and offline

- no network calls
- reproducible behaviour

### 3. Minimal intrusion

- developer mode must not affect:
  - explanation generation
  - tagging
  - scoring

### 4. Incremental build

This should be implemented in **small stages**, not all at once.

---

## Suggested staged plan

### Stage D1: Developer mode toggle

- add UI toggle
- conditionally render developer controls
- no functionality yet

---

### Stage D2: Sentence-level checkbox

- add "incorrect" checkbox per sentence
- maintain reactive state

---

### Stage D3: Comment capture

- show textbox when checkbox is selected
- store user comments

---

### Stage D4: Data structure assembly

- build internal R object representing feedback
- no file output yet

---

### Stage D5: JSON export

- implement "Save report to file"
- use jsonlite
- ensure clean structure

---

### Stage D6: Validation and tests

- test:
  - correct structure
  - missing comments handled
  - no incorrect entries works

---

## Technical notes

- Use `jsonlite::toJSON(..., auto_unbox = TRUE, pretty = TRUE)`
- Keep IDs stable (sentence order)
- Avoid deeply nested structures

---

## Non-goals

- automated grading of explanations
- modifying explanation content
- LLM feedback loops
- UI redesign beyond developer controls

---

## Why this matters

This system will:

- create a dataset of real explanation failures
- allow targeted improvements to deterministic rules
- support future grading or model-evaluation systems

---

## Suggested chat title

Developer Mode Feedback and Explanation Auditing

---

## First instruction for new chat

Please ask the user to load the latest code base before starting implementation.

