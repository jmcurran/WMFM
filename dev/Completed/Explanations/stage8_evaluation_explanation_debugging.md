# WMFM Context: Stage 8 – Evaluation and Explanation Debugging

## Context

Stage 7 (Developer Mode Feedback System) is now complete.

The app can:

* generate explanations
* produce sentence-level support maps
* capture developer feedback per sentence
* export structured JSON reports

This stage is focused on **evaluating explanation quality across a structured set of examples**.

---

## Goal

To:

* identify systematic explanation failures
* build a dataset of issues
* prioritise fixes
* improve both deterministic and LLM-driven components

---

## Test Set

The evaluation will use ~19 examples covering:

* Gaussian GLMs
* Logistic GLMs (using Assign as continuous predictor)
* Poisson GLMs

Examples follow the format:

test-XX-ModelSpec

---

## Workflow

For each example:

1. Run the model (Developer Mode ON)
2. Review explanation sentence-by-sentence
3. Mark incorrect sentences
4. Add comments
5. Add general debugging notes if needed
6. Export JSON report

---

## Rules

* Do NOT fix issues immediately unless blocking
* Capture issues first
* Look for patterns across examples

---

## Types of issues

* Hard failures (bugs)
* Soft failures (explanation quality)
* Audit gaps
* UI/communication issues

---

## Expected interaction with ChatGPT

For each example:

* Provide JSON report
* Optionally provide summary

ChatGPT will:

* classify issues
* identify root causes
* recommend fixes
* suggest prioritisation

---

## Output of this stage

* structured understanding of failure modes
* prioritised fix list
* preparation for next implementation stage

---

## First instruction

Please wait for the user to provide the first JSON report before proposing fixes.
