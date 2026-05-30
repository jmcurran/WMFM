# WMFM Deferred Follow-Up Work Context

## Title

GLM Follow-Up Enhancements (Post Stage 25)

## Background

Stage 25 (GLM Follow-Ups) has been completed, tested, merged into `master`, and released.

The completed work includes:

- Developer-only GLM follow-up examples
- Logistic probability follow-ups
- Logistic odds follow-ups
- Poisson expected-count follow-ups
- Natural-language follow-up parsing improvements
- Deterministic predictor resolution
- Response-scale confidence intervals
- Diagnostics JSON enhancements
- `generatedExplanation` included in diagnostics
- Prediction-request classification fixes
- In-range Poisson examples
- Manual smoke testing
- Stabilisation and regression coverage

This context describes work intentionally deferred from Stage 25.

---

## Project Conventions

Continue to follow:

- WMFM stage workflow
- Incremental stage numbering
- `=` assignment
- camelCase identifiers
- braces on all control structures
- roxygen2 documentation
- `@importFrom` usage
- modular helpers
- deterministic testing
- offline-safe tests
- thin orchestration layers

Use:

- r-development-style skill
- wmfm-stage-script-generator skill

---

# Deferred Workstream 1
# True GLM Prediction Intervals

## Motivation

Current follow-ups provide:

- fitted mean response
- confidence interval for the fitted mean

This is appropriate and statistically valid.

However students frequently ask questions that imply uncertainty for a future observation.

Examples:

### Logistic

"What is the probability that a student with Assign = 15 passes?"

Current:

- fitted probability
- CI for fitted probability

Potential future enhancement:

- uncertainty for a future Bernoulli outcome

### Poisson

"How many earthquakes would you expect at Magnitude = 5.4?"

Current:

- fitted expected count
- CI for fitted expected count

Potential future enhancement:

- prediction interval for a future count

## Goals

Investigate:

- statistical validity
- interpretation
- teaching usefulness
- implementation options

## Requirements

Any implementation must:

- clearly distinguish confidence intervals from prediction intervals
- remain deterministic
- expose diagnostics metadata
- include automated tests

---

# Deferred Workstream 2
# Extrapolation and Range Policies

## Motivation

Stage 25 introduced basic range-awareness.

Future work should formalise extrapolation handling.

Examples:

### In-range

Magnitude = 5.4

### Slight extrapolation

Magnitude = 5.2

### Extreme extrapolation

Magnitude = 3

## Possible Behaviour

### Level 1

Normal prediction

### Level 2

Prediction plus warning

### Level 3

Prediction suppressed

Status returned:

- extrapolation_warning
- extrapolation_blocked

## Requirements

Diagnostics should include:

- observed range
- requested value
- extrapolation classification
- explanatory text

---

# Deferred Workstream 3
# Expanded Natural-Language Parsing

## Motivation

Stage 25 improved parsing but only covers a subset of realistic student questions.

Examples to support:

- "What would you predict for a student who scored 15?"
- "What happens if attendance is yes?"
- "How many earthquakes would you expect in Washington at magnitude 5.6?"
- "What are the odds of passing for someone with a test mark of 10?"

## Goals

Increase robustness without introducing ambiguity.

## Requirements

- deterministic extraction
- clear clarification pathway
- factor-level awareness
- comprehensive tests

---

# Deferred Workstream 4
# Test and Example Naming Cleanup

## Motivation

Some files currently reflect stage history rather than behaviour.

Example:

- `test-stage25-3-glm-followups.R`
- `test-stage25-6-glm-odds-followups.R`

Preferred:

- `test-glm-followup-predictions.R`
- `test-glm-followup-odds-predictions.R`

## Goals

Make the test suite self-documenting.

## Requirements

- behaviour-oriented names
- preserve coverage
- no functional changes

---

# Suggested Priority

1. Extrapolation and Range Policies
2. Expanded Natural-Language Parsing
3. True GLM Prediction Intervals
4. Test and Example Naming Cleanup

## Suggested Branch

`glm-followup-enhancements`

## Suggested Starting Stage

Stage 26
