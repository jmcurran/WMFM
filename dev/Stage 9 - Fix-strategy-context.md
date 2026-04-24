# WMFM Context: Stage 9 - Explanation Debugging Fix Strategy

## Context

Stage 8 is complete.

Stage 8 evaluated 19 developer-feedback JSON reports from the WMFM app's explanation system. The examples covered:

- Gaussian linear models
- Logistic binomial GLMs
- Poisson GLMs
- Intercept-only models
- Numeric predictors
- Factor predictors
- Additive models
- Interaction models

The evaluation deliberately did not implement fixes. It focused on classifying explanation failures, identifying root causes, and looking for systematic patterns across examples.

## Goal of Stage 9

Stage 9 is the fix-strategy stage.

The goal is to turn the Stage 8 findings into an implementation plan that separates:

- deterministic fixes
- prompt fixes
- tagging/schema fixes
- UI/communication fixes
- later-stage or lower-priority polish

Stage 9 should not start by editing code. It should first decide what should be fixed, in what order, and which fixes belong together.

## Current code base

Use the latest uploaded WMFM code base for any code inspection or implementation planning.

The project is an R package/app. Follow the user's standing R development conventions:

- use `=` for assignment
- use camelCase identifiers
- use braces for all control structures
- use styleR-style formatting
- prefer `|>` over magrittr
- prefer roxygen2 documentation
- prefer `@importFrom` rather than `pkg::fun()` where appropriate
- do not manually edit `NAMESPACE`
- tests must be deterministic and offline
- do not make real LLM calls in tests
- use fake providers, mocks, or existing helpers for LLM boundaries
- provide replacement files or zips rather than long copy-paste patches when changes are substantial
- provide plain-text ASCII git commit messages for implemented changes

Use the user's WMFM stage-script workflow if implementation scripts are requested.

## Stage 8 key conclusion

The system has strong statistical capability but weak control systems.

The explanations can be excellent, especially in later Poisson examples, but the behaviour is inconsistent. The main problems are not usually basic statistical reasoning. They are mostly in:

- tagging
- schema coverage
- explanation scale control
- anchor choice
- narrative sequencing
- plain-language translation
- numeric formatting
- avoiding technical leakage

## Critical Stage 8 findings

### Tier 1: Critical systemic issues

1. Tagging system instability

The most pervasive issue across the evaluation.

Observed failures included:

- research-question restatements not tagged as researchQuestion
- answer sentences not tagged as answer
- partial answers tagged as full answers
- answers tagged as researchQuestion
- uncertainty sentences not tagged as uncertainty
- group comparisons not tagged as comparison
- typical-value statements missed when combined with CIs
- statistical disclaimers not recognised
- model-constraint sentences not recognised
- supporting context misclassified as comparison or effect

The system appears too surface-form dependent and the schema appears incomplete.

2. GLM scale-handling inconsistency

The explanation sometimes exposed model-scale details that students should not need to see:

- log-odds
- log expected count
- intercepts
- exponentiating coefficients
- raw coefficient values
- transformation mechanics

The preferred explanation scale should usually be the interpretable response scale:

- probability or odds, depending on the research question and UI context, for logistic models
- expected counts or multiplicative count changes for Poisson models

3. Model-awareness gaps

Repeated failures occurred when the explanation did not adapt to the actual model structure:

- intercept-only models described as having predictors
- baseline/reference language used when no predictors existed
- R-squared language used in the intercept-only LM case
- reference-category interpretations sometimes unclear
- interaction language sometimes over-technical or confused

4. Anchor selection problems

For numeric predictors, anchors were sometimes mathematically valid but pedagogically poor.

Repeated issue:

- using 0 for assignment mark because 0 is in range, even though a mean or meaningful midpoint such as 10 would be much more interpretable

Good counterexample:

- Poisson magnitude example used average magnitude 6.25 and was much clearer

### Tier 2: High-impact explanation issues

5. Interaction explanation instability

Interaction explanations ranged from excellent to confused.

Problems included:

- mentioning "interaction term"
- explaining model components rather than the comparison of relationships
- failing to focus on the difference in slopes or differential effect
- weak non-technical language for effect modification
- unclear logic around confidence intervals for interaction quantities

6. Narrative structure problems

Observed issues included:

- conclusions appearing before evidence
- effect estimates separated from their confidence intervals
- sentences doing too many jobs at once
- answer sentences repeated multiple times
- final answers mixed with statistical disclaimers

7. Traceability gaps

Some generated quantities could not easily be traced back to model output or audit information.

This matters because the app now has developer feedback and support-map machinery, and explanations need to be auditable.

8. Numerical-verbal inconsistency

Rare but critical.

One logistic two-factor example gave odds for a male subgroup that numerically decreased with attendance, but verbally said attendance raised the odds. This is a trust-breaking failure and should be guarded against deterministically where possible.

### Tier 3: Consistency and polish issues

9. Technical language leakage

Recurring phrases to avoid or replace:

- fitted model
- fitted linear model
- fitted logit model
- regression results
- interaction term
- intercept
- log-odds
- log count
- exponentiating the coefficient
- baseline level of the predictors, especially when there are no predictors

10. Over-explaining derivations

Student-facing explanations should usually say what the result means, not how the transformed quantity was derived.

11. Numeric formatting

Repeated issues included:

- too many decimal places
- odds not expressed as odds, e.g. 0.70 rather than 0.70:1
- tiny odds such as 0.001 not rescaled to something interpretable like 1:1000
- values such as 11.5671 or 11.567 should be rounded sensibly

12. Natural-language issues

Examples included:

- "average expected frequency" instead of either "average frequency" or "expected frequency"
- "earthquakes per observation"
- awkward research-question paraphrases worse than the original

13. Signal-to-noise issues

Some explanations included dataset metadata or response coding that did not help answer the research question.

## Suggested Stage 9 structure

Stage 9 should begin with triage before code changes.

Suggested sequence:

### Stage 9.01 - Consolidate failure taxonomy

Create a compact failure taxonomy from Stage 8 with categories such as:

- answerDetection
- researchQuestionDetection
- uncertaintyDetection
- comparisonDetection
- typicalCaseDetection
- disclaimerDetection
- modelConstraintDetection
- scaleLeakage
- anchorChoice
- technicalLanguageLeakage
- numericFormatting
- interactionExplanation
- traceability
- narrativeSequencing

Output: an agreed taxonomy that can guide implementation and testing.

### Stage 9.02 - Decide the tagging/schema changes

Clarify the tag ontology.

Likely needed tags or roles:

- researchQuestion
- answer
- effect
- comparison
- uncertainty
- typicalCase
- modelDescription or context
- modelConstraint
- statisticalDisclaimer
- justification or evidence
- scaleTranslation, if retained

Decide whether multi-role tagging should be expected and how it should be displayed.

### Stage 9.03 - Define deterministic formatting rules

Create explicit rules for:

- rounding fitted values
- rounding CIs
- rounding means/anchors
- odds display
- tiny odds display
- percentages
- multipliers
- avoiding raw log-scale quantities

### Stage 9.04 - Define model-aware explanation rules

Separate explanation rules for:

- intercept-only LM
- LM with numeric predictor
- LM with factor predictor
- LM additive models
- LM interactions
- intercept-only logistic
- logistic numeric predictor
- logistic factor predictor
- logistic additive models
- logistic interactions
- intercept-only Poisson
- Poisson numeric predictor
- Poisson factor predictor
- Poisson additive models
- Poisson interactions

### Stage 9.05 - Decide anchor policy

Review and adjust numeric-anchor selection.

The Stage 8 evidence suggests:

- zero should not always be used merely because it is in range
- for student-facing interpretation, a typical value such as mean, median, or meaningful midpoint may be preferable
- anchors should be rounded before display
- the audit object should make the chosen anchor and reason clear

### Stage 9.06 - Improve prompt constraints or post-processing guards

Only after taxonomy and deterministic rules are clear, decide which problems belong in:

- deterministic precomputed quantities
- deterministic post-processing
- prompt instructions
- sentence tagging rules
- developer-mode support maps

### Stage 9.07 - Implementation planning

Break implementation into small stages with tests.

Recommended priority:

1. Tag/schema correction and tests
2. Numeric formatting and scale display
3. GLM scale leakage prevention
4. Anchor policy refinement
5. Interaction explanation guardrails
6. Narrative sequencing and redundancy control
7. terminology cleanup

## Important constraints

Do not implement fixes until the user explicitly asks to begin a specific implementation stage.

When implementation starts:

- keep changes targeted
- prefer deterministic fixes where possible
- add or update offline tests
- do not rely on real LLM calls
- preserve existing working behaviour, especially strong examples from Stage 8.16 to 8.19
- provide changed files as downloadable files or zips
- provide a plain-text git commit message
- generate a WMFM stage script if requested

## First instruction for the next chat

Start by reviewing this Stage 9 context and the Stage 8 evaluation report. Then propose a Stage 9 triage plan that groups the fixes into deterministic, prompt, schema/tagging, and UI/presentation work. Do not edit code until asked.
