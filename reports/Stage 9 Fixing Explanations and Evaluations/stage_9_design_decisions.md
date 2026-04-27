# WMFM Stage 9 Design Decisions

## Overview

Stage 9 focuses on converting Stage 8 findings into a structured, implementable fix strategy. The emphasis is on introducing control systems around explanation generation, reducing reliance on LLM behaviour, and improving consistency, interpretability, and pedagogical clarity.

---

## Stage 9.1 – Failure Taxonomy

A compact, actionable taxonomy was defined.

### A. Tagging / Schema Failures
- answerDetection
- researchQuestionDetection
- uncertaintyDetection
- comparisonDetection
- typicalCaseDetection
- disclaimerDetection
- modelConstraintDetection
- evidenceDetection

### B. Statistical Control Failures
- scaleLeakage
- anchorChoice
- interactionExplanation
- modelAwareness
- traceability

### C. Presentation Failures
- technicalLanguageLeakage
- numericFormatting
- narrativeSequencing
- redundancy
- plainLanguage

### Key Decisions
- Statistical disclaimers refer to general scope limitations (e.g. not causal, applies on average).
- Multi-role tagging is REQUIRED.

---

## Stage 9.2 – Tagging / Schema Design

### Core Structure
Each sentence has:
- primaryRole
- roles (multi-valued)
- supporting metadata

### Role Ontology
- researchQuestion
- answer
- effect
- comparison
- uncertainty
- typicalCase
- evidence
- modelContext
- modelConstraint
- statisticalDisclaimer
- scaleTranslation

### Key Design Rules
- Multi-tagging is mandatory.
- Tags describe function.
- Quality flags describe problems.

### Effect vs Comparison
- effect = change in response due to a predictor or treatment
- comparison = explicit contrast between groups or levels
- A sentence may be both.

Refinement:
- Effects should be expressed as changes (e.g. "a change in x").

---

## Stage 9.3 – Deterministic Formatting Rules

### Core Principle
All numeric presentation should be deterministic and not delegated to the LLM.

### Rounding
- Default: 2 significant figures
- Percentages: whole numbers (or 1 decimal if needed)
- Anchors: rounded to sensible values

### Odds
- Default: decimal:1 format
- Small odds: use 1:N
- Integer ratios only when clearly beneficial

### Scale Rules
- Always present on interpretation scale
- Avoid log-scale quantities in student-facing output

### Quality Flags
- rawLogScaleValue
- oddsShownAsDecimal
- excessiveDecimalPlaces
- tinyOddsNotRescaled

---

## Stage 9.4 – Model-Aware Explanation Rules

### Model Profile
Must include:
- modelFamily
- modelStructure
- modelScale
- interpretationScale
- responseVariable
- transformationType

### Key Additions

#### Response Scale Handling
- Distinguish modelScale vs interpretationScale
- Always explain on interpretationScale
- Treat derived variables and inline transformations equivalently

#### Comparison Control
Introduce:
- comparisonScope: none / minimal / targeted / full

Default:
- avoid enumerating all group comparisons
- summarise differences unless explicitly required

#### Interaction Structure
Mandatory pattern:
1. Within-group effects
2. Comparison of effects

Avoid:
- "interaction term"
- coefficient-level explanations

---

## Stage 9.5 – Anchor Policy

### Core Principle
Anchors must be pedagogically meaningful, not just mathematically valid.

### Anchor Hierarchy
1. User-specified
2. Research-question relevant
3. Meaningful value
4. Median/mean
5. Zero (only if meaningful)

### Default Choice
- Prefer median for skewed variables

### Anchor Metadata
- anchorValue (raw)
- anchorDisplayValue (rounded)
- anchorReason

### Key Rule
- Separate computational value from displayed value

---

## Stage 9.6 – Control Architecture

### Core Principle
If something can be deterministic, it should not be left to the LLM.

### Control Layers
1. Deterministic inputs
2. Deterministic formatting
3. Prompt constraints
4. Tagging + validation
5. Post-processing guards

### Responsibilities

Deterministic:
- formatting
- scale handling
- anchors
- numeric values

Prompt:
- phrasing
- flow
- plain language

Validation:
- missing answer
- missing uncertainty
- numeric-verbal mismatch
- excessive comparisons

### Structural Templates
Explanations should follow deterministic skeletons (e.g. interaction pattern).

---

## Overall Architecture

Model → Audit → ModelProfile → Deterministic Quantities → Formatted Values → Prompt → Generated Text → Tagging → Validation → UI

---

## Summary of Key Decisions

- Multi-role tagging is mandatory
- Deterministic control of numbers, scale, and anchors
- LLM is responsible for language, not computation
- Comparison explosion must be controlled
- Interactions require structured explanation
- Response transformations must be handled explicitly

---

## Next Stage

Stage 9.7 – Implementation planning
- break into staged changes
- define tests
- begin code implementation
