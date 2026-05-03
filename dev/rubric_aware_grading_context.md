# WMFM context: rubric-aware grading of explanations

## Purpose

Extend grading to support:

1. Default automated grading (out of 10)
2. Instructor-defined rubric-based grading

## Background

Stage 15 introduced:

- structured grading objects
- audit-aware scoring
- deterministic + LLM hybrid grading

## Core question

Can grading align with instructor expectations using a structured rubric?

## Default grading

Maintain current behaviour (score out of 10).

## New feature: instructor rubric

grade(x, rubric = myRubric)

## Example rubric

myRubric = list(
  dimensions = list(
    numericalCorrectness = list(weight = 3),
    interpretationCorrectness = list(weight = 3),
    scaleUse = list(weight = 2),
    clarity = list(weight = 2)
  ),
  requirements = list(
    mustMentionCI = TRUE
  ),
  penalties = list(
    incorrectScale = -2
  )
)

## Scoring pipeline

1. deterministic evidence  
2. rubric evaluation  
3. optional LLM augmentation  
4. final score  

## Output object additions

- rubricUsed  
- dimensionScores  
- penaltiesApplied  

## Design principles

- deterministic first  
- transparent scoring  
- offline testable  
- backward compatible  

## Next stage goal

Implement minimal rubric system with 1–2 dimensions and tests.
