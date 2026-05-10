# WMFM context: pedagogical stability of model explanations

## Purpose

This work stream focuses on evaluating the stability and consistency of LLM-generated model explanations.

The goal is not to grade a single explanation, but to understand:

> How consistent, reliable, and pedagogically stable are explanations generated for the same model across repeated runs?

This is primarily a developer and instructor diagnostic tool, not a student-facing feature.

## Background

The WMFM explanation system now includes:

- deterministic audit objects
- structured prompt construction
- post-processing rules
- explanation diagnostics
- claim-to-evidence mapping
- teaching summaries

Stage 15 introduced:

- improved scoring and grading infrastructure
- audit-aware scoring context
- structured grading objects (wmfmGrade, wmfmGradeListObj)

## Core question

When the same model is explained multiple times, how stable are the resulting explanations?

## Key dimensions of stability

1. Numerical consistency  
2. Scale correctness  
3. Claim structure  
4. Interpretation correctness  
5. Pedagogical framing  

## Proposed approach

- Generate repeated explanations  
- Extract structured claims  
- Compute stability metrics  
- Summarise stability  

## API direction

computeExplanationStability(runs)  
summariseExplanationStability(stabilityObj)  
plotExplanationStability(stabilityObj)  

Object: wmfmStability

## Design principles

- deterministic first  
- no LLM required for core metrics  
- reproducible and testable  

## Next stage goal

Build minimal working version with one stability metric and summary output.
