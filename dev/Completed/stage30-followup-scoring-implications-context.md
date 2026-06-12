# Stage 30 - Scoring implications of follow-up questions

## Goal

Review and extend the explanation-scoring system so it can account for the richer follow-up question behaviour added during Stage 27.

## Motivation

Stage 27 expanded GLM follow-up handling, including extrapolation policy, diagnostics metadata, natural-language parsing, Poisson future-count prediction intervals, and logistic Bernoulli future-outcome framing.

These changes affect what a good student explanation may contain. The scoring system should avoid penalising correct follow-up-aware explanations and should detect new types of mistakes.

## Suggested scope

1. Review existing deterministic scoring rules for explanations involving predictions and intervals.
2. Identify whether follow-up answers need separate scoring profiles from main model explanations.
3. Add scoring evidence for prediction type, interval type, extrapolation warning, and future-outcome framing.
4. Ensure LLM grading prompts receive enough structured context to grade follow-up explanations fairly.
5. Add deterministic tests and fake-provider tests only; avoid real LLM calls.

## New scoring issues to consider

A student explanation may now need credit for:

- distinguishing fitted mean confidence intervals from future-observation prediction intervals
- correctly describing extrapolation warnings or blocked predictions
- interpreting Poisson prediction intervals as future-count uncertainty
- describing logistic future outcomes as Bernoulli outcome probabilities rather than continuous intervals
- avoiding overclaiming where parameter uncertainty is not included

## Compatibility requirements

Changes should preserve existing scoring behaviour for ordinary model explanations unless there is a deliberate and tested reason to update it.

## Development conventions

Continue to use:

- `=` assignment
- camelCase identifiers
- braces on all control structures
- roxygen2 documentation
- modular helper functions
- deterministic offline tests
- fake providers or mocks for LLM-related tests
- thin orchestration layers
