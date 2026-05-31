# Stage 28.8.10 - Adjustment comparison follow-up scaffold fix

## Problem

Diamonds IV correctly classified the follow-up question as an adjustment-prediction comparison, but the generated explanation still described only the adjusted model.

The issue was that adjusted models use the deterministic adjustment-aware explanation scaffold path in `lmToExplanationPrompt()`. That early scaffold path returned before the bounded follow-up prompt block was included, so the language model did not receive the deterministic adjusted-versus-weight-only comparison payload.

## Fix

The adjustment scaffold path now preserves the bounded adjustment-comparison follow-up block when the active follow-up category is `adjustment_prediction_comparison`.

For this pathway, the prompt now requires a direct final answer comparing the adjusted model with the simpler weight-only model, while still preserving the adjustment-variable guardrails that prevent level-by-level adjustment-variable interpretation.

## Testing note

A regression test checks that an adjusted log-log model with the Diamonds IV follow-up includes both the adjustment-aware scaffold and the deterministic adjustment-comparison payload in the final explanation prompt.
