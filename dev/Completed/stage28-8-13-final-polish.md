# Stage 28.8.13 final polish

This stage applies a small student-facing polish pass after the deterministic
adjustment-comparison architecture was added.

## Changes

- Rewrites residual meta openings such as "The question asks whether..." when
  they appear in generated explanations.
- Replaces compact "95% c.i." wording with "95% confidence interval".
- Reduces visible "log scale" wording for simple log-log relationship summaries
  by replacing it with proportional-change wording.
- Makes the optional follow-up question placeholder shorter and visually lighter
  so it is less likely to be mistaken for a real example follow-up.

## Deferred UI work

The larger UI/provider issues are intentionally deferred to a later workstream:

- provider and model persistence
- developer mode persistence
- Ollama model discovery hardening
- developer prompt diagnostics accordion styling
- broader developer-panel organisation
