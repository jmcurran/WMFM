# Stage 48.8.1: fitted-mean validation repair

Stage 48.8.1 repairs three validation failures found after Stage 48.8.

- Preserve the historical observation-index interface for fitted-mean fragments while supporting the new data-frame profile interface.
- Use linear-model confidence-interval prediction for `lm` objects rather than requesting the GLM-only `type = "link"` result.
- Narrow the model-plot wording test so unrelated developer diagnostic controls elsewhere in the application do not cause a false failure.

No user-facing statistical behaviour introduced in Stage 48.8 is removed.
