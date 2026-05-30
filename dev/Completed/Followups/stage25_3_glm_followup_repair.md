# Stage 25.3 GLM follow-up repair

This stage repairs the GLM follow-up test-example path from the clean Stage 25.2 baseline.

Changes:

- Replace parser-friendly GLM follow-up examples with more natural student-facing wording.
- Keep the natural wording deterministic enough for the bounded follow-up parser.
- Avoid the out-of-range `Assign = 80` request by using `Assign = 15`.
- Use `Magnitude = 3` in earthquake follow-up examples instead of the typical anchor value.
- Add deterministic confidence intervals for GLM fitted mean-response predictions by computing standard errors on the link scale and back-transforming the limits to the response scale.
- Keep explicit GLM prediction-interval requests as an unsupported safe-failure path.
- Add a clear unsupported reason when GLM future-observation prediction intervals are absent from ordinary fitted mean-response predictions.
- Include generated explanation text in the downloadable follow-up diagnostics JSON when it is available.
- Add offline tests covering the repaired natural numeric parser, GLM confidence intervals, explicit prediction-interval safe failure, earthquake examples at magnitude 3, and diagnostics JSON content.
