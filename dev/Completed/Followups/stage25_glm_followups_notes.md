# Stage 25 GLM follow-up prediction cleanup

Stage 25 extends WMFM's deterministic follow-up prediction path to supported
GLMs while keeping generated archives out of version control.

Implemented scope:

- binomial GLM follow-up predictions are computed on the response probability
  scale with recorded family/link metadata;
- Poisson GLM follow-up predictions are computed on the expected-count response
  scale with recorded family/link metadata;
- omitted fitted-model predictors can be completed through the existing
  deterministic reference-level/sample-mean pathway when that completion is
  explicitly allowed;
- research-question prediction detection can still disable missing-predictor
  completion and return a safe needs-input result;
- developer diagnostics JSON includes GLM model type, family, link, response
  scale, warnings, prediction payload, and prompt excerpt fields;
- prompt payloads include a GLM-specific deterministic follow-up block so the
  LLM does not recompute or invent GLM predictions.

No zip files, tarballs, RData files, rds files, or generated binary artifacts
belong in the Stage 25 pull request.
