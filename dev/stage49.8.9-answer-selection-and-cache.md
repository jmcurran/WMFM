# Stage 49.8.9: answer selection and cached explanation repair

Stage 49.8.9 repairs two defects identified by the Stage 49.8.8 question-routing evaluation.

- A successful two-profile comparison now takes precedence over an incomplete single-profile prediction payload when finalising the research-question objective.
- Cached general explanations are passed through the deterministic research-question answer selector before being returned, so a stale cache entry cannot reattach a generic model summary after a specialised answer.
- The QRoute 05 expected route is corrected to `needs_input` because the attendance value is genuinely absent.

Tests use behaviour-based names rather than stage numbers.
