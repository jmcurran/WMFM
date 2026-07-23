# Stage 47.4.1 missing-information routing repair

Stage 47.4.1 repairs two compatibility gaps found by the Stage 47.4 tests.

- Outcome-threshold questions newly handled by Stage 47.4 are treated as implemented routes in the unusual-question corpus rather than compared with their historical classifier categories.
- Prediction payloads that silently completed omitted fitted-model predictors are routed to `needs_input`; the clarification names the predictors that were completed instead of returning a prediction based on substituted values.
- The Stage 47.4 isolated post-commit build and installation workflow is retained.
