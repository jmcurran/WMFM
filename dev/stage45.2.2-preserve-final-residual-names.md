# Stage 45.2.2: preserve final residual names

Stage 45.2.1 attempted to restore names after constructing the observations data frame. That assignment did not survive the data-frame replacement pathway used by the returned object.

Stage 45.2.2 constructs the data-frame object from a named list so the residual column retains the observation names at the final return boundary. Names are taken from `stats::residuals(model)` and fall back to the fitted model-frame row labels when needed.

The repair does not change residual values, ranking order, row mapping, classifications, prompt guardrails, or statistical scope.
