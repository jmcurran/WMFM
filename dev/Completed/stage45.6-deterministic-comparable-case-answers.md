# Stage 45.6: Deterministic comparable-case answers

Stage 45.6 turns the Stage 45.5 comparable-observation payload into a student-visible deterministic answer.

The answer:

- reports the fitted-model predictor settings used for matching;
- summarises the observed response range and median across the selected neighbours;
- lists up to five verified nearest observations with source rows, responses and distances;
- appends only once after the main model explanation;
- reports deterministic failure guidance when comparable observations cannot be computed; and
- states that proximity alone does not establish a bargain, unusual value or causal difference.

The language model receives the comparable cases only as bounded consistency context and is instructed not to produce a competing answer or invent additional cases, thresholds, percentiles, predictions or residual rankings.

Stage 45.7 remains responsible for richer comparable-case presentation in the app.
