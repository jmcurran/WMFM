# Stage 49.8.2 regression repair

Stage 49.8.2 repairs three regressions exposed by the full test suite after
Stage 49.8.1:

- vague personal outcome questions such as "Will I do well?" retain all fitted
  predictors as missing unless concrete values are supplied;
- deterministic-first diagnostics compare normalized text, making the check
  robust to harmless whitespace differences; and
- naturally extracted values for numeric predictors are stored as numeric
  scalars rather than character strings.
