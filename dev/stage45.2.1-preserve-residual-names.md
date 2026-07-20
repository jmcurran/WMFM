# Stage 45.2.1: preserve residual observation names

Stage 45.2 correctly computed and ranked ordinary linear-model raw residuals,
but converting the selected residual vector with `as.numeric()` removed its
observation names. This caused the deterministic result to differ from the
named residual vector returned by `stats::residuals()`.

Stage 45.2.1 preserves the selected residual names after constructing the
observation table. The numeric values, ranking order, identifiers, source-row
mapping, prompt payload, and Stage 45.2 statistical scope are unchanged.
