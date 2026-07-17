# Stage 44.8: reproducible fitted values and intervals

Stage 44.8 sharpens the purpose of the downloadable Quarto analysis: it teaches students how to reproduce the statistical calculations performed by WMFM using ordinary R code.

The export does not include LLM-generated interpretation.

For models containing only factor predictors, the report now constructs the complete factor-level grid and calculates fitted means. Confidence intervals are attached to those fitted means because they demonstrate a distinct prediction calculation that is not already visible in the coefficient table.

For models containing continuous predictors, the report retains explicit fitted-equation code based on `coef(modelFit)`.

The separate all-coefficient confidence-interval section is disabled for newly created recipes. Coefficient uncertainty remains available in the regression output, while user-requested predictions and contrasts continue to reproduce their own intervals.

A future plotting stage may allow users to choose boxplots or beeswarm plots for factor-only models. That work is intentionally outside Stage 44.8.
