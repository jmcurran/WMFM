# Stage 44.7.1: dataset names, nominal factors, and fitted equations

Stage 44.7.1 refines the standalone educational analysis introduced in Stage 44.7.

## Changes

- Package datasets retain their original object names throughout generated Quarto documents. For example, the s20x Course analysis now uses `data(course.df)` and `data = course.df` rather than copying the data to `analysisData`.
- Variables deliberately placed in the Factors bucket are treated as nominal factors. Ordered factors are converted with `factor(as.character(x))` so that polynomial contrasts are not retained accidentally.
- The same nominal-factor conversion is applied before WMFM fits the model and in the generated standalone analysis.
- The fitted-equation section now begins with `betas = coef(modelFit)` and uses direct coefficient arithmetic for simple linear models containing one factor and one numeric predictor.
- Additive models show the common slope and level-specific intercepts. Interaction models show both level-specific intercept and slope adjustments.
- More complicated models fall back to the coefficient representation rather than generating a difficult-to-read synthetic equation.

## Validation focus

Offline tests cover package dataset-name preservation, ordered-factor demotion, treatment contrasts, additive group equations, and interaction group equations.
