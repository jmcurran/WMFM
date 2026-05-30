# Stage 28 - Power law models

## Branch

power-law-models

## Goal

Add explicit support for power-law model workflows where students fit models of the form:

```r
log(y) ~ log(x)
```

The app should help students understand that this is a transformed linear model whose fitted relationship on the original scale has a power-law form.

## Motivation

Students often encounter relationships where proportional changes in `x` are more meaningful than one-unit additive changes. A model fitted as `log(y) ~ log(x)` should not be explained as an ordinary additive linear model on the original response scale.

The app should distinguish:

- the fitted model on the log-log scale
- the implied original-scale relationship
- the interpretation of the slope as an elasticity
- the limits of back-transforming fitted values and intervals

## Suggested scope

1. Detect simple log-log model forms.
2. Build deterministic metadata describing the transformation.
3. Add plain-language explanation support for the power-law form.
4. Add equation-display support for the original-scale power-law expression.
5. Add deterministic tests using offline examples.
6. Preserve existing behaviour for ordinary transformed-response models.

## Teaching requirements

Explanations should make clear that:

- the fitted model is linear after taking logs
- the slope describes the approximate percentage change in `y` for a percentage change in `x`
- on the original scale the relationship is multiplicative, not additive
- fitted values may be back-transformed, but uncertainty needs careful wording

## Development conventions

Continue to use:

- `=` assignment
- camelCase identifiers
- braces on all control structures
- roxygen2 documentation
- modular helper functions
- deterministic offline tests
- thin orchestration layers
