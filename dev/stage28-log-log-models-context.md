# Stage 28 - Log-log models

## Branch

Suggested branch name after Stage 28.2:

```bash
log-log-models
```

The stage originally used `power-law-models`, but Stage 28.2 renames the development language to avoid misleading student-facing terminology.

## Goal

Add explicit support for log-log model workflows where students fit models of the form:

```r
log(y) ~ log(x)
```

The app should help students understand that this is a transformed linear model whose fitted relationship is best interpreted through proportional changes.

## Terminology decision from Stage 28.2

Use the following terminology split.

- Internal metadata: `logLog`
- Internal code, comments, and tests: "log-log model"
- UI model type display: "Log-log model"
- Student explanations: "proportional-change relationship"
- Follow-up questions: percentage changes or meaningful domain-specific changes
- Equation tab: show both the fitted log-log equation and the original-scale multiplicative equation

Avoid the student-facing terms "power law" and "elasticity" unless a user explicitly asks for those terms. The term "elasticity" is technically correct but is strongly associated with econometrics. The term "power law" is overloaded and can be misleading because it is often used for claims about heavy-tailed distributions or scale-free behaviour. A log-log regression may imply an original-scale multiplicative equation of the form `y = a * x^b`, but that does not mean the data-generating process should be described as a power-law process.

Preferred student-facing wording:

> The model was fitted after taking logs of both variables. This means the relationship is being interpreted in terms of proportional changes rather than ordinary one-unit changes.

Preferred example wording:

> A 10% increase in carat weight is associated with an estimated percentage change in price.

Avoid wording such as:

> The elasticity is ...

or:

> This is a power-law model.

## Motivation

Students often encounter relationships where proportional changes in `x` are more meaningful than one-unit additive changes. A model fitted as `log(y) ~ log(x)` should not be explained as an ordinary additive linear model on the original response scale.

The app should distinguish:

- the fitted model on the log-log scale
- the implied original-scale multiplicative relationship
- the interpretation of the slope as a proportional-change summary
- the limits of back-transforming fitted values and intervals

## Suggested scope

1. Detect simple log-log model forms.
2. Build deterministic metadata describing the transformation.
3. Add plain-language explanation support using proportional-change language.
4. Add equation-display support for the original-scale multiplicative expression.
5. Add deterministic tests using offline examples.
6. Preserve existing behaviour for ordinary transformed-response models.

## Teaching requirements

Explanations should make clear that:

- the fitted model is linear after taking logs
- the slope describes the approximate percentage change in `y` for a percentage change in `x`
- on the original scale the relationship is multiplicative, not additive
- fitted values may be back-transformed, but uncertainty needs careful wording

## Diamond examples

Use the `ggplot2::diamonds` data.

Suggested examples:

- Diamonds II: `log(price) ~ log(carat)`
- Diamonds III: `log(price) ~ log(carat)` with a follow-up asking for a more meaningful 0.1-carat change
- Diamonds IV: `log(price) ~ log(carat) + cut + color + clarity` with a follow-up asking whether adjusting for cut, color, and clarity substantially improves prediction

Research question:

> Can we predict the price of diamonds on the basis of weight?

Preferred interpretation sequence:

1. Primary interpretation: percentage change in price for a percentage change in carat weight.
2. Intermediate follow-up: change from 1.0 to 1.1 carats, or another domain-relevant 0.1-carat change.
3. Optional advanced follow-up: doubling carat weight.
4. Adjustment follow-up: whether cut, color, and clarity substantially improve predictive performance.

## Development conventions

Continue to use:

- `=` assignment
- camelCase identifiers
- braces on all control structures
- roxygen2 documentation
- modular helper functions
- deterministic offline tests
- thin orchestration layers

## Stage 28.3 implementation note

Stage 28.3 extends the log-log/proportional-change work to bounded unit-change follow-up questions. A request such as "Can you express the weight effect for a 0.1 carat increase?" cannot be answered from the slope alone, because a fixed original-scale increase corresponds to a different proportional change depending on the starting value. WMFM therefore uses a deterministic typical original-scale reference value recovered from the fitted log-predictor values, then converts the requested increase into a proportional predictor change before computing the fitted response multiplier and percentage change.

Student-facing wording should continue to avoid "elasticity" and "power law". The prompt payload should describe the model as log-log internally and should explain fixed original-scale changes using the chosen reference value and proportional-change language.
