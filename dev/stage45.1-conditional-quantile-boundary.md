# Stage 45.1: conditional-quantile boundary

## Purpose

Stage 45 originally included "What is a good deal on a diamond?" as an example of an observation-level residual question. That example conflated two different statistical tasks.

Residual inspection asks which observations already in the fitted data are above or below their fitted conditional means. A good-deal question for a specified diamond asks for a lower part of the conditional price distribution for diamonds with those characteristics.

Stage 45.1 establishes this distinction before residual-ranking work begins.

## Classification contract

The bounded follow-up classifier recognises a `conditional_quantile_request` only when the question contains all of the following:

- an outcome-value target, such as price, value, mark, score, or outcome;
- relative-threshold language, such as good deal, bargain, relatively low, or a named percentile; and
- characteristics defining a case, such as a one-carat diamond or a student with specified predictor values.

The classifier excludes ordinary coefficient questions and questions that explicitly ask WMFM to identify or rank existing observations.

## Deterministic response

A matched request is marked unsupported for the current pathway with reason code `conditional_distribution_required`.

The response records that:

- the question concerns a lower or upper part of a conditional distribution;
- an ordinary linear model estimates the conditional mean;
- residual inspection does not directly answer the question; and
- a conditional-quantile method, such as quantile regression, is needed after defining the intended percentile.

Raw question text remains withheld from unsupported prompt blocks.

## Scope

Stage 45.1 does not:

- fit a quantile-regression model;
- choose the twenty-fifth percentile or any other threshold on the user's behalf;
- calculate or rank residuals;
- add observation tables or plots; or
- change the paused Stage 44 analysis export.

## Next Stage 45 work

The next implementation step can add deterministic residual inspection for questions that explicitly ask which existing observations are high, low, or far from their fitted values. That pathway must remain separate from conditional-value questions introduced here.
