# Stage 48.6: individual prediction insertion

Stage 48.6 adds a distinct prediction tool to the student-authored explanation toolbar.

## Delivered behaviour

- A new `y-hat` toolbar button opens an individual-prediction dialog.
- Linear models provide an individual predicted value and an optional prediction interval.
- Linear-model wording can describe the inserted quantity as either a predicted individual value or a typical value.
- Logistic models provide predicted probability, predicted odds, or predicted log odds. They do not display a prediction interval for a future binary response.
- Poisson models provide predicted count or predicted log count. A predictive interval for a future Poisson count remains deferred because it requires a separately defined predictive-distribution calculation.
- The prediction tool remains distinct from `mu-hat`, which inserts an expected or fitted mean and its confidence interval.

## Scope boundary

Stage 48.6 does not add prediction intervals for binomial or Poisson outcomes. It also does not change the existing coefficient, fitted-mean, comparison, residual, or formative-feedback calculations.
