# Stage 48.7: User-defined prediction profiles

Stage 48.7 replaces observation selection in the individual-prediction dialog with direct covariate entry.

## User-facing behaviour

- The prediction dialog shows one control for every model covariate.
- Numeric covariates accept comma-separated values.
- Factor covariates use a multiple-selection control restricted to fitted levels.
- A single supplied value is repeated across all prediction profiles.
- Non-singleton covariate vectors must have a common length; incompatible lengths are rejected rather than silently recycled.
- The dialog previews the prediction profiles before insertion.
- Inserted text identifies the supplied covariate profile for every prediction.
- Existing linear prediction intervals, logistic scales, Poisson scales, and typical-value wording are retained.

## Controlled broadcasting example

For inputs

```text
Attend: Yes
Test: 5, 10, 15
```

WMFM creates three profiles:

```text
Attend = Yes, Test = 5
Attend = Yes, Test = 10
Attend = Yes, Test = 15
```

Inputs with incompatible non-singleton lengths, such as two attendance values and three test values, are rejected with a validation message.
