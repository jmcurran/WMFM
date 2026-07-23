# Stage 47.4.6: explicit predictor detection repair

Stage 47.4.6 repairs the remaining missing-predictor routing defect.

The prediction parser historically contains a convenience rule: when exactly one
numeric predictor remains unresolved, a lone number in the question may be
assigned to it. In a question such as `attend = 80`, that rule incorrectly reused
`80` for `test`, making the request look complete.

The repair separates two concerns:

- permissive prediction computation may still complete omitted predictors so the
  established prediction payload and compatibility metadata are retained;
- routing checks use a strict extraction mode that does not reuse a labelled
  value as an unlabelled value for another predictor.

Consequently, `attend = 80` is recognised as supplying only `attend`, and WMFM
asks specifically for `test` before presenting the prediction.
