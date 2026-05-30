# Stage 27.2 GLM extrapolation diagnostics

Stage 27.2 extends the Stage 27.1 extrapolation policy by making the policy result easier to debug from Developer Mode diagnostics.

The Stage 27.1 policy already decides whether a supplied numeric follow-up value is:

- `in_range`
- `extrapolation_warning`
- `extrapolation_blocked`

Stage 27.2 keeps that decision logic unchanged and adds stable diagnostic fields that can be copied into a future debugging context.

## Added diagnostic shape

The GLM follow-up prediction payload now exposes:

- `extrapolationDiagnostics`
- `extrapolationExplanation`

The diagnostics object contains:

- `status`
- `thresholdFraction`
- `numericPredictors`
- `observedRanges`
- `requestedValues`
- `classifications`
- `explanationText`

Each numeric predictor diagnostic records:

- predictor name
- observed lower and upper bounds
- observed range width
- requested value
- nearest boundary
- distance outside the range
- configured tolerance
- classification
- predictor-specific explanatory text

## Developer Mode JSON

The explanation diagnostics JSON now lifts the most important fields to the top level:

- `extrapolationStatus`
- `extrapolationExplanation`
- `extrapolationDiagnostics`

The full prediction payload is still included as `predictionPayload`, so this is additive and should remain backward compatible.

## User-facing behaviour

This stage does not change the Stage 27.1 policy thresholds or the fitted prediction calculation.

- In-range requests continue normally.
- Slight extrapolation requests still return a deterministic prediction with a warning.
- Extreme extrapolation requests are still suppressed.

The purpose of this stage is to make the reason for those outcomes explicit and machine-readable.
