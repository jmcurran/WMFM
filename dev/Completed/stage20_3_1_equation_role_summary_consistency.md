# Stage 20.3.1 equation role-summary consistency fix

## Summary

Stage 20.3.1 closes a rendering gap from Stage 20.3: predictor-role summaries for primary vs adjustment variables were shown for data-frame equation outputs but not for character equation outputs.

## What changed

- Added focused fitted-equation UI helpers that render predictor-role summary blocks independently from equation-object shape.
- Updated fitted-equation observer orchestration to use shared helpers.
- Ensured role-summary rendering is applied consistently to both:
  - data.frame equation outputs, and
  - character equation outputs.

## Preserved behavior

- No model fitting behavior changed.
- No formula construction behavior changed.
- Adjustment variables remain included in full fitted equations.
- Stage 20.4 explanation/prompt behavior remains out of scope.
