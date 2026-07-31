# Stage 49.8.14.2 diagnostics test fixture repair

This repair keeps the Stage 49.8.14 explanation-generation diagnostics and fixes
three test failures found during strict validation.

- The mocked `lmExplanation()` binding in the equation fallback test now accepts
  the new optional `diagnostics` argument used by `runModel()`.
- The explanation-generation diagnostic tests now use non-perfect linear-model
  data so strict warning conversion does not reject the fixture before the
  diagnostic assertions run.
- No production explanation or diagnostic behaviour is changed by this repair.
