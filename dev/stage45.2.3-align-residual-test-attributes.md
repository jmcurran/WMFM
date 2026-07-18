# Stage 45.2.3: align residual test attributes

## Purpose

Repair the Stage 45.2 residual inspection test after Stage 45.2.2 correctly preserved observation names on the returned residual vector.

## Change

The first assertion continues to verify that the residual vector, including its observation names, matches `stats::residuals(model)` after ranking.

The second assertion verifies only the numerical identity

```text
residual = observed - fitted
```

and therefore removes names from both sides before comparison. This avoids imposing contradictory attribute requirements on the same returned vector.

## Behaviour

No package behaviour changes. Residual names, values, ranking order, row mapping, classifications, and prompt guardrails remain unchanged.
