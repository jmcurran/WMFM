# Stage 19.5 repeated scoring audit summary

## Purpose

Stage 19.5 freezes the repeated-scoring calibration outcome and prepares the Stage 19 branch for merge review. This stage does not tune semantic scoring further. It records the before/after audit result and adds regression protection for the calibrated SG-3 and SG-4 examples.

## Audit inputs

The summary compares the original Stage 19.1 audit against the Stage 19.3.1 rerun after semantic calibration and provider-key handling fixes.

- Baseline audit: `stage19_1_*` CSV outputs
- Calibrated audit: `stage19_3_1_*` CSV outputs
- Calibrated fixture directory: `inst/extdata/developer-scoring/stage19-3-1`

## Before and after example stability

| Example | Stage 19.1 mark spread | Stage 19.3.1 mark spread | Result |
|---|---:|---:|---|
| SG-1 | 0.81 | 0.00 | Stable |
| SG-2 | 0.00 | 0.00 | Stable |
| SG-3 | 6.00 | 0.00 | Severe cliff resolved |
| SG-4 | 5.62 | 0.00 | Severe cliff resolved |
| SG-5 | 0.38 | 0.38 | Minor residual variability |

## Main conclusion

The severe SG-3 and SG-4 semantic cliffs are resolved. Their repeated-run mark spreads moved from large binary-like jumps to zero spread in the Stage 19.3.1 rerun.

The remaining SG-5 variability is small. It consists of a 9.62 to 10.00 mark range and does not produce low-mark runs. This looks like mild residual grading variability rather than a catastrophic semantic threshold failure.

## Remaining unstable metrics

The calibrated unstable-metric output contains only SG-5 residual movement:

| Example | Metric | Spread | Interpretation |
|---|---|---:|---|
| SG-5 | Overall score | 3.8 | Minor overall-score drift |
| SG-5 | Effect direction correct | 1.0 | Mild component variability |

No SG-3 or SG-4 unstable metrics remain in the calibrated audit output.

## Recommendation

Do not continue semantic tuning in Stage 19 unless a new regression appears. Further tuning against the current fixtures risks overfitting.

Recommended next workflow:

1. Keep the Stage 19.3.1 fixture JSON files as regression evidence.
2. Keep the configurable Stage 19.4 audit script for future reruns.
3. Use the Stage 19.5 regression tests to guard against reintroducing SG-3 or SG-4 cliffs.
4. Prepare the branch for merge after a clean strict test and check run.

## Known limitation

The current audit uses three repeated runs per example. That is sufficient for detecting the original SG-3 and SG-4 cliffs, but it is not a full stochastic provider-robustness study. Cross-provider robustness should remain a separate future audit rather than being folded into this calibration stage.
