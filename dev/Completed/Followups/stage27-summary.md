# Stage 27 summary: GLM follow-up enhancements

## Scope

Stage 27 completed the deferred GLM follow-up enhancements after the earlier Stage 25 work. The work focused on deterministic follow-up handling for GLM predictions, diagnostics, extrapolation policy, and future-outcome uncertainty framing.

## Completed substages

- Stage 27.1 introduced a deterministic extrapolation policy for numeric GLM follow-up values.
- Stage 27.2 expanded diagnostics for extrapolation handling, including observed ranges, requested values, classification, and explanatory text.
- Stage 27.3 expanded natural-language parsing for common student wording, including unnamed numeric values, attendance wording, Washington location wording, and test-mark odds wording.
- Stage 27.4 documented the distinction between fitted-mean confidence intervals and future-outcome prediction intervals.
- Stage 27.5 added conditional Poisson future-count prediction intervals.
- Stage 27.6 added Bernoulli future-outcome framing for binomial-logit follow-up requests.
- Stage 27.6.1 and Stage 27.6.2 repaired compatibility and policy alignment issues found by the local test suite.
- Stage 27.7 cleaned up behaviour-oriented test descriptions and moved Stage 27 development notes into `dev/completed/`.

## Final behaviour

GLM follow-up predictions now distinguish between:

- fitted mean response predictions;
- confidence intervals for the fitted mean;
- conditional Poisson future-count prediction intervals;
- Bernoulli future-outcome probability framing for binomial-logit models;
- unsupported interval requests where a conventional interval would be misleading.

The implementation keeps the deterministic, offline-safe testing approach used throughout WMFM. It also preserves diagnostics metadata so future UI or explanation work can describe why a prediction was allowed, warned, blocked, or framed in a particular way.

## Notes for future work

Potential later work includes exposing these diagnostics more visibly in the GUI, refining student-facing wording for prediction intervals, and deciding whether any of the developer diagnostics should be promoted into regular user-facing output.
