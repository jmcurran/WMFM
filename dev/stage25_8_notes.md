# Stage 25.8 notes

Stage 25.8 routes explicit expected-value follow-up questions with predictor assignments through the deterministic prediction pathway.

This addresses Poisson examples such as:

- What earthquake frequency would you expect for Magnitude = 3?
- What earthquake frequency would you expect for Magnitude = 3 and Locn = WA?

These questions should be classified as `prediction_request` rather than `emphasis_effect_size`, allowing deterministic prediction diagnostics to be populated before any out-of-range handling or explanation wording is applied.
