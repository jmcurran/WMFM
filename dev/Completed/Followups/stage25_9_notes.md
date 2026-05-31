# Stage 25.9 notes

Stage 25.9 keeps the Poisson GLM follow-up examples inside the observed earthquake magnitude range.

Changes:

- Updated the one-predictor Poisson developer follow-up example from `Magnitude = 3` to `Magnitude = 5.4`.
- Updated the two-predictor Poisson developer follow-up example from `Magnitude = 3 and Locn = WA` to `Magnitude = 5.4 and Locn = WA`.
- Updated the deterministic follow-up tests to use the same in-range magnitude value.
- Kept the Stage 25.8 classifier behaviour for expected-frequency questions with explicit predictor assignments.

Rationale:

`Magnitude = 3` is useful as an extrapolation stress test, but it is not a good developer-mode example because the fitted earthquake examples use observed magnitudes from about 5.25 to 7.25. `Magnitude = 5.4` is in range but away from the typical value used in the main explanation.
