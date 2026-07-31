# Stage 49.5 cross-model-family extension

Stage 49.5 extends question-aware expected-response, individual-outcome, and profile-comparison contracts to logistic and Poisson regression.

The deterministic layer remains the source of truth:

- logistic questions lead with fitted probabilities;
- logistic individual outcomes are described as binary rather than assigned a continuous prediction interval;
- classification thresholds are used only when explicitly supplied;
- Poisson questions lead with expected counts;
- expected counts are not presented as exact future counts;
- logistic probability differences and Poisson expected-count differences are calculated on the response scale;
- GLM comparison intervals use a response-scale delta-method contrast; and
- confidence intervals for fitted quantities remain distinct from future-observation uncertainty.

Focused offline tests cover individual-outcome and two-profile questions for both supported GLM families.
