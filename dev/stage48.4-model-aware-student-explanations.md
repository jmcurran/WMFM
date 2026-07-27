# Stage 48.4: model-aware student explanation support

Stage 48.4 broadens the student explanation workspace introduced in Stages 48.2 and 48.3.

The insertion toolbar now adapts to the fitted model family:

- linear models expose response-scale coefficients, confidence intervals, and fitted values;
- binomial logit models expose log-odds coefficients, odds ratios with confidence intervals, and predicted probabilities;
- Poisson log models expose log-count coefficients, expected-count ratios with confidence intervals, and expected counts.

Term labels now translate common transformations and interactions into more readable wording, including log, square-root, quadratic, and interaction terms.

Fitted-result choices are sampled when a model contains many observations so the toolbar remains manageable. These insertions describe analysed observations and do not replace the existing follow-up prediction machinery for user-specified new observations.

The formative grading workflow remains deterministic and continues to use the existing model-aware WMFM rubric. Stage 48.4 does not reveal a generated model explanation and does not use the LLM provider when checking student work.

Focused offline tests cover linear, logistic, and Poisson insertions, transformed and interaction labels, and the additional toolbar controls.
