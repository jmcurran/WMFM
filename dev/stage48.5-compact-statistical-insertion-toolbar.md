# Stage 48.5: compact statistical insertion toolbar

Stage 48.5 replaces the persistent explanation selectors with a compact editor toolbar inspired by ordinary rich-text editors.

The toolbar provides five statistical insertion tools:

- beta-hat opens a coefficient dialog;
- mu-hat opens a fitted mean or fitted-response dialog;
- delta-hat opens a pairwise-comparison dialog;
- epsilon-hat opens a residual dialog; and
- the ellipsis opens a small collection of supplementary model quantities.

The coefficient, fitted-response, and pairwise-comparison dialogs expose model-aware scale selectors. Logistic models distinguish log odds, odds, probabilities, probability differences, and odds ratios. Poisson models distinguish log expected counts, expected counts, expected-count differences, and expected-count ratios.

Confidence intervals are selected by default. The ellipsis dialog also stores the confidence level used by later insertion dialogs, with 95% as the default.

Pairwise comparisons currently compare two analysed observations. General custom contrasts remain deferred because they require a separate interface for contrast weights and estimands.

Response-scale differences for nonlinear links are inserted without a confidence interval in this first implementation. Link-scale differences and ratios use model-based Wald intervals. A later stage can add delta-method or simulation intervals for probability and expected-count differences.
