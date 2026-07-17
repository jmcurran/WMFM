# Stage 44.7.4: preserve ordered-factor provenance

## Purpose

Repair the Diamonds analysis export so WMFM can identify package variables that
were originally ordered factors before it creates the fitted-model copy.

## Changes

- Removed the `diamondsPlainFactors` loading transform from packaged Diamonds examples.
- Retained the original ordered classes in `rv$data` after example loading.
- Continued to convert selected factor variables only in the model-fitting copy.
- Allowed the analysis recipe to record `cut`, `color`, and `clarity` as originally ordered.
- Removed the now-unused Diamonds-specific loading transform helper.
- Updated tests to require the original ordered classes after example loading.

## Expected exported analysis

The generated Diamonds preparation section should identify the selected ordered
factors and convert them compactly with one loop. The fitted model and downloaded
analysis should then use treatment contrasts rather than polynomial contrasts.
