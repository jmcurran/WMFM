# Stage 47.7.4: example naming and portable paths

This repair removes stage identifiers from all packaged LLM-review example names.

The example directories, specification filenames, metadata keys, display names, and suite identifier now use durable descriptive names. In particular, the incomplete diamond prediction example is named `DiamondsMissingPredictors`, with display name `Diamonds Missing Predictors`.

The shorter durable names also keep all installed paths within the portability limits checked by `R CMD check`.
