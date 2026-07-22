# Stage 47.7.2 runner path repair

Stage 47.7.1 replaced the simulated developer examples with real packaged data, but its runner looked for a stage-numbered script that was not present.

This repair retains the stable developer script name:

```text
dev/run-stage47.7-question-examples.R
```

The Stage 47.7.2 runner now executes that file directly. No question examples, fitted models, expected routes, or report logic are changed by this repair.
