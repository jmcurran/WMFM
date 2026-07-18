# Stage 45.5.1: comparable-observation test repair

Stage 45.5 deliberately rerouted suitable "good deal" and "bargain" questions
from the unsupported conditional-quantile pathway to deterministic comparable
observations.

The first Stage 45.5 test run exposed two test-suite problems:

1. the comparable-observation fixture fitted `factor(cyl)` but supplied `cyl = 6`,
   which is not the fitted-model predictor name expected by the established
   prediction parser; and
2. the older conditional-quantile tests still required all good-deal questions
   to remain unsupported, contradicting the Stage 45.5 routing change.

This repair uses an explicit factor predictor in the comparable-case fixture and
separates genuinely distributional percentile questions from the newly
supported comparable-case questions. No package implementation behaviour is
changed.
