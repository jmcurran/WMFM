# Stage 48.9: Prediction-aware explanation grading

Stage 48.9 improves deterministic grading of student-written explanations that combine model effects, individual predictions, prediction intervals, expected responses, and confidence intervals.

It prevents negated statements such as "there is no guarantee" from being classified as overclaiming, removes ordinary overclaim wording from the fatal-flaw cap, broadens comparison and numerical evidence recognition, adds prediction-aware semantic diagnostics, and replaces the oversized diagnostic `dput()` with a compact report that omits the underlying data and fitted-model internals.

A developer-only example, `test-Course Explanation Grading`, loads the Course model and research question used to reproduce the motivating explanation.
