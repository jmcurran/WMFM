# Stage 45.8.4: evaluation discovery for developer observation examples

## Purpose

Make the six developer-only observation-question examples available through
`listWMFMEvaluationExamples()` and `runWMFMEvaluationSuite()` without exposing
them in the classroom example list.

## Changes

- Add explicit evaluation metadata to each observation-question example.
- Keep `listWMFMExamples()` and the default example library restricted to
  classroom-facing examples.
- Append developer examples with evaluation metadata to the evaluation
  catalogue.
- Preserve the existing numbering of the original classroom examples.
- Add tests for suite discovery, metadata, and numbering stability.
