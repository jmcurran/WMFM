# Stage 45.9.1: evaluation discovery for developer examples

Stage 45.9.1 exposes the six developer observation-question examples through
`listWMFMEvaluationExamples()` and `runWMFMEvaluationSuite()` while preserving
the classroom-facing example catalogue.

Evaluation discovery now reads the `evaluation` block directly from each YAML
specification. It does not load example datasets merely to build the catalogue.
This prevents unrelated developer and test fixtures from failing catalogue
construction because of fixture-specific relative data paths.

The six observation examples use the suite name `observation_questions` and
retain descriptive permanent filenames without stage numbers.
