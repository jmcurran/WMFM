# Stage 49.8.1: natural value extraction repair

Stage 49.8.1 repairs regressions found by the strict test suite after Stage 49.8.

The repair:

- recognises personal prediction wording using `will` and `how will`;
- extracts the numerator rather than the denominator from phrases such as
  `score 10 out of 20 on the test`;
- resolves the Course `Test` predictor by its normalised name rather than a
  case-sensitive hard-coded lookup;
- aligns the older prediction-first test with Stage 49.8's intended suppression
  of a redundant generic explanation; and
- removes a duplicated YAML key that caused one routing example to be omitted
  from the evaluation suite.
