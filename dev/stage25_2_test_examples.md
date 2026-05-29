# Stage 25.2 test-example setup

Stage 25.2 adds developer-only examples for GLM deterministic follow-up smoke testing.

## Changes

- Added two logistic GLM follow-up examples using `s20x::course.df` without bundled copied data:
  - `test-20-B01F-followup`: `Pass ~ Assign`
  - `test-21-B11F-followup`: `Pass ~ Attend + Test`
- Added two Poisson GLM follow-up examples reusing the existing earthquake example data and context:
  - `test-22-P01F-followup`: `Freq ~ Magnitude`
  - `test-23-P11F-followup`: `Freq ~ Magnitude + Locn`
- Renamed the existing Course Follow-Up example to `test-Course Follow-Up` under a `test-` example directory so it is hidden from the regular example list and available only when developer test examples are included.
- Bumped the package version build component for Stage 25.2.

## Validation

Run the focused example-listing and Course Follow-Up tests when R is available:

```bash
Rscript -e 'devtools::test(filter = "load-examples-and-research-question-ui|course-followup-example|explanation-prompt-diagnostics")'
```
