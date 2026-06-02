# Stage 33 example-coupling close-out

## Summary

Stage 33 audited and reduced example-specific coupling in WMFM production code and prompts.
The remaining example-specific terms are expected to live in example fixtures, tests,
documentation examples, or user-facing teaching placeholders rather than production
branches that special-case a built-in example.

## Durable regression coverage

The temporary Stage 33 coupling test has been converted into permanent
domain-agnostic regression coverage. The durable test name is:

```text
tests/testthat/test-domain-agnostic-behaviour.R
```

This keeps the anti-coupling checks without leaving a stage-numbered test file in
the package.

## Stage 33.8 final cleanup

Stage 33.8 removes the last low-risk domain-specific examples from internal
Roxygen documentation for generic helpers:

- GLM teaching notation documentation now uses neutral outcome/count examples.
- Natural-log parser documentation now uses `log(x)` rather than a built-in
  example variable.

No production interpretation logic is intentionally changed in Stage 33.8.

## Recommended close-out check

Before closing the branch, run the standard stage script workflow and then run a
final grep audit from the repository root:

```bash
grep -RniE "Diamonds|diamonds|carat|price|cut|color|clarity|earthquake|quake|magnitude|Locn|Freq|oyster|Course|Pass|Fail" \
  R inst DESCRIPTION \
  --exclude-dir=.git \
  --exclude="*.Rd" \
  --exclude="NEWS.md" 2>/dev/null || true
```

Remaining hits should be classified before action. Not all hits are defects:
words such as `passed`, `failed`, `failure`, `color`, `frequency`, and CSS
colour terms are normal generic code terms and should not be treated as example
coupling.
