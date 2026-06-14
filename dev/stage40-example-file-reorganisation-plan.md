# Stage 40.1 example-file reorganisation plan

## Context

Stage 40 focuses on reorganising the packaged WMFM example files so the example library is easier to maintain and easier to present to students. The current example library mixes classroom examples, public teaching examples, developer regression examples, follow-up examples, and scoring/grading fixtures under the same installed path: `inst/extdata/examples/`.

The future-work context identifies example library polish as a useful follow-on stream, with three main goals:

- separate classroom examples from internal test examples;
- add example metadata such as difficulty, model family, and teaching topic;
- provide a smaller public example list by default, with internal/test examples hidden or developer-only.

## Current example groups

The Stage 39.8 source tree contains these broad example groups.

### Public or classroom-facing examples

These examples do not use the `test-` prefix and are already close to classroom-facing material:

- `Course`
- `DiamondsII`
- `DiamondsIII`
- `DiamondsIV`
- `DiamondsUnitChange`
- `Mtcars`
- `Oysters`
- `Quakes`
- `QuakesUnitChange0_1`

These should remain visible in ordinary release-mode example listing unless a later stage deliberately narrows the default set.

### Developer regression matrix examples

These examples are primarily systematic model-shape fixtures rather than polished teaching examples:

- `test-01-G00F` to `test-19-P11T`
- `test-20-B01F-followup` to `test-25-B11F-followup-odds`

They cover linear, logistic, and Poisson model families, including one-factor, two-factor, interaction, numeric, and follow-up behaviours. They should remain available to tests and developer workflows, but should not be treated as classroom examples.

### Scoring and grading fixture examples

These examples appear to support scoring/grading checks rather than public teaching:

- `test-SG-1` to `test-SG-6`

They include context files and small CSV fixtures. They should be kept developer-only unless a later stage turns one into a polished classroom example.

### Special developer examples

These examples appear to support targeted UI, adjustment, or follow-up behaviours:

- `test-arousal-01`
- `test-arousal-02`
- `test-course-follow-up`

They should remain developer-only for now.

## Proposed staged reorganisation

Do not move all example files in one broad step. The safer sequence is:

1. Add an example manifest or metadata file that classifies each example without changing paths.
2. Update `listWMFMExamples()` to use metadata rather than relying only on the `test` name prefix.
3. Add tests proving public examples remain visible and developer examples remain available when requested.
4. Only after the metadata path is stable, consider moving developer examples into a nested internal folder if the package lookup code and tests are ready.

This avoids breaking `runExample()` or tests that assume the current `inst/extdata/examples/<name>/` layout.

## Suggested metadata fields

A later implementation stage should prefer explicit metadata over path or name heuristics. Useful fields are:

- `exampleAudience`: `public`, `classroom`, `developer`, or `test`
- `exampleFamily`: `lm`, `logistic`, or `poisson`
- `exampleDifficulty`: `introductory`, `intermediate`, or `advanced`
- `teachingTopic`: short human-readable topic label
- `developerPurpose`: optional note for regression fixtures

The existing `modelType`, `formula`, `researchQuestion`, and `followupQuestion` fields should remain unchanged for backward compatibility.

## Stage 40.1 scope

Stage 40.1 is documentation-only. It records the reorganisation inventory and staged implementation strategy but does not change R code, tests, installed example files, or package metadata directly.

Because this stage adds only developer documentation, the runner should use the no-package-validation path: bump `DESCRIPTION`, update `NEWS.md`, commit, and archive, but skip `devtools::document()`, `devtools::test()`, `devtools::check()`, package build, and package install.
