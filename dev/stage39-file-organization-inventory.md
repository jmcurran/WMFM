# Stage 39.1 file-organization inventory

## Purpose

This document starts the Stage 39 file-organization stream. It records the current filename structure and identifies low-risk follow-up work before any broad source-file rename or consolidation is attempted.

Stage 39 is deliberately separate from CRAN hardening. The goal is maintainability: understand the current shape of the source tree, preserve useful naming conventions, and make later refactors smaller and easier to review.

## Current version stream

Stage 39 uses the 1.0.1.xxx build series. The first attempted build should use 1.0.1.001, with the final numeric component incremented on every build attempt regardless of success.

## Source inventory from the Stage 38.13 completed archive

The uploaded Stage 38.13 archive contains 194 files under `R/` and 195 files under `tests/testthat/`.

The current source-file prefix counts are:

| Prefix | Count |
| --- | ---: |
| WMFM | 1 |
| api | 8 |
| app | 41 |
| class | 5 |
| examples | 1 |
| explain | 3 |
| methods | 33 |
| model | 56 |
| plot | 4 |
| prompt | 16 |
| scoring | 14 |
| text | 3 |
| utils | 9 |

This confirms that the prefix system is still coherent. The issue is not the naming strategy itself, but local drift in several large or historically accumulated areas.

## Largest source files

The largest current files are:

| File | Lines |
| --- | ---: |
| `R/model-ci-data.R` | 2683 |
| `R/app-developer-scoring-grading.R` | 1396 |
| `R/model-explanation-cleanText.R` | 1373 |
| `R/app-server-contrasts.R` | 1148 |
| `R/app-ui.R` | 1127 |
| `R/utils-runRecords.R` | 1075 |
| `R/model-explanationClaimEvidence.R` | 1008 |
| `R/scoring-llm.R` | 914 |
| `R/scoring-comparison.R` | 841 |
| `R/text-describeField.R` | 835 |
| `R/scoring-runRecords-core.R` | 820 |
| `R/model-explanationTeachingSummary.R` | 795 |
| `R/app-server-model-setup.R` | 768 |
| `R/model-question-unit-change.R` | 741 |
| `R/model-question-prediction-lm.R` | 740 |
| `R/examples-run.R` | 731 |
| `R/model-explanationClaimTagDetectors.R` | 695 |
| `R/model-question-prediction-glm.R` | 646 |
| `R/scoring-grade-summariseLosses.R` | 636 |

These files are better candidates for careful internal decomposition than for simple filename renaming. Any future split should be workflow-based and should include tests that prove behavior is unchanged.

## Small source files

Several source files are intentionally thin API or method wrappers. The smallest current files include:

| File | Lines |
| --- | ---: |
| `R/utils-copyTestOutput.R` | 9 |
| `R/api-score.R` | 12 |
| `R/api-stability.R` | 12 |
| `R/api-compare.R` | 13 |
| `R/api-grade.R` | 13 |
| `R/api-describeField.R` | 15 |
| `R/app-model-output-adjustment-controls.R` | 16 |
| `R/model-lm-typeEquations.R` | 19 |
| `R/app-interpretation-mode-ui.R` | 24 |
| `R/app-server-observer-dependencies.R` | 24 |
| `R/methods-print-wmfmEquationTable.R` | 25 |
| `R/app-run.R` | 27 |
| `R/WMFM-package.R` | 29 |
| `R/class-wmfmGradeComparison.R` | 30 |
| `R/methods-print-metricComparisonSummary.R` | 34 |
| `R/methods-print-wmfmScoresDiagnosis.R` | 37 |

These should not be merged automatically. Thin files can improve discoverability when they represent exported API entry points, S3 methods, or narrow helper roles.

## Naming observations

The only obvious mixed-style source filename found during this first inventory is:

- `R/app-developerFeedback.R`

This is a possible later rename candidate because the rest of the nearby app files use dash-separated filename components. Renaming it should be deferred until a small stage can update references, documentation if needed, and any tests or tooling that mention the exact filename.

Files containing the word `developer` are not automatically a problem. In this package they describe intentional developer-mode UI and scoring workflows, not leaked stage-development terminology.

## Stage 39 recommendations

1. Preserve the existing prefix system.
2. Avoid broad file renames in early Stage 39 work.
3. Use this inventory to choose one narrow follow-up at a time.
4. Prefer behavior-preserving internal decomposition of large files over cosmetic file movement.
5. Treat any filename rename as a separate small stage with before-and-after validation.
6. Do not merge thin exported API wrappers solely because they are short.

## Suggested next Stage 39 steps

A sensible sequence is:

1. Add or update a lightweight filename audit helper that reports prefix counts, large files, small files, and mixed-style filename candidates.
2. Decide whether `R/app-developerFeedback.R` should be renamed in a small isolated stage.
3. Review `R/model-ci-data.R` for internal section boundaries and possible extraction targets.
4. Review `R/app-server-contrasts.R` and related app-server files by UI workflow rather than historical feature stage.
5. Review prompt/scoring files only after the source-tree audit helper is stable.

## Validation note

This stage adds a developer-facing markdown inventory only. It does not change installed package code, tests, generated documentation, package metadata, or user-facing behavior. The Stage 39.1 runner can therefore use the no-package-validation path while still bumping `DESCRIPTION`, updating `NEWS.md`, committing, and archiving the completed stage.

## Stage 39.3 follow-up: developer feedback filename normalization

Stage 39.3 resolves the one concrete mixed-style source filename identified in the Stage 39.1 inventory:

- `R/app-developerFeedback.R` is renamed to `R/app-developer-feedback.R`.

This is a filename-only organization change. The R function names remain unchanged because they are part of the existing internal developer-feedback implementation, and changing those identifiers would be a broader code refactor rather than a file-organization cleanup.

The rename aligns the source filename with the surrounding dash-separated app-file convention while keeping behavior unchanged. Full package validation is still appropriate because the installed `R/` source tree changes and the old filename must be removed during installation of the staged change set.

## Stage 39.4 follow-up: unprefixed and mixed-style filename audit

Stage 39.4 extends the inventory beyond recognized source prefixes. The aim is to make sure that the audit does not only find files that already participate in the prefix system.

### Unprefixed source files

Using the current recognized source prefixes:

- `api`
- `app`
- `class`
- `examples`
- `explain`
- `methods`
- `model`
- `plot`
- `prompt`
- `scoring`
- `text`
- `utils`

there are no unprefixed `R/*.R` files in the Stage 39.3 completed source tree. The package-level file `R/WMFM-package.R` is the only source file outside the lowercase prefix family, and it is an acceptable special case because it is the package-level roxygen documentation file.

This means the source-tree issue is not orphaned unprefixed R files. The higher-value organization work remains:

- large source files that may benefit from internal decomposition;
- historically accumulated app-server files;
- prompt, model-explanation, and scoring files whose names encode narrow behaviours;
- test filenames that still use function-level camelCase names.

### Test helper files

The test tree includes 13 helper files that do not start with `test-`. These are expected `testthat` helper files and should remain unrenamed unless a specific helper is being reorganized:

| File | Lines |
| --- | ---: |
| `tests/testthat/helper-developer-scoring-fixtures.R` | 96 |
| `tests/testthat/helper-diagnose-fixtures.R` | 23 |
| `tests/testthat/helper-fake-wmfmRuns.R` | 104 |
| `tests/testthat/helper-fake-wmfmScores.R` | 74 |
| `tests/testthat/helper-llm-scoring-fixtures.R` | 81 |
| `tests/testthat/helper-metric-comparison-fixtures.R` | 134 |
| `tests/testthat/helper-mockDiagnosisObjects.R` | 67 |
| `tests/testthat/helper-offline-wmfm-model.R` | 21 |
| `tests/testthat/helper-package-source.R` | 43 |
| `tests/testthat/helper-project-files.R` | 36 |
| `tests/testthat/helper-stats20x-fixtures.R` | 24 |
| `tests/testthat/helper-valid-parsed-scores.R` | 40 |
| `tests/testthat/helper-wmfm-test-fixtures.R` | 137 |

Several helper filenames still contain camelCase object names, such as `helper-fake-wmfmRuns.R`, `helper-fake-wmfmScores.R`, and `helper-mockDiagnosisObjects.R`. These are not urgent because helper filenames are not user-facing, but they are reasonable later cleanup candidates if nearby tests are being touched.

### Mixed-style test filenames

The remaining test filename drift is mostly function-name mirroring. Many files use names such as `test-buildExplanationTeachingSummary.R` or `test-scoreWmfmRunRecordsCore.R`. This was useful while the tests were written close to individual functions, but it leaves mixed camelCase and dash-separated naming in the test tree.

This should not be fixed by one broad rename. A safer strategy is:

1. keep function-specific test filenames when they improve traceability;
2. rename only small clusters when a related source-file refactor is already happening;
3. prefer dash-separated behaviour names for new tests;
4. add or extend a filename audit helper only after deciding which naming rule should be enforced.

### Stage 39.4 recommendation

Do not rename more files immediately. The audit now shows that unprefixed source files are not the problem. The next useful implementation stage should choose either:

- a narrow helper/test filename normalization cluster; or
- an internal decomposition plan for one large source file, starting with `R/model-ci-data.R` or one app-server workflow file.


## Stage 39.5 follow-up: reusable file-organization audit helper

Stage 39.5 adds `dev/audit-file-organization.R`, a lightweight developer helper for regenerating the file-organization evidence used by this stream.

The helper is intentionally kept under `dev/` rather than `R/` or `scripts/` because it is not part of the installed package API and should not affect package behaviour. It reports:

- source-file prefix counts;
- source files with unrecognized prefixes;
- mixed-style source filenames;
- mixed-style test filenames;
- the largest source files by line count;
- the smallest source files by line count.

This gives future file-organization stages a repeatable way to check whether a proposed rename or decomposition is addressing a real naming or maintainability issue. It also prevents the audit from depending on one-off manual observations made in chat.

The helper does not enforce a policy yet. Enforcement should wait until the project decides which naming rules should be treated as hard checks rather than advisory diagnostics.

### Suggested command

From the package root:

```bash
Rscript dev/audit-file-organization.R --output dev/file-organization-audit-report.md
```

The output report is intended for local review and does not need to be committed unless it is useful evidence for a later stage.
