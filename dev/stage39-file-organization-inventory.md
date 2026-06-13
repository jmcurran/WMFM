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
