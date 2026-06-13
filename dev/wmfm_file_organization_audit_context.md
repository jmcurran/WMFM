# WMFM file-organization audit context

## Purpose

This context records a small CRAN-hardening observation about WMFM source and test file names. It is intentionally a planning/audit note, not an implementation stage.

The package still broadly follows the earlier source-file refactor pattern, where files are grouped by functional prefixes such as:

- `api-`
- `app-`
- `class-`
- `examples-`
- `explain-`
- `methods-`
- `model-`
- `plot-`
- `prompt-`
- `scoring-`
- `text-`
- `utils-`

That structure remains useful and should not be replaced during CRAN hardening unless a concrete CRAN issue requires it.

## Current observation

The main source-tree drift is not that the prefix system has failed. The drift is mostly localised:

- some `app-server-*` files are now highly fragmented because app features have accumulated over many stages;
- some `model-explanation*`, `prompt-*`, and `scoring-*` files encode very specific behaviours that may be worth regrouping later;
- some historical development-stage wording leaked into tests and NEWS entries, which is more CRAN-facing than the source-file organisation itself.

Stage-numbered test filenames have now been replaced with meaningful names and guarded by an offline test. That is the higher-priority CRAN-facing cleanup.

## Recommendation for CRAN hardening

Do not perform a broad R-file rename/refactor during the CRAN-hardening stream unless checks force it.

Reasons:

- file renames are noisy in review;
- they can obscure CRAN-readiness changes;
- they risk merge conflicts and stale roxygen output;
- they do not materially change package behaviour;
- they can be addressed safely after the CRAN submission path is stable.

For Stage 38, keep any file-organisation work limited to naming hygiene that affects public quality, such as avoiding stage-numbered tests, stage-numbered exported objects, or development-stage terminology in user-facing files.

## Suggested post-CRAN follow-up stream

A later file-organisation stream could:

1. inventory all `R/*.R` files by prefix;
2. identify small one-function files that could be combined without reducing readability;
3. identify large app-server files that should be split by UI workflow rather than historical feature stage;
4. normalise any remaining mixed naming styles in touched files;
5. add a lightweight filename guard for development-stage names in `R/` and `tests/` if the Stage 38 guard proves useful.

This should be treated as a maintainability/refactor stream, not as part of the CRAN-hardening critical path.
