# WMFM Stage 38.2 CRAN preflight context

## Stage purpose

Stage 38.2 starts the CRAN-hardening work on a dedicated branch named `cran-hardening`.

The immediate goal is to run the package through a CRAN-oriented validation pass before making functional CRAN-compliance changes. This stage should surface the first set of concrete failures, warnings, notes, example issues, external-service issues, or metadata concerns that need to be repaired in later Stage 38 steps.

## Branch

Use a dedicated branch for this release-preparation stream:

```bash
git checkout -b cran-hardening
git push -u origin cran-hardening
```

If the branch already exists locally, check it out instead. If it already exists remotely, push with the existing upstream rather than creating a second branch.

## Version numbering

Stage 38 uses beta-style internal preparation versions before CRAN release:

- Stage 38.1 started at `0.9.9.001`.
- Stage 38.2 should consume the next build number, `0.9.9.002`, when the stage runner is executed.
- The final CRAN submission version is expected to become `1.0.0` later, after CRAN hardening is complete.

As usual, every local build attempt should increment the final numeric component regardless of success.

## Validation intent

Unlike ordinary documentation-only stages, Stage 38.2 should force package validation because this is explicitly a CRAN preflight stage.

The runner should run:

- R string escape preflight, if the helper exists.
- `devtools::document()`.
- Strict offline tests with warnings promoted to errors.
- The usual strict package check used by the WMFM stage workflow.
- A CRAN-oriented check using `devtools::check(args = c("--as-cran"), error_on = "warning")`.

The `--as-cran` result should be treated as the main evidence for the next repair stage. If it fails, keep the output and use it as the Stage 38.3 starting context.

## Scope

This stage does not yet fix CRAN issues. It creates the branch and records the preflight intent so the CRAN-hardening stream has a clean starting point.

Later Stage 38 work should address issues such as:

- External-provider behavior during examples, tests, help-page execution, and package load.
- The non-localhost Ollama default URL.
- CRAN-safe examples and documentation.
- Public-release README, NEWS, and metadata polish.
- Persistent-write behavior in examples and tests.

## Expected result

If all checks pass, Stage 38.2 can be committed as the CRAN-preflight baseline. If CRAN checks fail, that failure is still useful: the local output becomes the evidence for the next targeted repair stage, but the stage runner will stop before committing completed work.
