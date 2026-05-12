# Stage 20.7.1: check roxygen documentation freshness in CI

- Updated the GitHub Actions R test workflow to run `devtools::document()` before tests.
- Added a `git diff --exit-code` check immediately after documentation generation so CI fails when tracked generated files are stale.
- This protects against missing updates to generated documentation artifacts (including `NAMESPACE` and `man/*.Rd`) in pull requests.
- Kept CI behavior read-only with respect to docs: no auto-commit or write-back from CI.
- Left application runtime behavior unchanged.
- Bumped package version from `0.1.8.584` to `0.1.9.584` for this Stage 20.7.1 increment.

## Validation note

This stage can be syntax-checked locally, but complete validation depends on a GitHub Actions run because the enforcement point is CI.
