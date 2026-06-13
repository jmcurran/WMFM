# WMFM Stage 38.13 public NEWS cleanup context

## Purpose

Stage 38.13 prepares the package for a public CRAN release by replacing the detailed development-stage NEWS history with concise release notes for WMFM 1.0.0.

## Changes

- Set `DESCRIPTION` to `Version: 1.0.0` for the CRAN release.
- Rewrote `NEWS.md` as public-facing release notes rather than a stage-by-stage development log.
- Preserved a short note explaining that detailed pre-release history remains available through git history and archived development contexts.

## Validation mode

This stage changes release metadata and documentation only. The runner should not bump the version beyond 1.0.0 and should not run package documentation, tests, checks, build, or install steps.
