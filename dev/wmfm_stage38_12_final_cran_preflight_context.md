# WMFM Stage 38.12 final CRAN preflight context

## Purpose

Stage 38.12 is the final CRAN-hardening preflight for the current release-preparation branch.

The goal is not to add new package functionality. The goal is to run a deliberately full validation pass after the earlier CRAN-hardening stages that addressed external-provider defaults, provider documentation, startup/network boundaries, README guidance, package-level help, example safety, and development-history cleanup.

## Validation intent

This stage intentionally forces full validation even though the change set itself is documentation/context only.

The stage runner should:

- bump the development build number before validation;
- run the R string escape preflight if the helper exists;
- run `devtools::document()`;
- run strict tests with warnings as errors;
- run a CRAN-style check with `devtools::check(args = c("--as-cran"), error_on = "warning")`;
- update `NEWS.md` after validation succeeds;
- commit the stage result;
- create `stage38_12_completed.zip`;
- build the package source tarball without repeating checks; and
- install the built source tarball.

## Important runner rule

`devtools::check()` is the validation gate. The later build step must only build the source tarball with `devtools::build()` and must not repeat CRAN/as-CRAN checks.

## Expected outcome

If the CRAN-style check passes, Stage 38 can be regarded as CRAN-hardening complete unless manual review identifies new issues.

If the check fails, the failure output should become the context for a follow-up Stage 38.12.1 or 38.13 repair stage, depending on the size of the fix.
