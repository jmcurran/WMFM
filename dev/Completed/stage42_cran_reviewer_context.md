# WMFM Stage 42 context: CRAN reviewer corrections

## Purpose

This stage is a CRAN resubmission repair stage for WMFM. It should address the CRAN reviewer issues reported by Benjamin Altmann after submission of WMFM 1.0.0.

The stage should be treated as Stage 42. The code base entering this workstream is the latest completed Stage 41 bundle, including the provider configuration work up to Stage 41.12 or the latest completed repair if available.

The primary goal is CRAN compliance, not feature development. Keep all changes targeted, conservative, and easy to audit.

## Versioning

Use the Stage 42 version pattern agreed for the CRAN repair stream. If no newer rule has been given in the active chat, use the existing WMFM build-number convention and increment the final numeric component on every build attempt, regardless of whether validation succeeds.

If starting from a 1.0.3.xxx development version, continue from the current DESCRIPTION version rather than resetting to 1.0.0.

## Required skills and workflow

Use the WMFM stage workflow and the usual skills:

- r-development-style
- wmfm-stage-script-generator
- wmfm-news-maintainer

Follow James's R development conventions:

- use `=` for assignment
- use camelCase identifiers
- always use braces on control structures
- prefer roxygen2 documentation
- prefer `@importFrom` over `pkg::fun()` in package code
- keep tests offline and deterministic
- do not use live LLM calls in tests
- keep changes modular and targeted
- do not weaken existing validation gates

Generate normal WMFM deliverables for any implementation stage:

- `stage42_changes.zip` or the appropriate incremental Stage 42 archive name
- `run_stage42.sh` or the appropriate incremental runner name

The runner should use the standard WMFM script-generator rules, including version bump before validation, NEWS update after validation, strict tests, check, build, install, and archive creation where appropriate.

## CRAN reviewer message summary

The reviewer requested the following corrections.

### 1. DESCRIPTION formatting for package/software/API names

CRAN requested that package names, software names, and API names in the Title and Description be written in single quotes.

Current submitted Title:

```text
Explore Fitted Linear and Generalised Linear Models with Shiny
```

Current submitted Description excerpt:

```text
Provides a Shiny application that helps learners connect regression tables to fitted (generalised) linear models.
```

Required direction:

- Use single quotes around package/software/API names such as 'shiny'.
- Respect package-name case sensitivity.
- Update DESCRIPTION Title and Description accordingly.
- Avoid over-quoting ordinary statistical terms unless CRAN expects them as software/API/package names.

Possible Title direction:

```text
Explore Fitted Linear and Generalised Linear Models with 'shiny'
```

Possible Description direction:

```text
Provides a 'shiny' application that helps learners connect regression tables to fitted generalised linear models.
```

Confirm final wording before committing if the wording could affect package positioning.

### 2. DESCRIPTION references

CRAN requested references describing the methods in the package, if available, in DESCRIPTION form:

```text
authors (year) <doi:...>
authors (year, ISBN:...)
<https:...>
```

There must be no space after `doi:` or `https:` and angle brackets must be used for auto-linking.

Action needed:

- Decide whether WMFM has method references that should be cited in DESCRIPTION.
- If yes, add them to the Description field in CRAN-compatible form.
- If no formal method paper exists, add stable web references only if they genuinely describe the methods or software basis.
- Avoid adding weak or irrelevant references solely to satisfy the reviewer.

Potential reference candidates to consider carefully:

- Shiny documentation or package reference if the app framework is materially described.
- Generalised linear model reference if the package is explicitly educational around GLMs.
- Any WMFM-specific repository URL already in URL may not be sufficient as a method reference.

This is a point where it may be better to ask James for preferred references before editing DESCRIPTION.

### 3. Examples for unexported functions

CRAN reported examples for unexported functions:

```text
detectWmfmPattern() in drawModelPlot.Rd
getAdjustedPrimaryEffectSummary() in getChatProvider.Rd
```

Action needed:

- Inspect the relevant roxygen examples that generate those Rd files.
- Either remove the examples, move them to tests, or export the functions if they are genuinely intended as public API.
- Default preference: remove or suppress examples for internal helpers rather than expanding the public API.
- Ensure generated Rd files no longer contain examples that call unexported functions.
- Run `devtools::document()` and inspect the affected `man/*.Rd` files.

Likely files to inspect:

- `R/drawModelPlot.R` or equivalent source for `drawModelPlot()` documentation
- `R/utils-llm.R`, `R/app-server-chat-provider-helpers.R`, or whichever source documents `getChatProvider()`
- `man/drawModelPlot.Rd`
- `man/getChatProvider.Rd`

### 4. Triple-colon usage in documentation

CRAN reported `:::` in generated documentation:

```text
man/buildVarSummary.Rd:
   WMFM:::buildVarSummary(df)
man/detectRespTransform.Rd:
   WMFM:::detectRespTransform("log(y)")
man/detectRespTransform.Rd:
   WMFM:::detectRespTransform(" log1p(count) ")
man/detectRespTransform.Rd:
   WMFM:::detectRespTransform("sqrt(x)")
man/detectRespTransform.Rd:
   WMFM:::detectRespTransform("y")
man/fmt3.Rd:
   WMFM:::fmt3(c(0.001234, 12.3456, 12345.6))
man/getFactorOnlyPredictors.Rd:
   WMFM:::getFactorOnlyPredictors(m, mf)
man/parseWeightText.Rd:
   WMFM:::parseWeightText("0.5")
man/parseWeightText.Rd:
   WMFM:::parseWeightText("1/2")
man/parseWeightText.Rd:
   WMFM:::parseWeightText(" 3 / 4 ")
man/parseWeightText.Rd:
   WMFM:::parseWeightText("not a number")
man/renderVarSummaryUi.Rd:
   ui = WMFM:::renderVarSummaryUi(summaryDf)
```

Action needed:

- Remove triple-colon examples from roxygen source.
- Do not simply replace `:::` with `::` unless the function is exported and intended to be public.
- For internal functions, prefer `@examples` removal, `@examplesIf FALSE`, or moving examples into tests.
- If examples are useful developer documentation, put them in `dev/` notes or tests, not in CRAN-facing Rd examples.
- Regenerate documentation with roxygen2.
- Verify no generated `man/*.Rd` files contain `:::`.

Recommended search commands:

```bash
grep -R ":::" man R tests -n
```

### 5. Interactive examples should use `if (interactive())`

CRAN requested replacing `\dontrun{}` with `if (interactive()) {}` where possible for functions intended only to run interactively, especially Shiny-related functions.

Action needed:

- Search roxygen and generated docs for `dontrun`.
- For app-launching or file-editing functions, use examples such as:

```r
if (interactive()) {
  runWmfmApp()
}
```

or

```r
if (interactive()) {
  editWmfmConfig()
}
```

- Do not run Shiny apps, file editors, or external viewers in examples during checks.
- Regenerate docs and verify generated Rd examples are CRAN-friendly.

Recommended search commands:

```bash
grep -R "dontrun" R man -n
grep -R "interactive()" R man -n
```

### 6. No modification of `.GlobalEnv`

CRAN said not to modify `.GlobalEnv`, including by use of `<<-` in functions.

Action needed:

- Search for `.GlobalEnv`, `globalenv()`, `assign(..., envir = .GlobalEnv)`, and `<<-`.
- Determine whether each instance is in package code, tests, examples, or generated docs.
- Remove or refactor any package-code use that writes to the global environment.
- For closures, reactive values, or mocks, replace `<<-` with explicit mutable state objects, local environments, or return values as appropriate.
- Tests may still need careful handling, but avoid global environment writes in examples/vignettes/tests too if possible.

Recommended search commands:

```bash
grep -R "<<-" R tests vignettes examples inst man -n
grep -R "\.GlobalEnv\|globalenv()" R tests vignettes examples inst man -n
```

Important:

- Do not make broad rewrites. Each use must be inspected.
- If `<<-` is only in tests and writes to a local variable inside a test closure, it may still be better to refactor because CRAN flagged it generally.
- Preserve behaviour with focused tests.

### 7. Do not write by default to user home, package directory, or getwd()

CRAN said functions, examples, vignettes, and tests must not write by default to the user's home filespace, package directory, or current working directory. Examples/tests should write to `tempdir()`.

This is especially relevant to Stage 41 configuration helpers, which use `~/.wmfm/config.json` by default.

Action needed:

- Audit all functions that write files or directories by default.
- Ensure examples/tests use temporary directories via `withr::local_tempdir()` or `tempdir()`.
- Avoid examples that call config-writing functions without redirecting to a temporary config directory.
- Consider whether `editWmfmConfig()` and related helpers are CRAN-safe as exported functions:
  - They may create `~/.wmfm/config.json` interactively.
  - Examples must be guarded with `if (interactive())`.
  - Tests must set `options(wmfm.config_dir = tempdir())` or an equivalent temporary path.
  - Non-interactive checks must not open editors or write to real home.
- If a function writes to the user's config directory by design, make sure it is explicitly user-invoked, documented, and not used by examples/tests by default.

Recommended search commands:

```bash
grep -R "writeLines\|write_json\|file.create\|dir.create\|saveRDS\|write.csv\|write.table" R tests vignettes examples -n
grep -R "path.expand(\"~\|getwd()\|setwd(" R tests vignettes examples -n
```

### 8. Remove `installed.packages()` from package code

CRAN flagged use of `installed.packages()` in:

```text
R/utils-packageData.R
```

Action needed:

- Replace `installed.packages()` with a faster and CRAN-preferred alternative.
- Use `requireNamespace()`, `find.package()`, `system.file()`, or `packageDescription()` depending on the intent.
- If the code only checks whether a package is available, use `requireNamespace(pkg, quietly = TRUE)`.
- If the code needs package metadata for a small number of packages, use `packageDescription(pkg)`.
- Add or update tests for the replacement behaviour.

Recommended search command:

```bash
grep -R "installed.packages" R tests -n
```

## Suggested implementation ordering

Use small but meaningful increments. A suggested Stage 42 sequence is below.

### Stage 42.1: DESCRIPTION and documentation examples

Scope:

- DESCRIPTION Title and Description CRAN wording.
- Add references if James confirms preferred references.
- Remove examples for unexported functions.
- Remove `:::` from generated docs by changing roxygen source.
- Replace `\dontrun{}` with `if (interactive())` where appropriate.

Validation:

- `devtools::document()`
- search generated docs for `:::` and `dontrun`
- strict tests
- strict check

### Stage 42.2: global environment and file-writing audit

Scope:

- Remove or refactor `.GlobalEnv` writes and `<<-` where needed.
- Ensure examples/tests do not write to home, package root, or getwd().
- Make config helper examples CRAN-safe.
- Confirm all config-writing tests use temporary config paths.

Validation:

- targeted greps for `.GlobalEnv`, `globalenv()`, `<<-`, writing functions, home-path writes, and `getwd()`
- strict tests
- strict check

### Stage 42.3: replace `installed.packages()` and final CRAN preflight

Scope:

- Replace `installed.packages()` in `R/utils-packageData.R`.
- Add/update tests.
- Run final CRAN-focused checks.

Validation:

- strict tests
- strict check
- optionally run `devtools::check(args = c("--as-cran"), error_on = "warning")` if James requests CRAN preflight
- inspect `00check.log` for reviewer issues

This sequence can be collapsed if the changes are small after inspection, but avoid huge mixed commits if the global environment/file-writing audit becomes complex.

## Acceptance criteria

Stage 42 should be considered complete only when:

- DESCRIPTION uses CRAN-compliant single quotes for package/software/API names.
- DESCRIPTION contains appropriate references if applicable or a deliberate decision is recorded if no references are added.
- No Rd examples call unexported functions.
- Generated docs contain no `WMFM:::` usage.
- Interactive examples use `if (interactive())` rather than `\dontrun{}` where possible.
- Package functions do not write to `.GlobalEnv` or use `<<-` in CRAN-problematic ways.
- Examples, tests, and vignettes do not write to user home, package directory, or current working directory by default.
- `installed.packages()` is not used in package code.
- `devtools::document()` completes and generated documentation is committed.
- `devtools::test()` passes.
- `devtools::check()` passes.
- If requested, an as-CRAN check passes or any remaining CRAN notes are explicitly explained.

## Important cautions

- Do not remove functionality solely to satisfy a grep unless the functionality is actually CRAN-problematic.
- Do not export internal helpers just to preserve examples unless James explicitly wants them public.
- Do not weaken tests or skip CRAN-facing examples without replacing important coverage elsewhere.
- Do not weaken the stale-roxygen documentation gate.
- Do not make broad formatting churn outside touched files.
- Do not claim local R validation unless R actually ran.

## Reviewer message to keep available

The full reviewer message should be kept in the chat or copied into a private development note while working. It should not necessarily be committed verbatim into package docs, but the NEWS entry should briefly state that Stage 42 addressed CRAN reviewer feedback on DESCRIPTION wording, examples, global writes, file-writing policy, and package availability checks.
