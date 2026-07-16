# WMFM development news

## Purpose

This file records user-facing and developer-facing changes in WMFM. It is a release-note summary, not a commit-by-commit history. Entries are ordered newest to oldest and use WMFM version-number headings.

## WMFM 1.1.1.014

- Corrected reproducible-analysis provenance for package-backed examples loaded through the example interface.
- Package examples such as Course now generate Quarto code that loads the original dataset from its package rather than bundling it as uploaded data.
- Preserved portable bundled-data downloads for examples that genuinely originate from files.
- Added offline tests for package-backed and file-backed example provenance.

## WMFM 1.1.1.012

- Added a user-facing download for the deterministic reproducible analysis document.
- Package data analyses download as a Quarto source file, while uploaded data analyses download as a ZIP containing the Quarto source and a portable CSV copy of the fitted data.
- Kept document generation separate from model fitting and avoided exposing temporary upload paths.
- Added offline tests for Quarto writing, uploaded-data packaging, and app integration.

## WMFM 1.1.1.011

- Added dynamic reproducible-analysis sections for completed predictions and contrasts.
- Added deterministic recipe update helpers that preserve existing model and data state while appending post-fit analyses.
- Rendered linear-model mean and individual predictions with the appropriate confidence or prediction interval and kept GLM predictions on the response scale.
- Restored Stage 44.2 recipe construction and app-state files to the tracked source tree so completed archives are self-contained.

## WMFM 1.1.1.010

- Added previously validated analysis-recipe integration files to version control.
- Added the generated documentation for the Stage 44.4 analysis-section renderers.
- Made the completed Stage 44 baseline self-contained without changing package behaviour.
- Validated with the full offline test and package-check workflow.

## WMFM 1.1.1.009

- Completed the Stage 44.4 reproducible analysis renderer for model summaries, ANOVA tables, confidence intervals, diagnostic plots, and substantive model plots.
- Corrected the core analysis-recipe test so its expected model-plot state matches the new enabled-by-default Stage 44.4 behaviour.
- Retained the exported modelConfidenceIntervals() helper and deterministic report-section settings introduced in Stage 44.4.
- Validated the repaired expectation with the full WMFM package workflow.

## WMFM 1.1.1.007

- Repaired the Stage 44.3 package installation workflow so the exact built tarball remains available after the build process exits.
- Build source packages in a stable project-local temporary directory and remove that directory only after installation succeeds.
- Re-ran the full package validation workflow without changing the Stage 44.3 Quarto rendering behaviour.

## WMFM 1.1.1.006

- Added deterministic Quarto rendering for package loading, data loading, data preparation, and model fitting.
- Generated student-facing code from the validated analysis recipe without using an LLM.
- Supported package datasets and uploaded CSV files while failing explicitly for upload formats whose portable metadata is not yet retained.
- Added offline tests for workflow order, data readers, derived variables, factor conversion, and supported model families.

## WMFM 1.1.1.005

- Repaired the Stage 44.2 runner NEWS update after validation by separating two accidentally joined R statements.
- Retained the package-safe analysis-recipe integration test introduced in the preceding repair.
- Preserved the Stage 44.2 analysis-recipe implementation without changing package behaviour.
- Completed the standard full package validation workflow and release steps.

## WMFM 1.1.1.001

- Define the Stage 44 architecture for deterministic generation of reproducible WMFM analysis documents.
- Establish an output-format-independent AnalysisRecipe captured after model fitting and updated by supported post-fit analyses.
- Select Quarto as the first renderer, with portable handling of package datasets and uploaded data and a staged implementation plan for the 1.1.1 development line.
- Record the architecture as a documentation-only decision stage with no package behaviour changes.

## WMFM 1.1.0.23

- Reserve numerical prediction follow-up answers for WMFM's deterministic renderer so the language model does not duplicate or alter verified results.
- Strengthen prediction prompts against recomputation, paraphrasing, speculative replacement answers, and uncertainty-type changes.
- Rename the active development roadmap context for Stages 43 to 46 and record Stage 46 as unusual or unsuitable research-question handling.

## WMFM 1.1.0.21

- Classified conditioned expected-value and qualitative expectation questions as deterministic prediction requests.
- Resolved conservative geographic factor aliases such as George River to fitted model levels such as GR.
- Routed expected-value requests through GLM extrapolation checks instead of effect-size or unsupported pathways.
- Added prompt and post-processing safeguards that prevent speculative predictions after extrapolation is blocked.
- Added offline regression tests for prediction intent, geographic factor resolution, and blocked-extrapolation explanations.
## WMFM 1.1.0.20

- Back-transformed linear-model predictions and confidence and prediction intervals when the response uses a recognised invertible transformation.
- Reported transformed-response predictions using the original response variable and scale while retaining model-scale values as diagnostic metadata.
- Updated deterministic and language-model follow-up controls so transformed predictions are not described as log-scale or other transformed quantities.
- Renamed stage-numbered prediction tests to behaviour-based filenames and descriptions.
- Added offline regression tests for original-scale transformed-response predictions and deterministic answer wording.
## WMFM 1.1.0.18

- Reused one source-variable restoration helper for transformed predictors across prediction and factor-comparison confidence interval pathways.
- Corrected model-matrix construction for comparisons involving terms such as log(carat).
- Added behaviour-named regression coverage for transformed predictors in factor-comparison weights.
- Validated through the standard WMFM full package workflow.

## WMFM 1.1.0.14

- Repaired deterministic linear-model prediction for transformed predictors fitted through local app formula environments.
- Bound source-variable prediction values into a safe child terms environment before calling predict.lm().
- Added regression coverage for the Diamonds log-log prediction workflow and source-variable terms resolution.
- Preserved standard predict.lm() estimates, confidence intervals, and prediction intervals.
- Validated through the standard WMFM full package workflow.

## WMFM 1.1.0.013

- Add `listWMFMEvaluationExamples()` for numbered, metadata-backed evaluation example discovery.
- Add `runWMFMEvaluationSuite()` with selection by number, exact name, regular expression, task type, or suite.
- Save per-example diagnostics JSON together with a run manifest, JSONL collection, and CSV summary.
- Display progress, elapsed time, and estimated time remaining while continuing safely after individual example failures.
- Add prediction evaluation metadata to the twelve Course, Oysters, Quakes, and Diamonds examples.
- Extend `runModel()` with an optional follow-up question for reusable non-Shiny evaluation workflows.

## WMFM 1.1.0.012

- Build linear-model prediction data from source variables when fitted terms use supported transformations such as log, log1p, or sqrt.
- Prevent unsupported or malformed transformed predictions from crashing the Shiny app by returning a deterministic needs-input payload.
- Replace generic people-or-cases interval wording with concise response-specific prediction and confidence-interval sentences.
- Preserve factor-adjusted Diamonds predictions using the supplied cut, color, and clarity levels.
- Add focused offline tests for transformed predictors, safe prediction failure, and student-facing interval wording.
  - Repair the focused wording assertion to expect the improved phrase used by the deterministic answer.

## WMFM 1.1.0.010

- Preserve the Stage 43.8 natural expected-value, factor-level, transformed-predictor, and plain-language prediction improvements.
- Replace example-specific binary-factor wording with predictor-driven matching so prediction helpers remain domain agnostic.
- Correct the threshold-safeguard test to call the existing follow-up prompt-control helper.
- Re-run the full offline validation workflow for the accumulated Stage 43.8 changes.

## WMFM 1.1.0.008

- Repair the Stage 43.6 prediction-example validation to use the exported listWMFMExamples() API.
- Retain the twelve loadable Course, Oysters, Quakes, and Diamonds prediction examples unchanged.
- Confirm the examples remain visible and loadable through the package example API.
- Run the complete package validation, archive, build, and installation workflow after the test repair.

## WMFM 1.1.0.006

- Run the complete package validation workflow before using the Stage 43 manual prediction-evaluation examples.
- Confirm the prediction implementation through strict tests and a zero-error, zero-warning, zero-note package check.
- Build and install the exact validated source package for the subsequent developer-mode manual evaluation.
- Record the forced-validation stage without changing the prediction engine or the labelled example prompts.

## WMFM 1.1.0.005

- Add a uniquely labelled manual prediction-evaluation set for the Course, Oysters, Quakes, and Diamonds examples.
- Cover explicit individual predictions, ambiguous personal or practical wording, and explicit mean or expected responses.
- Keep the prompts suitable for developer-mode JSON review without asserting fixed automated answers.
- Preserve Stage 44 for deterministic R code generation after prediction behaviour has been reviewed.

## WMFM 1.1.0.004

- Distinguish mean-response, individual-outcome, and ambiguous personal prediction questions.
- Report both confidence and prediction intervals for supported personal ordinary-linear-model questions.
- Prevent repeated explanation assembly from duplicating deterministic follow-up answers that contain both interval types.
- Preserve a development context for deterministic R code generation and residual-inspection work in Stages 44 and 45.
- Exclude local R history files and stage context notes from source-package builds.
- Continue the WMFM 1.1.0 development line using build-attempt version numbers.

## WMFM 1.1.0.003

- Distinguish mean-response, individual-outcome, and ambiguous personal prediction questions.
- Report both confidence and prediction intervals for supported personal ordinary-linear-model questions.
- Prevent repeated explanation assembly from duplicating deterministic follow-up answers that contain both interval types.
- Preserve a development context for deterministic R code generation and residual-inspection work in Stages 44 and 45.
- Exclude local R history files and stage context notes from source-package builds.
- Continue the WMFM 1.1.0 development line using build-attempt version numbers.





















## WMFM 1.0.4.005

- Added a final CRAN reviewer audit script for documentation, example, global-write, and package-availability checks.
- Removed the remaining triple-colon test call so the full checked source tree is clean for the reviewer audit.
- Validated the audit before and after roxygen documentation regeneration as part of the standard stage workflow.

## WMFM 1.0.4.004

- Replaced the package-data scan so WMFM no longer uses installed.packages() in package code.
- Added deterministic tests for detecting installed package dataset metadata from temporary library directories.
- Added a CRAN preflight check to confirm the Stage 42 reviewer repair stream remains release-ready.

## WMFM 1.0.4.003

- Repaired the Stage 42.2.1 provider-profile save observer so the matched-profile flag is assigned explicitly before status messages are built.
- Removed the CRAN check note about no visible binding for global variable matched in registerChatProviderObservers.
- Kept the repair narrowly scoped to the provider-configuration observer and retained the Stage 42.2 CRAN-audit changes.
- Validated with CRAN reviewer greps, strict tests, and strict check.

## WMFM 1.0.4.001

- Addressed the first CRAN reviewer documentation slice by quoting the shiny package name in DESCRIPTION.
- Removed CRAN-facing examples for internal helpers and eliminated generated triple-colon documentation examples.
- Replaced remaining roxygen dontrun examples with interactive guards where examples should stay visible but not run during checks.
- Regenerated documentation and validated the package with the standard WMFM workflow.

## WMFM 1.0.3.030

- Removed the remaining provider-credential removal control from the legacy provider setup modal.
- Removed the corresponding server observer so the app no longer advertises API-key deletion in the provider settings workflow.
- Updated provider settings tests to assert that API-key replacement is available, while API-key removal is hidden from the normal user flow.


## WMFM 1.0.3.029

- Removed the visible Remove API key control from the provider edit modal.
- Kept API-key replacement through the existing password field: leaving it blank preserves the current credential, while entering a new value replaces it.
- Updated provider settings tests so API-key removal is not advertised in the main edit-provider flow.

## WMFM 1.0.3.028

- Reduced the provider registry plus and minus controls to a smaller 28px square size.
- Tightened the provider registry action strip so action controls sit closer to the table row height.
- Added stronger CSS specificity so Bootstrap button defaults do not make the controls visually oversized.
- Updated provider settings tests to assert the compact action-control dimensions.
## WMFM 1.0.3.027

- Reduced the provider registry plus and minus controls to a smaller 28px square size.
- Tightened the provider registry action strip so action controls sit closer to the table row height.
- Added stronger CSS specificity for provider action buttons so Bootstrap button defaults do not make them visually oversized.
- Updated provider settings tests to assert the compact action-control dimensions.

## WMFM 1.0.3.026

- Expanded the provider registry table and output container so the table fills the rounded settings panel.
- Kept provider columns evenly distributed across the available provider list width.
- Reduced the plus and minus provider action buttons to a consistent compact row-height size.
- Preserved the provider-object settings behaviour while improving the visual layout.
## WMFM 1.0.3.023

- Expanded the provider registry table so it fills the rounded settings panel.
- Integrated provider add and remove controls into the provider list footer.
- Applied consistent monospace sizing to the provider action buttons so plus and minus controls align visually.
- Preserved the provider-object settings behaviour while improving the visual layout.
## WMFM 1.0.3.022

- Normalised editWmfmConfig() return paths so they match getWmfmConfigPath() across platform-specific path aliases.
- Preserved the provider-object settings UI while fixing a config helper path consistency issue.
- Kept the repair limited to the Stage 41.11.3 path-normalisation failure.
## WMFM 1.0.3.018

- Repaired the provider settings developer-mode guard so developer controls are not present when developer mode UI is disabled.
- Updated editWmfmConfig() to accept an injectable editor function for deterministic offline tests.
- Replaced the config editor test's dependency on newer withr mocking helpers with a package-local editor stub.
- Preserved the Stage 41.10 provider registry UI polish and config path helper behavior.
## WMFM 1.0.3.016

- Added a confirmation dialog before removing provider objects from the Settings provider registry.
- Kept the existing safeguard that WMFM must retain at least one configured provider.
- Updated provider settings tests and design notes for the confirmed remove workflow.
- Validated by the standard WMFM full package workflow.
## WMFM 1.0.3.015

- Added an explicit Edit action for provider objects in the Settings provider registry.
- Reused the provider modal for both adding and editing provider profiles, with existing values pre-populated for edits.
- Updated provider settings tests to cover the edit action while keeping credentials out of the main UI.
- Validated by the standard WMFM full package workflow.
## WMFM 1.0.3.014

- Repaired Stage 41.7 provider profile resolution so legacy local configuration remains usable during migration.
- Allowed provider type identifiers such as claude to resolve the corresponding configured provider profile.
- Restored local config precedence for persisted Ollama URLs and models when the active profile does not supply those fields.
- Preserved the provider-object UI redesign while keeping legacy resolver behavior covered by existing tests.
- Validated by the standard WMFM stage workflow.

## WMFM 1.0.3.012

- Repaired Stage 41.6 provider settings tests so they match the provider-object Settings redesign.
- Updated expectations for the simplified active-provider status and provider registry controls.
- Removed obsolete assertions for the old inline Ollama-only settings layout.
- Preserved the Stage 41.6 implementation while restoring the standard WMFM validation workflow.
- Validated by the standard WMFM stage workflow.

## WMFM 1.0.3.008

- Added Stage 41.5 design notes for the provider-as-object Settings redesign.
- Defined the user-facing provider table, active-provider selector, and add/edit/delete modal workflow.
- Recorded that credentials must remain hidden and exposed only as provider status.
- Clarified that Stage 41.6 is the first implementation stage for the redesign.
- No package-code validation was required because this stage only adds design documentation.

## WMFM 1.0.3.007

- Updated README setup guidance for local desktop and deployed WMFM configuration.
- Documented Provider setup as the local desktop path for credential guidance and optional local credential entry.
- Documented usethis::edit_r_environ() for users who prefer standard R environment-variable setup.
- Clarified that deployed Shiny apps are administrator-managed and ordinary users may only choose approved providers or models.
- Updated README coverage tests for the revised setup guide.

## WMFM 1.0.3.006

- Repaired Stage 41.3 provider credential wording so existing provider-setting tests continue to match documented messages.
- Preserved the local desktop credential setup behavior and deployed-app administrator controls added in Stage 41.3.
- Kept Claude missing-credential guidance explicit about ANTHROPIC_API_KEY while mentioning the Provider setup modal.
- Validated through the standard WMFM stage workflow.

## WMFM 1.0.3.004

- Added Stage 41.2.1 provider setup policy helpers so WMFM can distinguish local desktop use from administrator-managed deployments.
- Moved provider credential guidance into a modal setup dialog rather than keeping API-key guidance front and centre in Settings.
- Blocked ordinary deployed-app users from saving provider configuration while preserving local desktop configuration editing.
- Recorded the Stage 41.2.1 implementation decision in the configuration design notes.
- Validated with the standard WMFM full package workflow.

## WMFM 1.0.3.001

- Added Stage 41.1 configuration design principles before changing provider setup code.
- Documented the single-user desktop and deployed Shiny configuration boundaries.
- Recorded security, simplicity, UI, documentation, and testing principles for later implementation stages.
- Skipped package validation because this stage only adds a design note plus the standard version and NEWS updates.

## WMFM 1.0.2.009

- Added a developer-mode metadata box beside the built-in example selector.
- Exposed example audience, model family, difficulty, teaching topic, developer purpose, installed path, and specification path for the selected example.
- Kept classroom mode unchanged by hiding the metadata box unless developer mode is unlocked.
- Added deterministic tests for developer example metadata formatting.
- Validated through the standard full WMFM package workflow.

## WMFM 1.0.2.008

- Add a Stage 40 manual validation checklist for the reorganised example library.
- Record the completed Stage 40 example-file reorganisation steps in the developer plan.
- Confirm that this documentation-only step does not require devtools document, tests, check, build, or install.

## WMFM 1.0.2.007

- Shortened the physically separated internal model-grid example path so package checks no longer report non-portable file names.
- Kept classroom examples at the top level while preserving grouped developer and test fixtures behind metadata-backed lookup.
- Removed the abandoned long `test-fixtures/model-grid` fixture location during installation.
- Validated by the standard WMFM full package workflow.

## WMFM 1.0.2.005

- Added a metadata-backed example detail listing for classroom and developer example-library views.
- Kept the existing example-name listing compatible by deriving it from the richer detail table.
- Added offline tests for metadata fields, developer-example hiding, and stable empty detail columns.
- Validated by the standard full WMFM package workflow.

## WMFM 1.0.2.004

- Prepared Stage 40.3 example loading for future nested example-folder reorganisation.
- Stored package-relative example paths and specification paths in internal example records.
- Updated example lookup so runExample can resolve examples by display name, directory, or specification stem without assuming every example is a direct child of inst/extdata/examples.
- Added regression coverage for nested example record paths before any physical file moves are attempted.

## WMFM 1.0.2.002

- Added an installed example metadata manifest that classifies classroom, developer, and test examples without moving example folders.
- Updated packaged example listing so release-mode choices use explicit audience metadata rather than only the test-name prefix.
- Kept developer and regression examples available when test examples are requested, preserving existing example lookup compatibility.
- Added offline tests for metadata-driven visibility and metadata override behaviour.
- Validated by the standard full WMFM package workflow because R code, tests, and installed example files changed.
## WMFM 1.0.2.001

- Documented the Stage 40.1 example-file reorganisation plan for separating classroom examples from developer and regression examples.
- Recorded the current example groups and a staged metadata-first implementation strategy before moving installed files.
- Preserved package behaviour for this documentation-only step; devtools document, tests, check, build, and install were intentionally skipped.

## WMFM 0.9.9.001

- Started Stage 38 CRAN hardening with beta-style internal build numbering for the 1.0.0 release-preparation stream.
- Moved the completed Stage 37 restart context into dev/Completed so active development contexts stay focused on current work.
- Added a separate future-development ideas context under dev for post-CRAN follow-up work.
- Validated by the standard WMFM stage workflow before committing this entry.

## WMFM 0.3.1.010

- Repair the adjustment-aware scaffold test so it rejects generated model-mechanics language without rejecting instruction text.
- Keep adjustment variables in the background while allowing high-level interaction caveats when a single adjusted estimate would be misleading.
- Keep the interaction wording invariants while allowing the prompt to contain negative guidance about model-structure wording.
- Preserve the existing adjustment governance and deterministic scoring behavior from Stage 37.
- Validated by the Stage 37.6.1 runner with strict tests and check.

## WMFM 0.3.1.008

- Centralized adjustment-aware LLM scoring policy text in reusable helpers.
- Kept scoring prompts aligned around restrained adjusted-for interpretation and forbidden adjustment-level narration.
- Added offline governance tests for the shared adjustment scoring policy helpers.

## WMFM 0.3.1.007

- Aligned deterministic scoring with adjustment-aware restraint for weak interactions involving adjustment variables.
- Allowed research-question-focused adjusted explanations to avoid adjustment-level numeric and cell-by-cell narration.
- Preserved ordinary no-adjustment scoring behavior with a regression test.

## WMFM 0.3.1.006

- Aligned deterministic scoring with adjustment-aware restraint for weak interactions involving adjustment variables.
- Allowed research-question-focused adjusted explanations to avoid adjustment-level numeric and cell-by-cell narration.
- Preserved ordinary no-adjustment scoring behavior with a regression test.

## WMFM 0.3.1.001

- Added adjustment-variable and primary-variable metadata to raw WMFM run records for downstream scoring.
- Forwarded example-spec adjustment metadata through runExample() into each run record.
- Added offline tests covering run-record metadata and example metadata forwarding.
- Preserved existing non-adjustment and follow-up scoring behavior.

## WMFM 0.3.0.007

- Stage 36.5 preserves follow-up scoring context when raw WMFM run records are rebuilt.
- Keeps follow-up prediction type, interval type, future-observation type, extrapolation metadata, and parameter-uncertainty metadata available after rebuilds.
- Adds a deterministic regression test for rebuildWmfmRunRecords with follow-up scoring context.
- Keeps Stage 36.4 developer scoring behaviour unchanged while protecting cached or rebuilt run objects.
- Validated by the standard WMFM stage workflow with deterministic offline tests and strict package check.

## WMFM 0.3.0.006

- Stage 36.4 keeps developer scoring comparison-structure metrics applicable for follow-up answers without factor predictors.
- Preserves numeric-only reference-group metrics as not applicable while allowing follow-up comparisonStructureClear to remain scored.
- Adds a deterministic developer-scoring regression test for follow-up comparison-structure display.
- Keeps Stage 36.3 LLM/fake-provider follow-up scoring behaviour unchanged.
- Validated by the standard WMFM stage workflow with deterministic offline tests and strict package check.

## WMFM 0.3.0.005

- Stage 36.3 extends LLM scoring prompt guidance for follow-up prediction and interval explanations.
- Maps follow-up scoring policy explicitly onto uncertaintyHandlingAppropriate, comparisonStructureClear, and fatalFlawDetected.
- Adds fake-provider tests confirming follow-up context reaches the LLM scoring prompt and cache key.
- Preserves deterministic follow-up scoring behaviour from Stage 36.2.1.
- Validated by the standard WMFM stage workflow with deterministic offline tests and strict package check.

## WMFM 0.3.0.004

- Stage 36.2.1 tightens deterministic scoring for follow-up prediction and interval explanations.
- Distinguishes fitted-mean confidence intervals from future-observation prediction intervals when follow-up context is supplied.
- Adds checks for blocked predictions, extrapolation warnings, Poisson future-count intervals, and logistic Bernoulli outcome framing.
- Preserves ordinary model-explanation scoring when no follow-up context is present.
- Validated by the standard WMFM stage workflow with deterministic offline tests and strict package check.

## WMFM 0.3.0.002

- Stage 36.1.1 repairs missing follow-up context defaults in WMFM run records.
- Ensures ordinary non-follow-up explanations keep scalar NA follow-up fields rather than zero-length NULL fields.
- Preserves the Stage 36.1 follow-up scoring metadata when a follow-up scoring context is supplied.
- Adds regression coverage that raw run records without follow-up context can still be coerced to data frames.
- Validated by the standard WMFM stage workflow with deterministic tests and strict package check.

## WMFM 0.2.9.055

- Stage 35.16.7 repairs a deterministic post-processing splice that could produce ungrammatical multiplicative conclusion sentences.
- Converts conclusion fragments such as an inserted conditional unit-change phrase into a complete student-facing sentence.
- Keeps the response-transformation and back-transformation explanation path unchanged while tightening final text cleanup.
- Adds regression coverage for the diamond-style spliced conclusion artefact while preserving numeric values.
- Validated by the standard WMFM stage workflow.

## WMFM 0.2.9.054

- Stage 35.16.6 polishes recurring student-facing explanation wording after the response-transformation work.
- Adds prompt guidance to avoid spaced percent signs, hyphenated multiplier phrases, and awkward research-question answer wording.
- Adds deterministic post-processing for 95% formatting, hyphenated multiplier phrases, direct research-question answer wording, and sentence capitalization after deterministic rewrites.
- Adds regression coverage for the diamond-style explanation wording artefacts while preserving numeric values.
- Validated by the standard WMFM stage workflow.

## WMFM 0.2.9.053

- Stage 35.16.5 prevents Markdown-escaped dollar signs from leaking into student-facing explanation text.
- Adds prompt guidance telling the model not to write escaped currency symbols such as backslash-dollar amounts.
- Adds deterministic post-processing so any literal escaped dollar signs are displayed as ordinary dollar signs without changing the numeric values.
- Adds regression coverage for escaped-dollar cleanup and debug-rule reporting.
- Validated by the standard WMFM stage workflow.

## WMFM 0.2.9.052

- Stage 35.16.5 prevents Markdown-escaped dollar signs from leaking into student-facing explanation text.
- Adds prompt guidance telling the model not to write escaped currency symbols such as backslash-dollar amounts.
- Adds deterministic post-processing so any literal escaped dollar signs are displayed as ordinary dollar signs without changing the numeric values.
- Adds regression coverage for escaped-dollar cleanup and debug-rule reporting.
- Validated by the standard WMFM stage workflow.

## WMFM 0.2.9.050

- Stage 35.16.4 synchronizes the formula left-hand side to the selected derived response variable whenever derived response metadata is available.
- Preserves the existing right-hand side while ensuring stale inline response expressions no longer control the fitted response.
- Keeps transformation metadata as the source of truth for response-scale interpretation and back-transformation.
- Adds deterministic regression coverage for stale left-hand sides and arbitrary derived response names.
- Validated by the standard WMFM stage workflow.

## WMFM 0.2.9.049

- Stage 35.16.3 repairs derived-response formula synchronization after response selection.
- Resynchronises the model formula when a user selects a stored derived response variable such as `logPrice`.
- Replaces matching inline left-hand-side transformations such as `log(price)` with the recorded derived variable name.
- Keeps existing derived-variable metadata connected to the fitted model and back-transformation payload.
- Adds an offline regression check for the response-selection observer.

## WMFM 0.2.9.048

- Stage 35.16.2 repairs response-transformation wording and interval formatting regressions.
- Allows prompt formatting to keep working when anchored baseline confidence interval bounds are unavailable.
- Omits unavailable interval bounds from prompt lines instead of failing validation.
- Repairs malformed unit-change text such as `Each an increase of one unit...` into natural phrasing such as `Each additional carat...`.
- Keeps the narrow-confidence-interval precision behavior introduced in Stage 35.16.

## WMFM 0.2.9.043

- Stage 35.16.1 repairs response-scale formatting after Stage 35.16.
- Allows prompt quantity formatting to handle missing confidence interval bounds without failing model-prompt construction.
- Fixes malformed each-increase wording cleanup so phrases such as "Each an increase of one unit" are rewritten for student-facing explanations.
- Keeps the response back-transformation and interval precision changes from Stage 35.16.


## WMFM 0.2.9.045

- Stage 35.15 keeps stored derived response variables synchronized with the model formula.
- Replaces a matching inline response transformation on the formula left-hand side with the selected derived response variable name.
- Preserves the existing right-hand side, so examples such as `log(price) ~ log(carat)` become `logPrice ~ log(carat)` after `logPrice` is selected.
- Keeps derived-response metadata connected to fitted models so later back-transformation logic can use the stored transformation record.
- Adds offline tests for formula substitution and non-matching formula preservation.

## WMFM 0.2.9.044

- Stage 35.14.1 polishes multiplicative wording for response back-transformation explanations.
- Steers original-response-scale multiplier phrasing away from awkward `multiplies by` wording and toward `is multiplied by` or `times as high` wording.
- Adds deterministic cleanup for malformed inserted fragments such as `meaning If ... is associated with`.
- Avoids prompting the LLM to state both multiplier and equivalent percentage-change wording unless percentages are specifically requested.
- Adds offline tests for the prompt guidance and post-processing cleanup.

## WMFM 0.2.9.042

- Stage 35.13 tightens response back-transformation prompt control for derived response variables.
- Keeps Both mode focused on original-response-scale fitted values and effects while retaining only brief model-scale context.
- Suppresses transformed-scale formatted quantity blocks when deterministic original-response-scale quantities are available.
- Adds prompt guidance to avoid expected-log-response and log-scale effect wording in student-facing explanations.
- Adds offline tests for Both and Original response-scale prompt behaviour.

## WMFM 0.2.9.041

- Stage 35.12 tightens response back-transformation prompt control for derived response variables.
- Keeps Both mode focused on original-response-scale fitted values and effects while retaining only brief model-scale context.
- Suppresses transformed-scale formatted quantity blocks when deterministic original-response-scale quantities are available.
- Adds prompt guidance to avoid expected-log-response and log-scale effect wording in student-facing explanations.
- Adds offline tests for Both and Original response-scale prompt behaviour.

## WMFM 0.2.9.040

- Stage 35.12 tightens response back-transformation prompt control for derived response variables.
- Keeps Both mode focused on original-response-scale fitted values and effects while retaining only brief model-scale context.
- Suppresses transformed-scale formatted quantity blocks when deterministic original-response-scale quantities are available.
- Adds prompt guidance to avoid expected-log-response and log-scale effect wording in student-facing explanations.
- Adds offline tests for Both and Original response-scale prompt behaviour.

## WMFM 0.2.9.039

- Stage 35.11 adds deterministic response back-transformation payloads for derived response variables.
- Supplies original-response-scale fitted values and safe multiplicative effects to the explanation prompt when an invertible response transformation is available.
- Respects the response-transformation mode selector so model-scale-only explanations do not include original-scale payloads.
- Keeps predictor-only transformations as metadata without treating them as response back-transformations.
- Adds offline tests for available, not-requested, and predictor-only response transformation paths.

## WMFM 0.2.9.038

- Stage 35.10 marks documented s20x package data sets as having data context on the Model tab.
- Treats s20x help files as built-in data context so package data sets display the green Provided status.
- Keeps uploaded-data context behaviour unchanged and preserves the existing data description modal for package data.

## WMFM 0.2.9.037

- Stage 35.9.20 removes the empty sortable title gap from the Model tab variable buckets.
- Hid the empty rank-list title element generated by the sortable widget so bucket contents sit directly under the custom headers.
- Preserved the existing bucket-header alignment and did not change modelling or transformation behaviour.

## WMFM 0.2.9.036

- Stage 35.9.19 tightens spacing inside the variable bucket containers on the Model tab.
- Reduced sortable bucket container padding so the variable, factor, and numeric headers sit closer to the top of their boxes.
- Preserved the existing bucket-header alignment and did not change modelling or transformation behaviour.

## WMFM 0.2.9.035

- Stage 35.9.18 tightens spacing inside the variable bucket containers on the Model tab.
- Reduced sortable bucket container padding so the variable, factor, and numeric headers sit closer to the top of their boxes.
- Preserved the existing bucket-header alignment and did not change modelling or transformation behaviour.

## WMFM 0.2.9.034

- Stage 35.9.17 tightens spacing inside the variable bucket containers on the Model tab.
- Reduced sortable bucket container padding so the variable, factor, and numeric headers sit closer to the top of their boxes.
- Preserved the existing bucket-header alignment and did not change modelling or transformation behaviour.

## WMFM 0.2.9.033

- Stage 35.9.16 fixes source-tree path lookups in tests so strict tests and R CMD check can run from different working directories.
- Added a test helper for locating project files without relying on brittle ../../ relative paths.
- Updated source-file and README guard tests to use the shared helper while keeping tests offline and deterministic.

Some older entries were reconstructed from completed-stage notes where exact historical build numbers were not recoverable from the available source archive. Reconstructed grouped sections use `.9000` version headings and describe feature areas rather than every individual build attempt.

## WMFM 0.2.9.031

- Stage 35.9.14 keeps the Model tab CSS source readable after converting escaped newlines to real source newlines.
- Repairs brittle UI tests so they check durable CSS selectors and properties rather than exact newline formatting.
- Preserves the compact variable-bucket and fit-control layout changes from the preceding UI polish work.
- Leaves model fitting, transformation handling, and public APIs unchanged.
- Validates with the standard WMFM stage workflow.

## WMFM 0.2.9.029

- Stage 35.9.12 compacts the Model tab so the formula box is easier to see on laptop screens.
- Reduces excess vertical padding above the Variables, Factors, and Numeric bucket headers while keeping their alignment intact.
- Reduces the optional follow-up accordion padding without changing its behaviour.
- Leaves model fitting, transformation handling, and public APIs unchanged.
- Validates with the standard WMFM stage workflow.

## WMFM 0.2.9.028

- Stage 35.9.11 tunes the vertical spacing above the Model tab variable bucket headers.
- Keeps the shared Variables, Factors, and Numeric bucket-header alignment introduced in Stage 35.9.10.
- Leaves variable bucket widths, button placement, response transformation controls, and data context controls unchanged.
- Validates with the standard WMFM stage workflow.

## WMFM 0.2.9.027

- Stage 35.9.10 aligns the Model tab variable bucket headers by giving Variables, Factors, and Numeric the same header wrapper.
- Keeps the Add variable button inside the Variables header while preventing it from shifting that header out of line with the other buckets.
- Updates the UI layout test to check the shared bucket-header structure rather than exact visual coordinates.
- Removes the stale stage-named UI test during installation if it is still present from earlier repairs.
- Validates with the standard WMFM stage workflow.

## WMFM 0.2.9.026

- Stage 35.9.9 relaxes the Model tab UI layout test so it checks durable controls rather than rendered text order.
- Removes brittle substring-window assertions for response transformation and interaction controls.
- Keeps the behavioural checks that the response transformation choices, interaction UI, add-variable button, and data-context status controls exist.
- No app behaviour or public API changes are introduced in this repair.
- Validates with the standard WMFM stage workflow.

## WMFM 0.2.9.022

- Stage 35.9.5 polishes the Model tab layout for derived variables, response-transformation controls, interactions, and data-context status.
- Shortens the data-context status labels to Provided and Not provided and keeps the status beside the edit button.
- Aligns the variable bucket headers and keeps the Add variable button separated from the Variables heading.
- Aligns the response-transformation and interactions controls so their headings and input boxes start on matching rows.
- Updates UI tests to check structural layout intent rather than brittle screenshot-specific CSS details.
- Validates with the standard WMFM stage workflow.

## WMFM 0.2.9.021

- Stage 35.9.4 polishes the Model tab layout for derived variables, response-transformation controls, interactions, and data-context status.
- Shortens the data-context status labels to Provided and Not provided and keeps the status beside the edit button.
- Aligns the variable bucket headers and keeps the Add variable button separated from the Variables heading.
- Aligns the response-transformation and interactions controls so their headings and input boxes start on matching rows.
- Updates UI tests to check structural layout intent rather than brittle screenshot-specific CSS details.
- Validates with the standard WMFM stage workflow.

## WMFM 0.2.9.019

- Stage 35.9.2 relaxes the data-context UI regression test so it checks structure rather than an exact CSS fragment.
- Keeps the test focused on the intended layout: the data-context button and status are rendered in the same inline control container.
- Avoids brittle expectations such as a required white-space CSS declaration, so later UI layout changes are less likely to break unrelated tests.
- No user-facing behavior changes are intended.
- Validates with the standard WMFM stage workflow.

## WMFM 0.2.9.017

- Stage 35.9 keeps the derived-variable teaching-summary UI test offline by disabling explanation generation for its model fixture.
- Updated the app-model-explanation UI test to fit the model, build the audit, and render the teaching summary without contacting a chat provider.
- Preserved normal runModel() behaviour by leaving explanation generation enabled by default for ordinary use.
- Reduced slow and CRAN-hostile test paths by avoiding unnecessary LLM prompt generation in this deterministic test.
- Validates with the standard WMFM stage workflow.

## WMFM 0.2.9.016

- Stage 35.8 keeps variable-transformation tests offline by allowing runModel() to skip explanation generation.
- Added a generateExplanation argument so deterministic tests can fit models, build audits, and inspect metadata without contacting a chat provider.
- Updated variable-transformation tests to disable explanation generation and added a guard test that fails if a chat provider is requested.
- Preserved normal runModel() behaviour by leaving explanation generation enabled by default for ordinary use.
- Validates with the standard WMFM stage workflow.

## WMFM 0.2.9.015

- Stage 35.7 moves the Add variable button into the Variables bucket header so derived-variable creation is colocated with the variable list.
- Shortens the response transformation selector labels and adds an info icon explaining the model-scale, original-scale, and both-scales options.
- Moves the response transformation selector up beside the interaction header to reduce unused vertical space on the Model tab.
- Places the data context status beside the data context button for a more compact response-selection row.
- Validates with the standard WMFM stage workflow.

## WMFM 0.2.9.014

- Stage 35.6 moves the Add derived variable button onto the same row as the Interactions optional header on the Model tab.
- Adds a response transformation handling selector with model-scale, original-scale, and both-scales options for later response back-transformation work.
- Stores the selected response transformation mode on fitted model metadata without changing prediction, confidence interval, or explanation calculations yet.
- Keeps predictor-derived-variable transformations distinct from response back-transformation handling.
- Validates with the standard WMFM stage workflow.

## WMFM 0.2.9.013

- Stage 35.5.1 repairs a stale no-transformation teaching-summary test expectation.
- Keeps the refined student-facing no-transformation wording introduced in Stage 35.5.
- Confirms that derived-variable evidence rows remain conditional on recorded derived-variable transformations.
- Preserves the metadata-only scope with no prediction, confidence interval, or automatic back-transformation behaviour changes.
- Validates with the standard WMFM stage workflow.

## WMFM 0.2.9.011

- Stage 35.4.5 repairs the derived-variable teaching-summary test regressions left by the previous repair.
- Restores the stable checklist shape for models that do not use derived variables while still showing a derived-variable row when transformation records are present.
- Suppresses expected perfect-fit warnings in the new derived-variable teaching-summary tests so strict warning-as-error validation remains focused on real regressions.
- Preserves the metadata-only scope with no prediction, confidence interval, explanation text, or automatic back-transformation behaviour changes.
- Validates with the standard WMFM stage workflow.

## WMFM 0.2.9.005

- Stage 35.3.1 repairs the derived-variable transformation helper wiring used by the deterministic explanation audit.
- Add the missing audit-table helper and variable-transformation prompt block so existing explanation-audit tests can run.
- Preserve the Stage 35.3 metadata-only scope with no prediction, confidence interval, explanation text, or automatic back-transformation behaviour changes.
- Validate through offline explanation-audit and variable-transformation tests plus the standard WMFM stage workflow.

## WMFM 0.2.9.003

- Stage 35.2 carries derived-variable transformation records from the creation registry into fitted model state.
- Add helpers that select formula-relevant transformation records and attach them to fitted models using a dedicated WMFM attribute.
- Extend runModel() and wmfmModel objects so command-line workflows can preserve the same transformation metadata used by the app.
- Keep the change metadata-only: no prediction, confidence interval, explanation, or automatic back-transformation behaviour is changed yet.
- Validate through offline transformation-registry tests and the standard WMFM stage workflow.

## WMFM 0.2.9.002

- Stage 35.1.1 repairs derived-variable transformation evaluation for the Stage 35.1 metadata registry.
- Add log10 and arithmetic operators to the safe derived-variable evaluation environment so recognised expressions can be evaluated before metadata is recorded.
- Preserve the conservative transformation metadata design introduced in Stage 35.1 without changing model fitting, prediction, confidence interval, or back-transformation behaviour.
- Validate through the existing offline variable-transformation tests and the standard WMFM stage workflow.

## WMFM 0.2.8.012

- Recorded the June 2026 branch audit: `backup_sync` and `backup-before-sync` are retained as recovery/archive branches, and `grading-scoring-revision` was audited and tagged as archived.
- Confirmed that no significant unmerged WMFM functionality was identified from the Stage 32 branch investigation.
- No merge or cherry-pick action is required for the audited historical branches.

- Complete the Stage 33 example-coupling close-out with low-risk documentation cleanup only.
- Replace remaining built-in-example wording in internal helper documentation with neutral outcome and predictor examples.
- Add a close-out note documenting the durable domain-agnostic regression coverage and final audit command.
- Preserve production interpretation behavior while keeping NEWS entries version-numbered and newest first.
- Validate by the standard WMFM stage workflow.


## WMFM 0.2.8.011

- Repair intercept-only research-question normalization after the domain-specific prompt cleanup.
- Treat sample data and data sample as generic sample-level wording before converting to relevant-setting language.
- Preserve setting-generic wording without reintroducing course-specific prompt rules.
- Validate the repair through the standard stage script workflow.

## WMFM 0.2.8.009

- Remove attendance-specific semantic factor matching from production prediction parsing.
- Keep follow-up prediction parsing based on explicit model variable names and fitted factor levels.
- Replace example-specific binomial documentation examples with generic response-level wording.
- Extend domain-agnostic regression coverage for prediction helpers and binomial notation helpers.

## WMFM 0.2.8.008

- Rebuild `NEWS.md` with a purpose section at the top.
- Normalize top-level entries to WMFM version-number headings rather than stage headings.
- Reorder package news from newest to oldest and group unrecoverable older stage history under reconstructed version sections.
- Preserve available historical change notes while documenting reconstruction limits.

## WMFM 0.2.8.007

- Replace course-specific equation-prompt examples with neutral symbolic examples.
- Keep the teaching data-context placeholder wording intact while avoiding new production coupling.
- Remove course-specific follow-up detection cues from production question handling.
- Support generic prediction requests written as variable-is-level conditions, such as what happens if group is B.
- Extend domain-agnostic regression coverage for prompt text and generic prediction wording.

## WMFM 0.2.8.003

- Rename the temporary coupling regression test to a permanent domain-agnostic behaviour test.
- Replace remaining domain-specific examples in production prompt and scoring-description text with generic examples.
- Generalize deterministic parser regression coverage so anti-coupling checks no longer depend on earthquake or diamond examples.
- Preserve built-in example content in example fixtures and focused example tests only.

## WMFM 0.2.8.002

- Remove example-specific production handling from selected follow-up classification and prediction helpers.
- Replace magnitude-specific unit-change cleanup with predictor-agnostic unit-change phrasing.
- Add regression tests for generic named-unit requests and model-level factor matching.
- Preserve example-specific wording in tests and example fixtures only.

## WMFM 0.2.7.9000

This section reconstructs Stage 31 and Stage 32 development history from the available package news. Exact build numbers for these entries were not recoverable from the source archive.

- Reworked the log-log polishing repair so production code uses only generic log-log wording patterns.
- Removed example-specific post-processing for particular variable names from the pending repair.
- Added generic coverage for raw log-scale estimate sentences, unsupported doubling examples, and coefficient-scale uncertainty leakage.
- Kept anchored fixed-unit follow-up wording while preserving percentage-change interpretation for log-log models.
- Refined log-log unit-change follow-up wording so fixed carat changes keep the reference point and use carat units.
- Tightened log-log explanation guidance to avoid raw coefficient estimates and unsupported doubling examples in student-facing text.
- Added post-processing coverage for model-estimate sentence variants seen in log-log diagnostics.
- Preserved interval dash formatting while keeping log-log uncertainty on the percentage-change scale.
- Allowed the log-log percentage-language post-processing rule to bypass numeric-token preservation while keeping the guard active for other rules.
- Fixed adjusted-estimate confidence interval rewrites so coefficient-scale wording can be converted to percentage-change wording.
- Strengthened log-log guidance so student-facing explanations use percentage-change interpretations.
- Added post-processing for recurring raw log-predictor and log-response coefficient wording.
- Added deterministic tests for percentage-change wording and log-scale leakage cleanup.
- Standardised compact confidence interval labels in student-facing post-processing.
- Rewrote additional log-scale mechanism wording toward proportional-change language.
- Expanded deterministic tests for compact interval labels and log-scale wording leakage.
- Updated adjustment and contrast text to prefer `95% confidence interval` over compact `95% CI` wording.
- Polished deterministic adjustment-comparison follow-up wording so student-facing answers no longer begin with a bare yes.
- Restated the adjusted-versus-weight-only conclusion in natural language before the caution about in-sample model comparison.
- Updated log-log tests to require improved adjustment-comparison wording and to keep technical diagnostics out of the appended student answer.
- Added a download button for the current student-facing Model plots view.
- Exported the selected observed-vs-fitted or residuals-vs-fitted plot as a deterministic PNG file.
- Reused the same plot helper so smoothing and density-aware point display are preserved in downloaded plots.
- Kept model-plot downloads separate from model explanations and avoided diagnostic or assumption-check wording.
- Added deterministic tests for download filenames, UI wiring, and server wiring.
- Added density-aware point transparency to student-facing model plots without adding new plot types.
- Kept the alpha choice automatic so students do not need to tune plotting parameters.
- Applied lighter points to larger datasets while preserving the existing red reference lines and blue smoother behaviour.
- Added concise student-facing wording that explains lighter points for larger datasets without diagnostic framing.
- Added deterministic tests for alpha thresholds, plot layer alpha values, and cautious wording.
- Added an optional blue smooth trend to linear-model observed-vs-fitted and residuals-vs-fitted model plots.
- Kept the smoother automatic, with no user control over the smoothing parameter.
- Drew plot layers in the order points, blue smoother, then red reference line so the reference remains visible.
- Kept the smoother unavailable for unsupported model families and safely ignored for GLM plots.
- Added deterministic tests for smoother layers, UI controls, and cautious student-facing wording.

## WMFM 0.2.6.9000

This section reconstructs Stage 30 development history from the available package news. Exact build numbers for these entries were not recoverable from the source archive.

- Styled the Model plots information control to match the existing provider-settings help control and open as a small modeless details panel.
- Kept the teaching note next to the Model plots heading instead of rendering it as a full-width visible block.
- Added deterministic UI tests for the Model plots information control classes and text.
- Replaced the Model plots hover-only information icon with an expandable information control so the teaching note is visible when clicked.
- Preserved line styling and developer-tab ordering changes.
- Added deterministic UI tests for Model plots information text.
- Added the Model plots teaching note as an information icon next to the tab heading.
- Made linear and Poisson reference lines thicker so they remain visible over the points.
- Used a dashed logistic trend curve on the observed-outcome versus predicted-probability plot.
- Moved the developer Scoring and Grading tab before Settings so developer tabs stay at the end.
- Moved the Model plots tab immediately after Model Explanation and kept the existing Plot tab next to it.
- Drew red reference lines after points for linear and Poisson model plots so the guide remains visible.
- Added a red logistic trend curve to the observed-outcome versus predicted-probability plot.
- Extended offline tests for tab ordering and plot-layer behavior.
- Added a short deterministic summary sentence above each Model plots figure.
- Reported the number of plotted observations and the fitted-value or residual scale used by the selected plot.
- Kept summary text student-facing and separate from diagnostic pass/fail language.
- Extended offline tests for the summary helper and new UI output binding.
- Hardened the Model plots tab for unsupported fitted model families so unavailable choices do not trigger plotting errors.
- Added model-aware plot metadata and ggplot labels based on the fitted response variable.
- Kept student-facing wording focused on visual summaries and missing structure rather than assumption checking.
- Extended offline tests for unsupported models and model-aware plot labels.
- Added a student-facing Model plots tab for fitted models.
- Added deterministic observed-vs-fitted and residuals-vs-fitted model-plot helpers.
- Supported lm, binomial logistic glm, and poisson glm model-plot data on appropriate fitted-value scales.
- Added short teaching notes that frame plots as visual summaries rather than assumption checks.
- Added offline tests for model-plot data, labels, residual types, and cautious UI language.

## WMFM 0.2.5.017

- Make the Provider settings information icon expandable/clickable by replacing the title-only tooltip with an inline details panel.
- Keep provider configuration guidance close to the Provider settings heading without showing the longer text permanently on the settings page.

## WMFM 0.2.5.016

- Move provider-setting guidance into an information tooltip beside the Provider settings heading so the settings page is less cluttered.
- Remove the redundant current-provider text and rely on the active provider selection box as the visible source of truth.
- Remove the Apply provider button so changing provider selection immediately updates and saves the non-secret provider preference.
- Show configuration guidance when a selected provider is not usable: Ollama uses a modal configuration prompt, while commercial providers use a warning notification when the required API key is missing.

## WMFM 0.2.5.015

- Restore password protection for the maintainer-only developer-mode toggle while preserving the local developer-mode preference after a successful unlock.
- Keep developer-mode unlock quiet by avoiding the previous success notification.
- Render explanation prompt diagnostics through the same accordion construction as the student-facing support panels.
- Remove the duplicate standalone diagnostics heading from the diagnostics panel content so the accordion heading provides the visual affordance.

## WMFM 0.2.5.012

- Repair `NEWS.md` so every top-level news section has a version-like heading that can be parsed by `R CMD check`.
- Keep the reconstructed pre-Stage 29 history in the package news without using non-version section titles.

## WMFM 0.2.5.011

- Persist developer-mode enabled/disabled state in the local WMFM config when the maintainer-only developer-mode UI is exposed.
- Persist provider preferences whenever the selected provider or non-secret provider settings change.
- Avoid Ollama model discovery when the active or preferred provider is not Ollama.
- Add the initial `NEWS.md` file reconstructed from completed stage notes and the Stage 29 workflow.

## WMFM 0.2.5.010

- Hide developer-mode controls unless `WMFM_SHOW_DEVELOPER_MODE=1` is set in the local R environment.
- Document developer-mode setup in `dev/developer-mode.md` rather than in the public-facing README.
- Add README guidance for configuring an AI provider before first use.
- Clarify that commercial API providers require paid API credits and API keys in `.Renviron`; a ChatGPT Plus subscription is not sufficient for OpenAI API use.
- Add startup guidance when no usable AI provider has been configured.
- Repair provider-config tests so blank options fall back to package defaults.

## WMFM 0.2.5.008

- Replace the developer-mode lock/unlock buttons with a compact slider-style toggle.
- Make the developer toggle visually distinct when on or off, without showing a checkbox tick.
- Update tests to target the custom toggle markup rather than the previous Shiny `checkboxInput()` implementation.
- Continue separating provider settings from developer-mode state.

## WMFM 0.2.5.006

- Introduce the first version of the developer-mode toggle UI.
- Keep developer mode behind a local opt-in path for maintainers.
- Remove the unlocked developer-mode notification that became redundant once the toggle showed state directly.
- Repair tests for the new red/green toggle styling and startup observer wiring.

## WMFM 0.2.5.004

- Correct the Stage 29 build-number recovery path so subsequent Stage 29 work resumes from the intended `0.2.5.xxx` sequence.

## WMFM 0.2.5.003

- Add `scripts/checkRStringEscapes.R`, a fast preflight scanner for suspicious R string escapes in source and tests.
- Add offline tests for the string-escape scanner.
- Update generated stage runners so the escape scan runs before the slower documentation, test, and check workflow.
- Change the runner workflow so failed build attempts still consume the next `0.2.5.xxx` build number.

## WMFM 0.2.5.001

- Polish UI details that were not specific to log-log modelling.
- Make the optional follow-up placeholder shorter and visually lighter.
- Make explanation prompt diagnostics look and behave like the other accordion sections.
- Begin provider-preference and developer-mode persistence work.
- Harden Ollama discovery so failures are handled gracefully and tests do not require a real Ollama service.
- Rename lasting provider test files to remove stage numbers from durable test names.

## WMFM 0.2.4.9000

This section reconstructs earlier development history from completed-stage notes. It is intentionally grouped by feature area rather than by exact release number.

- Add and refine log-log model support, including power-law interpretation and Diamonds II, Diamonds III, and Diamonds IV examples.
- Improve log-log plotting and explanatory guidance for transformed predictors and responses.
- Add deterministic support for adjustment-variable follow-up questions, adjustment comparisons, and follow-up scaffolding.
- Build the structured follow-up-question architecture for predictions, clarification, bounded inputs, and model-aware parsing.
- Add deterministic linear-model and GLM follow-up prediction support.
- Add support for unit-change questions and natural-language parsing of requested predictor changes.
- Add GLM extrapolation policy, diagnostics, and follow-up framing repairs for logistic and Poisson models.
- Add adjustment-variable model specification, prompt payloads, and explanation behaviour.
- Harden adjustment-variable regression handling and interaction guardrails.
- Inventory and progressively refactor app-server responsibilities toward smaller server helper modules and thinner orchestration in the main app server.
- Add diagnostic reports and planning notes for explanation and scoring development.
- Continue GLM explanation work and logistic interaction prompt payload repairs.
- Improve confidence-interval and explanation payload handling for model-specific outputs.
- Improve answer selection, explanation cleanup, and prompt validation.
- Add prompt input audits and prompt/explanation integration refinements.
- Add teaching-summary construction, data contracts, and validation checklists.
- Add a developer-mode feedback system for scoring and explanation diagnostics.
- Define the explanation-audit contract and audit inventory.
- Reorganize package source files into feature-oriented modules.
- Improve data-context handling, loading progress feedback, UI cleanup, and pedagogical stability planning.
