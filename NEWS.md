# WMFM development news

## Purpose

This file records user-facing and developer-facing changes in WMFM. It is a release-note summary, not a commit-by-commit history. Entries are ordered newest to oldest and use WMFM version-number headings.

Some older entries were reconstructed from completed-stage notes where exact historical build numbers were not recoverable from the available source archive. Reconstructed grouped sections use `.9000` version headings and describe feature areas rather than every individual build attempt.

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
