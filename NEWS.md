# WMFM development news

## Stage 30.1

- Add a student-facing Model plots tab for fitted models.
- Add deterministic observed-vs-fitted and residuals-vs-fitted model-plot helpers.
- Support lm, binomial logistic glm, and poisson glm model-plot data on appropriate fitted-value scales.
- Add short teaching notes that frame plots as visual summaries rather than assumption checks.
- Add offline tests for model-plot data, labels, residual types, and cautious UI language.


This file records user-facing and developer-facing changes reconstructed from
completed stage notes and the Stage 29 workflow. It is a release-note summary,
not a commit-by-commit history.

## WMFM 0.2.5.017

* Make the Provider settings information icon expandable/clickable by replacing the title-only tooltip with an inline details panel.
* Keep provider configuration guidance close to the Provider settings heading without showing the longer text permanently on the settings page.

## WMFM 0.2.5.016

* Move provider-setting guidance into an information tooltip beside the Provider
  settings heading so the settings page is less cluttered.
* Remove the redundant current-provider text and rely on the active provider
  selection box as the visible source of truth.
* Remove the Apply provider button so changing provider selection immediately
  updates and saves the non-secret provider preference.
* Show configuration guidance when a selected provider is not usable: Ollama uses
  a modal configuration prompt, while commercial providers use a warning
  notification when the required API key is missing.

## WMFM 0.2.5.015

* Restore password protection for the maintainer-only developer-mode toggle while
  preserving the local developer-mode preference after a successful unlock.
* Keep developer-mode unlock quiet by avoiding the previous success notification.
* Render explanation prompt diagnostics through the same accordion construction as
  the student-facing support panels.
* Remove the duplicate standalone diagnostics heading from the diagnostics panel
  content so the accordion heading provides the visual affordance.

## WMFM 0.2.5.012

* Repair `NEWS.md` so every top-level news section has a version-like heading
  that can be parsed by `R CMD check`.
* Keep the reconstructed pre-Stage 29 history in the package news without using
  non-version section titles.

## WMFM 0.2.5.011

* Persist developer-mode enabled/disabled state in the local WMFM config when the
  maintainer-only developer-mode UI is exposed.
* Persist provider preferences whenever the selected provider or non-secret
  provider settings change.
* Avoid Ollama model discovery when the active or preferred provider is not
  Ollama.
* Add the initial `NEWS.md` file reconstructed from completed stage notes and the
  Stage 29 workflow.

## WMFM 0.2.5.010

* Hide developer-mode controls unless `WMFM_SHOW_DEVELOPER_MODE=1` is set in the
  local R environment.
* Document developer-mode setup in `dev/developer-mode.md` rather than in the
  public-facing README.
* Add README guidance for configuring an AI provider before first use.
* Clarify that commercial API providers require paid API credits and API keys in
  `.Renviron`; a ChatGPT Plus subscription is not sufficient for OpenAI API use.
* Add startup guidance when no usable AI provider has been configured.
* Repair provider-config tests so blank options fall back to package defaults.

## WMFM 0.2.5.008

* Replace the developer-mode lock/unlock buttons with a compact slider-style
  toggle.
* Make the developer toggle visually distinct when on or off, without showing a
  checkbox tick.
* Update tests to target the custom toggle markup rather than the previous Shiny
  `checkboxInput()` implementation.
* Continue separating provider settings from developer-mode state.

## WMFM 0.2.5.006

* Introduce the first version of the developer-mode toggle UI.
* Keep developer mode behind a local opt-in path for maintainers.
* Remove the unlocked developer-mode notification that became redundant once the
  toggle showed state directly.
* Repair tests for the new red/green toggle styling and startup observer wiring.

## WMFM 0.2.5.004

* Correct the Stage 29 build-number recovery path so subsequent Stage 29 work
  resumes from the intended `0.2.5.xxx` sequence.

## WMFM 0.2.5.003

* Add `scripts/checkRStringEscapes.R`, a fast preflight scanner for suspicious R
  string escapes in source and tests.
* Add offline tests for the string-escape scanner.
* Update generated stage runners so the escape scan runs before the slower
  documentation, test, and check workflow.
* Change the Stage 29 runner workflow so failed build attempts still consume the
  next `0.2.5.xxx` build number.

## WMFM 0.2.5.001

* Polish Stage 28 UI details that were not specific to log-log modelling.
* Make the optional follow-up placeholder shorter and visually lighter.
* Make explanation prompt diagnostics look and behave like the other accordion
  sections.
* Begin provider-preference and developer-mode persistence work.
* Harden Ollama discovery so failures are handled gracefully and tests do not
  require a real Ollama service.
* Rename lasting provider test files to remove stage numbers from durable test
  names.

## WMFM 0.2.4.9000

This section reconstructs earlier development history from completed-stage notes.
It is intentionally grouped by feature area rather than by exact release number.

* Add and refine log-log model support, including power-law interpretation and
  Diamonds II, Diamonds III, and Diamonds IV examples.
* Improve log-log plotting and explanatory guidance for transformed predictors
  and responses.
* Add deterministic support for adjustment-variable follow-up questions,
  adjustment comparisons, and follow-up scaffolding.
* Build the structured follow-up-question architecture for predictions,
  clarification, bounded inputs, and model-aware parsing.
* Add deterministic linear-model and GLM follow-up prediction support.
* Add support for unit-change questions and natural-language parsing of requested
  predictor changes.
* Add GLM extrapolation policy, diagnostics, and follow-up framing repairs for
  logistic and Poisson models.
* Add adjustment-variable model specification, prompt payloads, and explanation
  behaviour.
* Harden adjustment-variable regression handling and interaction guardrails.
* Inventory and progressively refactor app-server responsibilities toward
  smaller server helper modules and thinner orchestration in the main app server.
* Add diagnostic reports and planning notes for explanation and scoring
  development.
* Continue GLM explanation work and logistic interaction prompt payload repairs.
* Improve confidence-interval and explanation payload handling for model-specific
  outputs.
* Improve answer selection, explanation cleanup, and prompt validation.
* Add prompt input audits and prompt/explanation integration refinements.
* Add teaching-summary construction, data contracts, and validation checklists.
* Add a developer-mode feedback system for scoring and explanation diagnostics.
* Define the explanation-audit contract and audit inventory.
* Reorganize package source files into feature-oriented modules.
* Improve data-context handling, loading progress feedback, UI cleanup, and
  pedagogical stability planning.
