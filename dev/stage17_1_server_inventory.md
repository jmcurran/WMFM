# Stage 17.1 server inventory

## Purpose

This note records the first pass over the monolithic app server before any larger module extraction. The aim is to make Stage 17.1 a safe refactor step that establishes the pattern for moving small, pure helpers out of `R/app-server.R` while preserving app behaviour.

## Current server shape

- `R/app-server.R` is approximately 3,981 lines in the Stage 16.5.2 code base.
- The server mixes startup data-source setup, developer-mode controls, tab rendering, plotting, contrast management, data loading, bucket management, formula construction, model fitting, fitted-equation display, fitted-means display, explanations, and model-summary output.
- Several reusable helper files already exist, including `R/app-output-messages.R`, `R/app-modelOutput-serverHelpers.R`, and multiple UI-helper files. Stage 17 should continue this pattern rather than introducing a large rewrite.

## Main responsibility blocks observed

- Lines 38-577: startup package/example loading, developer mode, data-source controls, and chat-provider controls.
- Lines 578-793: main tab UI rendering.
- Lines 794-837: fitted model plot output.
- Lines 838-1876: contrasts UI, contrast state, and contrast computations.
- Lines 1877-2056: symbolic model formula rendering.
- Lines 2057-2334: formula checker and data-loading pathways.
- Lines 2335-2499: bucket UI and interaction UI.
- Lines 2490-2648: dataset help and model-help modal.
- Lines 2649-3113: bucket state, response picker, formula population, and numeric-as-factor confirmation.
- Lines 3114-3506: formula validation and model fitting.
- Lines 3507-3960: reset, fitted equations, fitted means, and model explanation.
- Lines 3961-end: model summary output.

## Stage 17.1 extraction

The first extraction is intentionally small and low risk. It moves startup/status message wording into pure helper functions in `R/app-server-startup-helpers.R` and updates the server to call those helpers.

This is useful because:

- the moved logic is deterministic and easy to test;
- no Shiny reactivity is changed;
- the extracted helpers demonstrate the Stage 17 pattern of shrinking the server through behaviour-preserving steps;
- future work can continue by moving complete feature blocks into helpers or Shiny modules.

## Recommended next candidates

1. Extract package/example startup orchestration into a dedicated server helper or module.
2. Extract data-loading event handlers into a focused data-source module.
3. Extract formula/bucket synchronisation after the data-source code is isolated.
4. Leave model fitting and explanation generation until the data-flow boundaries are clearer.
