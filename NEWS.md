# WMFM news

## WMFM 1.0.0

This is the first CRAN-ready release of WMFM, a teaching-focused R package and Shiny application for fitting statistical models and helping students interpret them in plain language.

### User-facing changes

- Added a classroom-oriented Shiny workflow for loading data, fitting common regression models, viewing fitted equations, and generating student-facing explanations.
- Added support for linear, logistic, and Poisson model explanations, including fitted values, confidence intervals, contrasts, interactions, and response-scale interpretations where applicable.
- Added deterministic follow-up question support for supported prediction and interval questions, with safeguards for unsupported requests and extrapolative predictions.
- Added adjustment-variable workflows so users can ask research-question-focused questions while controlling for background variables.
- Added teaching-oriented examples and documentation that describe intended classroom use, model setup, provider configuration, and offline deterministic workflows.

### AI provider behavior

- External AI providers are optional and user-configured.
- Loading the package, running deterministic model workflows, and running package checks do not require credentials, internet access, or a running Ollama server.
- Ollama defaults to a user-managed local endpoint, and model discovery is only performed after explicit user action.
- Tests and examples are designed to avoid live provider calls during package checks.

### Developer-facing changes

- Added deterministic scoring, grading, and comparison helpers for evaluating generated explanations.
- Added offline tests for provider boundaries, deterministic scoring, follow-up handling, adjustment-variable behavior, and CRAN-safe example usage.
- Added package-level documentation, clearer help pages for main entry points, and public README guidance for classroom and CRAN-safe use.
- Cleaned development-stage naming from tests and recorded remaining source-file organization follow-up work for post-release maintenance.

### Release preparation

- Hardened package metadata, documentation, examples, tests, and provider defaults for CRAN submission.
- Completed local CRAN-style preflight checks on the release-preparation branch before setting the public release version to 1.0.0.

## Earlier development history

Detailed pre-release development history was condensed before the 1.0.0 release so this file can serve as public release notes rather than a stage-by-stage development log. The project repository retains the detailed commit history and archived development contexts for maintainers who need to inspect earlier implementation stages.
