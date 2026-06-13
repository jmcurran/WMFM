#' WMFM: fitted-model teaching support
#'
#' WMFM provides a Shiny application and supporting helpers for teaching fitted
#' linear and generalised linear models. The package is designed to help learners
#' connect model formulae, fitted equations, plots, confidence intervals, and
#' plain-language interpretation.
#'
#' The main interactive entry point is [runWMFMApp()]. Programmatic workflows can
#' use [runModel()] for fitted-model output and [runExample()] for packaged
#' teaching examples.
#'
#' AI-backed explanations are optional. External providers are configured by the
#' user through [getChatProvider()] or the app settings, and are not required for
#' package loading, examples, tests, or deterministic workflows.
#'
#' @section Teaching workflow:
#' A typical classroom workflow is to launch the app with [runWMFMApp()], load or
#' select data, define a research question, fit a model, and compare the fitted
#' equations, plots, intervals, and explanation text. Programmatic examples can
#' be listed with [listWMFMExamples()] and run with [runExample()].
#'
#' @section External-provider behavior:
#' WMFM does not contact AI providers during package loading. Provider access is
#' only attempted after a user explicitly configures a provider and requests an
#' AI-backed explanation or scoring workflow. Tests and CRAN checks are expected
#' to run offline.
#'
#' @keywords internal
"_PACKAGE"
