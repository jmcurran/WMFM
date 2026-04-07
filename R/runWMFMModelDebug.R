#' Fit a WMFM model and generate console outputs without launching Shiny
#'
#' Backward-compatible wrapper around [runModel()]. This keeps the previous
#' command-line entry point available while the package transitions to the more
#' general `wmfmModel` workflow.
#'
#' @inheritParams runModel
#'
#' @return Invisibly returns an object of class `wmfmModel`.
#' @export
runWMFMModelDebug = function(
    data,
    formula,
    modelType = c("lm", "logistic", "poisson"),
    dataContext = NULL,
    ollamaBaseUrl = NULL,
    printOutput = TRUE,
    useExplanationCache = TRUE
) {
  runModel(
    data = data,
    formula = formula,
    modelType = modelType,
    dataContext = dataContext,
    ollamaBaseUrl = ollamaBaseUrl,
    printOutput = printOutput,
    useExplanationCache = useExplanationCache
  )
}
