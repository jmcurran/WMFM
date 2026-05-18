#' Create app server reactive state
#'
#' @return A list containing the app server reactive values and reactive holders.
#'
#' @keywords internal
#'
#' @importFrom shiny reactiveValues reactiveVal
createAppServerReactiveState = function() {
  rv = reactiveValues(
    data = NULL,
    allVars = character(0),
    autoFormula = "",
    modelEquations = NULL,
    modelExplanation = NULL,
    modelExplanationAudit = NULL,
    modelExplanationTutor = NULL,
    modelExplanationMessage = NULL,
    bucketGroupId = 0,
    lastResponse = NULL,
    lastFactors = character(0),
    pendingFactorVar = NULL,
    pendingExampleInteractions = character(0),
    chatProvider = NULL,
    contrastLlmCache = new.env(parent = emptyenv()),
    modelContext = NULL,
    bucketFactors = character(0),
    bucketContinuous = character(0),
    adjustmentVariables = character(0),
    isResetting = FALSE,
    activeChatBackend = "ollama",
    activeOllamaModel = "gpt-oss",
    activeOllamaThinkLow = FALSE,
    availableOllamaModels = "gpt-oss",
    userDatasetContext = "",
    researchQuestion = "",
    loadedExample = NULL
  )

  list(
    rv = rv,
    modelFit = reactiveVal(NULL),
    contrastPairs = reactiveVal(character(0)),
    contrastResultText = reactiveVal("")
  )
}
