#' Create app server reactive state
#'
#' @return A list containing the app server reactive values and reactive holders.
#'
#' @keywords internal
#'
#' @importFrom shiny reactiveValues reactiveVal
createAppServerReactiveState = function() {
  providerDefaults = resolveWmfmProviderConfig()
  if (identical(providerDefaults$backend, "claude")) {
    providerDefaults$backend = wmfmProviderDefaults()$backend
  }

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
    activeChatBackend = providerDefaults$backend,
    activeOllamaModel = providerDefaults$ollamaModel,
    activeOllamaThinkLow = providerDefaults$ollamaThinkLow,
    availableOllamaModels = providerDefaults$ollamaModel,
    userDatasetContext = "",
    researchQuestion = "",
    modelFollowupQuestion = "",
    explanationPromptDiagnostics = NULL,
    loadedExample = NULL,
    providerConfigSaveStatus = NULL
  )

  list(
    rv = rv,
    modelFit = reactiveVal(NULL),
    contrastPairs = reactiveVal(character(0)),
    contrastResultText = reactiveVal("")
  )
}
