#' Application server logic
#'
#' Defines the server-side logic for the Model Builder app. Handles data
#' upload, variable assignment via buckets, model fitting, calls to the
#' language model for fitted equations and explanations, and plotting of
#' the fitted model when appropriate.
#'
#' @param input Shiny input object.
#' @param output Shiny output object.
#' @param session Shiny session object.
#'
#' @return No return value; called for its side effects.
#'
#' @keywords internal
#'
#' @importFrom shiny reactive reactiveValues reactiveVal renderPlot renderUI renderText
#' @importFrom shiny renderPrint observeEvent observe req showNotification withProgress
#' @importFrom shiny incProgress helpText updateRadioButtons updateTextInput updateCheckboxInput
#' @importFrom shiny updateSelectInput showModal removeModal modalDialog removeNotification
#' @importFrom shiny renderTable tableOutput downloadButton downloadHandler
#' @importFrom shiny radioButtons textInput textAreaInput modalButton actionButton
#' @importFrom shiny updateTabsetPanel tagList selectInput div tags htmlOutput
#' @importFrom shiny isolate validate need freezeReactiveValue
#' @importFrom sortable bucket_list add_rank_list
#' @importFrom tools file_ext
#' @importFrom stats as.formula family formula lm glm binomial poisson model.frame terms
#' @importFrom stats predict na.omit setNames
#' @importFrom grDevices replayPlot
#' @importFrom stats confint density median quantile sd var
#' @importFrom utils data read.table capture.output str combn getFromNamespace head packageVersion
#' @importFrom graphics plot.new text
#' @importFrom ggplot2 ggplot geom_point geom_line geom_histogram geom_density after_stat labs aes vars
#' @importFrom ggplot2 geom_boxplot position_jitter scale_y_continuous
#' @importFrom ggplot2 theme_minimal theme element_text facet_wrap theme_bw
#' @importFrom rlang .data
#' @importFrom htmltools htmlEscape
appServer = function(input, output, session) {
  startupState = registerStartupDataChoiceObservers(
    input = input,
    output = output,
    session = session
  )

  developerModeUnlocked = startupState$developerModeUnlocked
  exampleLoadStatus = startupState$exampleLoadStatus

  rv = reactiveValues(
    data = NULL,
    allVars = character(0),
    autoFormula = "",
    modelEquations = NULL,
    modelExplanation = NULL,
    modelExplanationAudit = NULL,
    modelExplanationTutor = NULL,
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
    isResetting = FALSE,
    activeChatBackend = "ollama",
    activeOllamaModel = "gpt-oss",
    activeOllamaThinkLow = FALSE,
    availableOllamaModels = "gpt-oss",
    userDatasetContext = "",
    researchQuestion = "",
    loadedExample = NULL
  )


  modelFit = reactiveVal(NULL)

  registerModelOutputTabs(
    output = output,
    input = input,
    modelFit = modelFit
  )

  contrastPairs = reactiveVal(character(0))

  modelExplanationObservers = registerModelExplanationObservers(
    input = input,
    output = output,
    rv = rv,
    modelFit = modelFit,
    developerModeUnlocked = developerModeUnlocked
  )

  modelExplanationTeachingSummary = modelExplanationObservers$modelExplanationTeachingSummary
  modelExplanationClaimEvidenceMap = modelExplanationObservers$modelExplanationClaimEvidenceMap

  contrastResultText = reactiveVal("")

  registerChatProviderObservers(
    input = input,
    output = output,
    session = session,
    rv = rv
  )


  serverStateHelpers = createAppServerStateHelpers(
    input = input,
    session = session,
    rv = rv,
    modelFit = modelFit
  )

  setBucketState = serverStateHelpers$setBucketState
  resetModelPage = serverStateHelpers$resetModelPage
  applyLoadedExampleToInputs = serverStateHelpers$applyLoadedExampleToInputs

  registerContrastObservers(
    input = input,
    output = output,
    session = session,
    rv = rv,
    modelFit = modelFit,
    contrastPairs = contrastPairs,
    contrastResultText = contrastResultText,
    setBucketState = setBucketState
  )

  registerModelPlotObservers(
    input = input,
    output = output,
    modelFit = modelFit
  )


  registerModelFormulaObservers(
    output = output,
    modelFit = modelFit
  )

  registerFittedEquationObservers(
    output = output,
    rv = rv,
    modelFit = modelFit
  )

  registerDataLoadObservers(
    input = input,
    session = session,
    rv = rv,
    exampleLoadStatus = exampleLoadStatus,
    resetModelPage = resetModelPage,
    applyLoadedExampleToInputs = applyLoadedExampleToInputs
  )

  registerModelHelpObservers(
    input = input,
    output = output,
    session = session,
    rv = rv
  )

  registerModelSetupObservers(
    input = input,
    output = output,
    session = session,
    rv = rv,
    setBucketState = setBucketState
  )

  registerFitModelObservers(
    input = input,
    output = output,
    session = session,
    rv = rv,
    modelFit = modelFit,
    resetModelPage = resetModelPage
  )

  # -------------------------------------------------------------------
  # Display model summary (regression table)
  # -------------------------------------------------------------------
  output$model_output = renderPrint({
    m = modelFit()
    if (is.null(m)) {
      cat("No model fitted yet.")
      return()
    }

    out = capture.output(summary(m))

    # Find first occurrence of "Coefficients:"
    idx = grep("^Coefficients:", out)

    # Keep that line and everything after it
    out = out[idx:length(out)]

    cat(out, sep = "\n")
  })
}
