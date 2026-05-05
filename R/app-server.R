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

  modelExplanationTeachingSummary = reactive({
    audit = rv$modelExplanationAudit
    m = modelFit()

    if (is.null(audit) || is.null(m)) {
      return(NULL)
    }

    tryCatch(
      buildExplanationTeachingSummary(
        audit = audit,
        model = m,
        researchQuestion = rv$researchQuestion %||% NULL
      ),
      error = function(e) {
        NULL
      }
    )
  })

  modelExplanationClaimEvidenceMap = reactive({
    explanationText = rv$modelExplanation
    audit = rv$modelExplanationAudit
    teachingSummary = modelExplanationTeachingSummary()
    m = modelFit()

    if (is.null(explanationText) || is.null(audit) || is.null(teachingSummary) || is.null(m)) {
      return(NULL)
    }

    tryCatch(
      buildExplanationClaimEvidenceMap(
        explanationText = explanationText,
        audit = audit,
        teachingSummary = teachingSummary,
        model = m
      ),
      error = function(e) {
        NULL
      }
    )
  })

  developerFeedbackReport = reactive({
    if (!isTRUE(developerModeUnlocked())) {
      return(NULL)
    }

    m = modelFit()
    claimMap = modelExplanationClaimEvidenceMap()

    if (is.null(m) || is.null(claimMap)) {
      return(NULL)
    }

    buildDeveloperFeedbackReport(
      model = m,
      claimMap = claimMap,
      input = input,
      explanationText = rv$modelExplanation,
      researchQuestion = rv$researchQuestion %||% NULL,
      data = rv$data,
      otherIssues = input$developerFeedbackOtherIssues %||% NULL
    )
  })

  output$developerFeedbackReportDownload = downloadHandler(
    filename = function() {
      paste0("wmfm-developer-feedback-", format(Sys.time(), "%Y%m%d-%H%M%S"), ".json")
    },
    content = function(file) {
      report = developerFeedbackReport()

      if (is.null(report)) {
        stop("Developer feedback report is not available.", call. = FALSE)
      }

      writeDeveloperFeedbackReportJson(report = report, file = file)
    }
  )

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
  # Model explanation
  # -------------------------------------------------------------------
  output$model_explanation = renderUI({
    expl = rv$modelExplanation
    audit = rv$modelExplanationAudit
    claimMap = modelExplanationClaimEvidenceMap()
    tutorText = rv$modelExplanationTutor

    if (is.null(expl) && is.null(audit)) {
      return(helpText("Fit a model to see a textual explanation."))
    }

    teachingSummary = modelExplanationTeachingSummary()
    m = modelFit()
    researchQuestionText = trimws(as.character(rv$researchQuestion %||% attr(m, "wmfm_research_question", exact = TRUE) %||% ""))
    displayExplanation = if (!is.null(expl)) {
      cleanExplanationText(expl)
    } else {
      NULL
    }

    tagList(
      if (!is.null(displayExplanation)) {
        tagList(
          tags$div(
            class = "wmfm-explanation-helper-note",
            "The sections below unpack the choices the app made so you can see why it described the model this way."
          ),
          tags$div(
            class = "wmfm-explanation-box",
            if (nzchar(researchQuestionText)) {
              tags$p(
                tags$strong("Research question: "),
                researchQuestionText
              )
            },
            tags$pre(
              style = "white-space: pre-wrap; word-wrap: break-word; margin-bottom: 0;",
              displayExplanation
            )
          )
        )
      } else {
        helpText("No LLM explanation was generated, but the teaching summary is still available below.")
      },
      if (!is.null(teachingSummary)) {
        tagList(
          tags$hr(),
          bslib::accordion(
            id = "model_explanation_support_accordion",
            multiple = TRUE,
            open = FALSE,
            bslib::accordion_panel(
              title = "How each sentence was supported",
              tags$p(
                class = "wmfm-explanation-helper-note",
                "Each card below matches one sentence from the explanation to the main pieces of model information that support it."
              ),
              if (!is.null(claimMap)) {
                tagList(
                  renderExplanationClaimEvidenceUi(
                    claimMap = claimMap,
                    developerMode = isTRUE(developerModeUnlocked())
                  ),
                  if (isTRUE(developerModeUnlocked())) {
                    tagList(
                      tags$hr(class = "hr-tight"),
                      textAreaInput(
                        inputId = "developerFeedbackOtherIssues",
                        label = "Other debugging issues",
                        value = "",
                        width = "100%",
                        rows = 3,
                        placeholder = "Note any other issues that should be considered when debugging this explanation."
                      ),
                      downloadButton(
                        outputId = "developerFeedbackReportDownload",
                        label = "Save report to file",
                        class = "btn btn-primary btn-sm"
                      )
                    )
                  }
                )
              } else {
                tags$p(
                  class = "wmfm-explanation-helper-note",
                  "A sentence-by-sentence support map is not available for this explanation yet."
                )
              }
            ),
            bslib::accordion_panel(
              title = "How to read this explanation",
              tags$p(
                class = "wmfm-explanation-helper-note",
                "These sections explain the scale, starting point, comparison, and uncertainty choices that shaped the wording of the explanation."
              ),
              renderExplanationTeachingSummaryUi(teachingSummary)
            ),
            bslib::accordion_panel(
              title = "Optional AI tutor",
              tags$div(
                class = "wmfm-explanation-helper-box",
                tags$div(
                  class = "wmfm-explanation-helper-note",
                  "Want a more conversational walkthrough? You can optionally ask the app for a tutor-style explanation that stays grounded in the information already shown here."
                ),
                if (!is.null(rv$chatProvider) || (is.character(tutorText) && nzchar(trimws(tutorText)))) {
                  tagList(
                    actionButton(
                      inputId = "modelExplanationTutorBtn",
                      label = "Explain this more simply with AI",
                      class = "btn btn-secondary btn-sm"
                    ),
                    tags$br(),
                    tags$br()
                  )
                } else {
                  tagList(
                    tags$p(
                      class = "wmfm-explanation-helper-note",
                      "Turn on a chat provider in Settings if you want the optional AI tutor walkthrough."
                    ),
                    tags$br()
                  )
                },
                renderExplanationTutorUi(
                  text = tutorText,
                  available = !is.null(rv$chatProvider),
                  researchQuestion = researchQuestionText,
                  dataDescription = teachingSummary$dataDescription %||% NULL
                )
              )
            )
          )
        )
      } else {
        tagList(
          tags$hr(),
          bslib::accordion(
            id = "model_explanation_support_accordion",
            multiple = TRUE,
            open = FALSE,
            bslib::accordion_panel(
              title = "How each sentence was supported",
              tags$p(
                class = "wmfm-explanation-helper-note",
                "A sentence-by-sentence support map is not available for this explanation yet."
              )
            ),
            bslib::accordion_panel(
              title = "How to read this explanation",
              tags$p(
                class = "wmfm-explanation-helper-note",
                "The app could not build the teaching guide for this model yet, so only the main explanation is shown right now."
              )
            ),
            bslib::accordion_panel(
              title = "Optional AI tutor",
              tags$div(
                class = "wmfm-explanation-helper-box",
                tags$div(
                  class = "wmfm-explanation-helper-note",
                  "Want a more conversational walkthrough? You can optionally ask the app for a tutor-style explanation that stays grounded in the information already shown here."
                ),
                tags$p(
                  class = "wmfm-explanation-helper-note",
                  "Turn on a chat provider in Settings if you want the optional AI tutor walkthrough."
                ),
                renderExplanationTutorUi(
                  text = tutorText,
                  available = !is.null(rv$chatProvider),
                  researchQuestion = researchQuestionText,
                  dataDescription = NULL
                )
              )
            )
          )
        )
      }
    )
  })

  observeEvent(input$modelExplanationTutorBtn, {
    audit = rv$modelExplanationAudit
    m = modelFit()

    if (is.null(audit) || is.null(m)) {
      showNotification(
        "Fit a model first so the app has something to explain.",
        type = "warning",
        duration = 6
      )
      return(NULL)
    }

    if (is.null(rv$chatProvider)) {
      showNotification(
        "An active chat provider is needed for the tutor-style explanation.",
        type = "warning",
        duration = 6
      )
      return(NULL)
    }

    teachingSummary = modelExplanationTeachingSummary()

    if (is.null(teachingSummary)) {
      showNotification(
        "The teaching summary could not be built for this model.",
        type = "error",
        duration = 8
      )
      return(NULL)
    }

    withProgress(message = "Generating tutor-style explanation...", value = 0, {
      incProgress(0.25, detail = "Preparing the teaching summary")

      tutorText = buildAppTeachingTutorExplanation(
        teachingSummary = teachingSummary,
        chatProvider = rv$chatProvider,
        modelExplanation = rv$modelExplanation,
        researchQuestion = rv$researchQuestion %||% NULL
      )

      incProgress(0.60, detail = "Waiting for the chat provider")
      incProgress(0.15, detail = "Updating the explanation tab")

      rv$modelExplanationTutor = tutorText

      incProgress(0.00, detail = "Done")
    })

    if (is.null(rv$modelExplanationTutor) || !nzchar(trimws(rv$modelExplanationTutor))) {
      showNotification(
        "The tutor-style explanation could not be generated this time.",
        type = "warning",
        duration = 8
      )
    }
  })

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
