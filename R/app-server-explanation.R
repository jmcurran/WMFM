#' Build the model explanation support accordion
#'
#' @param panels List of accordion panels to display.
#' @param developerMode Logical indicating whether developer diagnostics are shown.
#' @param diagnostics Optional explanation prompt diagnostics payload.
#'
#' @return A bslib accordion tag object.
#' @keywords internal
#' @noRd
buildModelExplanationSupportAccordion = function(panels, developerMode = FALSE, diagnostics = NULL) {
  if (isTRUE(developerMode)) {
    panels = c(
      panels,
      list(bslib::accordion_panel(
        title = "Explanation prompt diagnostics",
        value = "explanation_prompt_diagnostics",
        buildExplanationPromptDiagnosticsUi(diagnostics = diagnostics)
      ))
    )
  }

  do.call(
    bslib::accordion,
    c(
      list(
        id = "model_explanation_support_accordion",
        multiple = TRUE,
        open = if (isTRUE(developerMode)) "explanation_prompt_diagnostics" else FALSE
      ),
      panels
    )
  )
}


#' Build the empty model-explanation prompt
#'
#' @param modelAvailable Logical indicating whether a fitted model is available.
#'
#' @return A Shiny tag object.
#' @keywords internal
#' @noRd
buildModelExplanationRequestUi = function(modelAvailable = FALSE) {
  if (!isTRUE(modelAvailable)) {
    return(helpText("Fit a model before requesting an explanation."))
  }

  tags$div(
    class = "wmfm-explanation-request",
    tags$p(
      "No explanation has been generated for this model."
    ),
    tags$p(
      class = "wmfm-explanation-helper-note",
      "Try writing your own explanation first, or ask WMFM to explain the fitted model."
    ),
    actionButton(
      inputId = "generateModelExplanationBtn",
      label = "Explain this model",
      icon = icon("wand-magic-sparkles"),
      class = "btn-primary"
    )
  )
}

#' Register model explanation observers for the app server
#'
#' @param input Shiny input object.
#' @param output Shiny output object.
#' @param rv App reactive values object.
#' @param modelFit Reactive value containing the fitted model.
#' @param developerModeUnlocked Reactive value indicating developer mode status.
#'
#' @return A list of model-explanation reactive helpers.
#'
#' @keywords internal
registerModelExplanationObservers = function(
  input,
  output,
  rv,
  modelFit,
  developerModeUnlocked
) {
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


  output$diag_followup_json_download = downloadHandler(
    filename = function() {
      paste0(
        "wmfm-followup-diagnostics-",
        format(Sys.time(), "%Y%m%d-%H%M%S"),
        ".json"
      )
    },
    content = function(file) {
      diagnosticsJson = buildExplanationPromptDiagnosticsJson(
        diagnostics = rv$explanationPromptDiagnostics
      )
      writeLines(diagnosticsJson, con = file, useBytes = TRUE)
    }
  )

  # -------------------------------------------------------------------
  # Model explanation
  # -------------------------------------------------------------------
  output$model_explanation = renderUI({
    expl = rv$modelExplanation
    audit = rv$modelExplanationAudit
    claimMap = modelExplanationClaimEvidenceMap()
    tutorText = rv$modelExplanationTutor
    explanationMessage = rv$modelExplanationMessage
    provenance = rv$modelExplanationProvenance

    m = modelFit()

    if (is.null(m)) {
      return(buildModelExplanationRequestUi(modelAvailable = FALSE))
    }

    if (is.null(expl)) {
      return(buildModelExplanationRequestUi(modelAvailable = TRUE))
    }

    teachingSummary = modelExplanationTeachingSummary()
    adjustmentVariables = getModelAdjustmentVariables(m)
    interpretationModeLabel = buildInterpretationModeLabel(adjustmentVariables = adjustmentVariables)
    researchQuestionText = trimws(as.character(rv$researchQuestion %||% attr(m, "wmfm_research_question", exact = TRUE) %||% ""))
    displayExplanation = if (!is.null(expl)) {
      cleanExplanationText(expl)
    } else {
      NULL
    }
    followupPayload = attr(m, "wmfm_model_followup_payload", exact = TRUE) %||% list()
    observationResidualUi = buildObservationResidualResultUi(
      followupPayload = followupPayload
    )
    comparableObservationUi = buildComparableObservationResultUi(
      followupPayload = followupPayload
    )

    tagList(
      comparableObservationUi,
      observationResidualUi,
      if (!is.null(displayExplanation)) {
        tagList(
          if (!is.null(interpretationModeLabel)) {
            tags$div(
              class = "wmfm-explanation-helper-note",
              tags$em(interpretationModeLabel)
            )
          },
          tags$div(
            class = "wmfm-explanation-helper-note",
            if (!is.null(interpretationModeLabel)) {
              "This explanation focuses on the primary research question after accounting for selected adjustment variables."
            } else {
              "The sections below unpack the choices the app made so you can see why it described the model this way."
            }
          ),
          tags$div(
            class = "wmfm-explanation-box",
            if (nzchar(researchQuestionText)) {
              tags$p(
                tags$strong("Research question: "),
                researchQuestionText
              )
            },
            renderSafeExplanationHtml(
              text = displayExplanation,
              zoomLevel = input$modelExplanationZoom %||% "normal"
            ),
            if (!is.null(provenance)) {
              tags$div(
                class = "wmfm-explanation-provenance",
                buildExplanationProvenanceText(
                  providerLabel = provenance$providerLabel %||% "Unknown provider",
                  modelName = provenance$modelName %||% NULL,
                  generatedAt = provenance$generatedAt %||% Sys.time()
                )
              )
            }
          )
        )
      } else if (!is.null(explanationMessage) && nzchar(trimws(explanationMessage))) {
        helpText(
          paste(
            explanationMessage,
            "The teaching summary is still available below."
          )
        )
      } else {
        helpText("No LLM explanation was generated, but the teaching summary is still available below.")
      },
      if (!is.null(teachingSummary)) {
        tagList(
          tags$hr(),
          buildModelExplanationSupportAccordion(
            panels = list(
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
            ),
            developerMode = isTRUE(developerModeUnlocked()),
            diagnostics = rv$explanationPromptDiagnostics
          )
        )
      } else {
        tagList(
          tags$hr(),
          buildModelExplanationSupportAccordion(
            panels = list(
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
            ),
            developerMode = isTRUE(developerModeUnlocked()),
            diagnostics = rv$explanationPromptDiagnostics
          )
        )
      }
    )
  })

  observeEvent(input$generateModelExplanationBtn, {
    m = modelFit()

    if (is.null(m)) {
      showNotification(
        "Fit a model before requesting an explanation.",
        type = "warning",
        duration = 6
      )
      return(NULL)
    }

    chatProvider = tryCatch(
      getChatProvider(
        backend = rv$activeChatBackend %||% "ollama",
        model = rv$activeOllamaModel %||% "gpt-oss",
        ollamaThinkLow = rv$activeOllamaThinkLow %||% FALSE
      ),
      error = function(e) {
        rv$modelExplanationMessage = buildChatProviderConnectionFailedMessage(
          conditionMessage(e)
        )
        showNotification(
          rv$modelExplanationMessage,
          type = "error",
          duration = 10
        )
        NULL
      }
    )

    if (is.null(chatProvider)) {
      return(NULL)
    }

    if (isWmfmDummyChatProvider(chatProvider)) {
      rv$modelExplanationMessage = getWmfmDummyChatProviderMessage(chatProvider)
      showNotification(
        rv$modelExplanationMessage,
        type = "error",
        duration = 12
      )
      return(NULL)
    }

    rv$chatProvider = chatProvider

    if (is.environment(rv$contrastLlmCache)) {
      rm(list = ls(envir = rv$contrastLlmCache), envir = rv$contrastLlmCache)
    }

    withProgress(message = "Generating the model explanation", value = 0, {
      incProgress(0.2, detail = "Preparing the fitted-model information")

      explanation = tryCatch(
        buildAppExplanation(
          model = m,
          chatProvider = chatProvider
        ),
        error = function(e) {
          showNotification(
            paste("The explanation could not be generated.", conditionMessage(e)),
            type = "error",
            duration = 10
          )
          NULL
        }
      )

      if (is.null(explanation)) {
        return(NULL)
      }

      incProgress(0.6, detail = "Preparing the explanation for display")

      explanation = postProcessExplanationText(explanation)
      rv$modelExplanation = explanation
      rv$modelExplanationMessage = NULL
      rv$modelExplanationTutor = NULL
      rv$modelExplanationProvenance = list(
        providerLabel = if (identical(rv$activeChatBackend, "claude")) "Claude" else "Ollama",
        modelName = if (identical(rv$activeChatBackend, "ollama")) rv$activeOllamaModel else NULL,
        generatedAt = Sys.time()
      )

      if (is.list(rv$explanationPromptDiagnostics)) {
        rv$explanationPromptDiagnostics$generatedExplanation = explanation
      }

      incProgress(0.2, detail = "Explanation ready")
    })
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



  list(
    modelExplanationTeachingSummary = modelExplanationTeachingSummary,
    modelExplanationClaimEvidenceMap = modelExplanationClaimEvidenceMap
  )
}
