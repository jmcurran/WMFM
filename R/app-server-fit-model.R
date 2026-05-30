#' Register fit model observers
#'
#' Wires formula validation, model fitting, output generation, and reset
#' behaviour for the app server.
#'
#' @param input Shiny input object.
#' @param output Shiny output object.
#' @param session Shiny session object.
#' @param rv App reactive values object.
#' @param modelFit Reactive value containing the fitted model.
#' @param resetModelPage Function that clears model-page state.
#'
#' @return No return value; called for observer side effects.
#'
#' @keywords internal
registerFitModelObservers = function(input, output, session, rv, modelFit, resetModelPage) {
  observeEvent(input$modelFollowupQuestion, {
    rv$modelFollowupQuestion = trimws(input$modelFollowupQuestion %||% "")
  }, ignoreInit = FALSE)

  # -------------------------------------------------------------------
  # Helper: formula checker
  # -------------------------------------------------------------------
  checkFormula = function() {
    if (is.null(rv$data)) {
      return(list(ok = FALSE, msg = "Load a data set first."))
    }

    txt = trimws(input$formula_text)

    if (txt == "") {
      return(list(
        ok  = FALSE,
        msg = "Enter a model formula, e.g. y ~ x1 + x2"
      ))
    }

    # parse as formula
    f = tryCatch({
      as.formula(txt)
    }, error = function(e) {
      NULL
    })

    if (is.null(f)) {
      return(list(ok = FALSE, msg = "Formula is not syntactically valid."))
    }

    # check variables exist in data
    vars    = all.vars(f)
    missing = setdiff(vars, names(rv$data))

    if (length(missing) > 0) {
      return(list(
        ok  = FALSE,
        msg = paste("Unknown variable(s):", paste(missing, collapse = ", "))
      ))
    }

    list(ok = TRUE, msg = "Formula OK.")
  }


  # -------------------------------------------------------------------
  # Show formula validation status
  # -------------------------------------------------------------------
  output$formula_status = renderUI({
    res = checkFormula()

    statusClass = if (isTRUE(res$ok)) {
      "wmfm-formula-status wmfm-formula-status-ok"
    } else {
      "wmfm-formula-status wmfm-formula-status-error"
    }

    tags$span(class = statusClass, res$msg)
  })

  # -------------------------------------------------------------------
  # Fit model when button clicked (lazy LLM connection)
  # -------------------------------------------------------------------
  observeEvent(input$fit_btn, {

    # Try to obtain a chat provider *now*, instead of at app startup
    chatProvider = tryCatch(
      getChatProvider(
        backend = rv$activeChatBackend %||% "ollama",
        model = rv$activeOllamaModel %||% "gpt-oss",
        ollamaThinkLow = rv$activeOllamaThinkLow %||% FALSE
      ),
      error = function(e) {
        showNotification(
          buildChatProviderConnectionFailedMessage(conditionMessage(e)),
          type     = "error",
          duration = 10
        )
        NULL
      }
    )

    rv$modelExplanationMessage = NULL

    if (is.null(chatProvider)) {
      rv$modelExplanationMessage = buildNoLanguageModelAvailableMessage()
      showNotification(
        rv$modelExplanationMessage,
        type     = "message",
        duration = 10
      )
    } else if (isWmfmDummyChatProvider(chatProvider)) {
      rv$modelExplanationMessage = getWmfmDummyChatProviderMessage(chatProvider)
      showNotification(
        rv$modelExplanationMessage,
        type     = "error",
        duration = 12
      )
    } else {
      rv$chatProvider = chatProvider

      # optional: clear cache on refit so interpretations match the new model
      if (is.environment(rv$contrastLlmCache)) {
        rm(list = ls(envir = rv$contrastLlmCache), envir = rv$contrastLlmCache)
      }
    }

    res = checkFormula()
    if (!res$ok) {
      showNotification(res$msg, type = "error")
      return(NULL)
    }

    f        = as.formula(input$formula_text)
    respName = all.vars(f)[1]

    ## Make sure the response is valid for the selected model
    chk = validateResponseVar(rv$data, respName, input$model_type)
    if (!isTRUE(chk$ok)) {
      showNotification(chk$reason, type = "error", duration = 8)
      return(NULL)
    }


    # Enforce at most 3 distinct predictor variables in the model
    allVarsInFormula = all.vars(f)
    predNames = setdiff(allVarsInFormula, respName)
    predNames = unique(predNames)

    if (length(predNames) > 3) {
      showNotification(buildTooManyPredictorsMessage(predNames), type = "error")
      return(NULL)
    }

    # Work on a copy of the data so we can safely coerce factors
    dfMod = rv$data

    # Anything in the Factors bucket should be treated as a factor
    factorVars = rv$bucketFactors %||% character(0)
    for (v in factorVars) {
      if (!is.null(dfMod[[v]]) && !is.factor(dfMod[[v]])) {
        dfMod[[v]] = factor(dfMod[[v]])
      }
    }

    y = dfMod[[respName]]

    # Fit the chosen model type
    if (input$model_type == "lm") {

      # Extract response from dfMod
      resp = dfMod[[respName]]

      # If response is a 2-level factor, convert to numeric 0/1
      if (is.factor(resp) && nlevels(resp) == 2) {

        levs = levels(resp)

        # Map: first level -> 0, second level -> 1
        newY = as.numeric(resp == levs[2])

        showNotification(
          buildLinearModelBinaryFactorRecodingMessage(respName, levs),
          type     = "warning",
          duration = 10
        )

        dfMod[[respName]] = newY
        resp = newY
      }

      # Fit normal lm() model
      m = lm(f, data = dfMod)

    } else if (input$model_type == "logistic") {

      # Extract response
      respName = all.vars(f)[1]
      y = dfMod[[respName]]

      # ---- Case 1: Character with exactly 2 values -> convert to factor ----
      if (is.character(y)) {
        u = unique(na.omit(y))
        if (length(u) == 2) {
          dfMod[[respName]] = factor(y)
          y = dfMod[[respName]]
        } else {
          showNotification(buildLogisticCharacterResponseMessage(respName, length(u)), type = "error")
          return(NULL)
        }
      }

      # ---- Case 2: Factor with exactly 2 levels ----
      if (is.factor(y)) {
        levs = levels(y)
        if (length(levs) != 2) {
          showNotification(buildLogisticFactorResponseMessage(respName, length(levs)), type = "error")
          return(NULL)
        }
      }

      # ---- Case 3: Numeric 0/1 ----
      else if (is.numeric(y)) {
        uy = unique(na.omit(y))
        if (!all(uy %in% c(0, 1))) {
          showNotification(buildLogisticNumericResponseMessage(respName, uy), type = "error")
          return(NULL)
        }
      }

      # ---- Case 4: Anything else -> reject ----
      else {
        showNotification(buildLogisticUnsupportedResponseMessage(), type = "error")
        return(NULL)
      }

      # ---- If we reach here: response is valid ----
      m = glm(f, data = dfMod, family = binomial(link = "logit"))

    } else if (input$model_type == "poisson") {
      if (any(na.omit(y) < 0) || any(na.omit(y) %% 1 != 0)) {
        showNotification(buildPoissonResponseWarningMessage(), type = "warning")
      }
      m = glm(f, data = dfMod, family = poisson(link = "log"))

    } else {
      showNotification(buildUnknownModelTypeMessage(), type = "error")
      return(NULL)
    }

    adjustmentVariables = buildAdjustmentMetadata(
      selectedVariables = rv$adjustmentVariables,
      formulaPredictors = predNames
    )
    attr(m, "wmfm_adjustment_variables") = adjustmentVariables

    # If this data came from a package, attach package metadata to the model.
    if (identical(input$data_source, "package")) {
      pkg = input$data_package %||% ""
      dsName = input$package_dataset

      docText = NULL
      if (identical(pkg, "s20x")) {
        docText = getS20xDocText(dsName)
      }

      if (!is.null(docText)) {
        attr(m, "wmfm_dataset_doc") = docText
      }

      attr(m, "wmfm_dataset_name") = dsName
      attr(m, "wmfm_dataset_package") = pkg

      nounPhrase = resolveResponseNounPhrase(m, respName)
      attr(m, "wmfm_response_noun_phrase") = nounPhrase

      rv$modelContext = list(
        responseVar = respName,
        nounPhrase = nounPhrase,
        datasetName = dsName,
        packageName = pkg,
        adjustmentVariables = adjustmentVariables
      )
    }

    # -------------------------------------------------------------
    # Attach user-provided dataset context when data are uploaded
    # -------------------------------------------------------------
    if (identical(input$data_source %||% "", "upload")) {

      userCtxRaw = rv$userDatasetContext %||% ""
      userCtxRaw = trimws(userCtxRaw)

      if (nzchar(userCtxRaw)) {

        # Escape double quotes before placing into any prompt text.
        userCtx = gsub("\"", "\\\\\"", userCtxRaw, fixed = TRUE)

        # Reuse the same attribute name the app already uses for s20x docs
        # so downstream LLM helpers can pick it up consistently.
        attr(m, "wmfm_dataset_doc")  = userCtx
        attr(m, "wmfm_dataset_name") = "Uploaded data"

        # Try to maintain the same "nounPhrase" flow you already have
        nounPhrase = resolveResponseNounPhrase(m, respName)
        attr(m, "wmfm_response_noun_phrase") = nounPhrase

        rv$modelContext = list(
          responseVar = respName,
          nounPhrase  = nounPhrase,
          datasetName = "Uploaded data",
          adjustmentVariables = adjustmentVariables
        )
      }
    }

    if (!is.null(rv$loadedExample)) {
      exampleInfo = rv$loadedExample
      exampleName = exampleInfo$name %||% "Example"
      exampleDataContext = trimws(exampleInfo$dataContext %||% "")

      if (nzchar(exampleDataContext)) {
        attr(m, "wmfm_dataset_doc") = exampleDataContext
      }

      attr(m, "wmfm_dataset_name") = paste0(exampleName, " example")
      attr(m, "wmfm_example_name") = exampleName

      nounPhrase = resolveResponseNounPhrase(m, respName)
      attr(m, "wmfm_response_noun_phrase") = nounPhrase

      rv$modelContext = list(
        responseVar = respName,
        nounPhrase = nounPhrase,
        datasetName = paste0(exampleName, " example"),
        adjustmentVariables = adjustmentVariables
      )
    }

    researchQuestionRaw = trimws(input$researchQuestion %||% rv$researchQuestion %||% "")

    if (!nzchar(researchQuestionRaw)) {
      showNotification(
        "Please enter the research question before fitting the model. WMFM uses it to frame the explanation from the start.",
        type = "warning",
        duration = 8
      )
      return(NULL)
    }

    researchQuestion = gsub("\"", "\\\"", researchQuestionRaw, fixed = TRUE)
    attr(m, "wmfm_research_question") = researchQuestion
    followupQuestion = trimws(input$modelFollowupQuestion %||% rv$modelFollowupQuestion %||% "")
    followupClassification = classifyModelFollowupQuestion(followupQuestion = followupQuestion)
    followupClassification = enrichFollowupPayloadWithLmPrediction(
      model = m,
      followupPayload = followupClassification
    )
    followupClassification = enrichFollowupPayloadWithUnitChange(
      model = m,
      followupPayload = followupClassification
    )
    attr(m, "wmfm_model_followup_question") = followupClassification$originalText
    attr(m, "wmfm_model_followup_payload") = followupClassification
    promptPreview = lmToExplanationPrompt(m)
    rv$explanationPromptDiagnostics = list(
      followupText = followupClassification$originalText %||% "",
      followupPayload = followupClassification,
      assembledPrompt = promptPreview,
      hasFollowupInPrompt = grepl("Follow-up model question", promptPreview, fixed = TRUE),
      hasPredictionPayloadInPrompt = grepl("WMFM deterministic prediction payload", promptPreview, fixed = TRUE),
      hasSeparateFollowupParagraphInstruction = grepl("separate paragraph after the main research-question answer", promptPreview, fixed = TRUE)
    )

    modelFit(m)

    outputMessages = buildAppOutputMessages(
      equationMethod = "deterministic",
      explanationAvailable = FALSE,
      explanationRequested = !is.null(chatProvider)
    )

    withProgress(message = outputMessages$progressMessage, value = 0, {

      incProgress(0.10, detail = outputMessages$equationDetail)

      equationResults = tryCatch(
        buildAppEquations(
          model = m,
          chatProvider = chatProvider
        ),
        error = function(e) {
          showNotification(
            paste(
              "Deterministic equation generation failed.",
              "You can still use the fitted model and plots.",
              "\nDetails:", conditionMessage(e)
            ),
            type     = "error",
            duration = 10
          )
          return(NULL)
        }
      )

      if (is.null(equationResults)) {
        return(NULL)
      }

      if (isTRUE(equationResults$equationFallbackUsed)) {
        fallbackMessages = buildAppOutputMessages(
          equationMethod = "llm",
          explanationAvailable = FALSE,
          explanationRequested = !is.null(chatProvider)
        )

        showNotification(
          fallbackMessages$fallbackNotification,
          type     = "warning",
          duration = 10
        )
      }

      incProgress(0.25, detail = outputMessages$equationCompleteDetail)

      explanationMessages = buildAppOutputMessages(
        equationMethod = equationResults$equationMethodUsed %||% "deterministic",
        explanationAvailable = FALSE,
        explanationRequested = !is.null(chatProvider)
      )

      incProgress(0.10, detail = explanationMessages$explanationDetail)

      explanation = buildAppExplanation(
        model = m,
        chatProvider = chatProvider
      )
      explanation = postProcessExplanationText(explanation)
      if (is.list(rv$explanationPromptDiagnostics)) {
        rv$explanationPromptDiagnostics$generatedExplanation = explanation %||% ""
      }
      explanationAudit = buildAppExplanationAudit(model = m)

      incProgress(0.35, detail = outputMessages$updateDetail)

      rv$modelEquations = equationResults$equations
      rv$modelExplanation = explanation
      rv$modelExplanationProvenance = if (!is.null(explanation)) {
        list(
          providerLabel = if (identical(rv$activeChatBackend, "claude")) "Claude" else "Ollama",
          modelName = if (identical(rv$activeChatBackend, "ollama")) rv$activeOllamaModel else NULL,
          generatedAt = Sys.time()
        )
      } else {
        NULL
      }
      rv$modelExplanationAudit = explanationAudit
      rv$modelExplanationTutor = NULL

      if (!is.null(explanation)) {
        rv$modelExplanationMessage = NULL
      }

      finishMessages = buildAppOutputMessages(
        equationMethod = equationResults$equationMethodUsed %||% "deterministic",
        explanationAvailable = !is.null(explanation),
        explanationRequested = !is.null(chatProvider)
      )

      incProgress(0.10, detail = finishMessages$finishDetail)
      incProgress(0.10, detail = finishMessages$doneDetail)
    })
    # After fitting and LLM completion, return to the fitted model tab
    updateTabsetPanel(session, "main_tabs", selected = "Fitted Model")
  })

  # -------------------------------------------------------------------
  # Reset model
  # -------------------------------------------------------------------
  observeEvent(input$reset_btn, {
    resetModelPage(resetResponse = TRUE)
  })

}
