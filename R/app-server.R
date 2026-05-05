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
  # Drag-and-drop buckets UI
  # -------------------------------------------------------------------
  output$var_buckets = renderUI({
    if (is.null(rv$data)) {
      return(helpText("Load a data set to see variables."))
    }

    vars = rv$allVars %||% character(0)

    # Preserve current bucket contents across re-renders,
    # but sanitize against current dataset vars
    factors = intersect(rv$bucketFactors %||% character(0), vars)
    cont    = intersect(rv$bucketContinuous %||% character(0), vars)

    # Remove the chosen response AND anything already placed into buckets
    currentResp = input$response_var

    vars = setdiff(vars, c(currentResp, factors, cont))

    bucket_list(
      header      = NULL,
      group_name  = paste0("vars_group_", rv$bucketGroupId),
      orientation = "horizontal",
      add_rank_list(
        text     = "Variables",
        labels   = vars,
        input_id = "variables"
      ),
      add_rank_list(
        text     = "Factors",
        labels   = factors,
        input_id = "factors"
      ),
      add_rank_list(
        text     = "Continuous",
        labels   = cont,
        input_id = "continuous"
      )
    )
  })

  # -------------------------------------------------------------------
  # Interactions UI (2-way and 3-way) built from Factor + Continuous buckets
  # -------------------------------------------------------------------
  output$interaction_ui = renderUI({
    if (is.null(rv$data)) {
      return(NULL)
    }

    factors = rv$bucketFactors %||% character(0)
    cont    = rv$bucketContinuous %||% character(0)
    resp    = input$response_var

    # Exclude the response from predictors
    predsAll = unique(setdiff(c(factors, cont), resp))

    # Hard limit: only first 3 predictors are allowed in the model
    predsLimited = predsAll
    if (length(predsLimited) > 3) {
      predsLimited = predsLimited[1:3]
    }

    if (length(predsLimited) < 2) {
      return(NULL)
    }

    # We'll label types using the full bucket info, but only build
    # interactions from predsLimited
    varType = c(
      setNames(rep("(F)", length(factors)), factors),
      setNames(rep("(C)", length(cont)),    cont)
    )

    # ---- Build 2-way and 3-way combinations ----
    combos2 = list()
    combos3 = list()

    if (length(predsLimited) >= 2) {
      combos2 = asplit(combn(predsLimited, 2), 2)
    }
    if (length(predsLimited) >= 3) {
      combos3 = asplit(combn(predsLimited, 3), 2)
    }

    combos = c(combos2, combos3)
    if (length(combos) == 0) {
      return(NULL)
    }

    # Internal values: "var1:var2" or "var1:var2:var3"
    values = vapply(
      combos,
      function(x) {
        paste(x, collapse = ":")
      },
      FUN.VALUE = character(1)
    )

    # Pretty labels with (F)/(C) and : signs
    labels = vapply(
      combos,
      function(x) {
        paste(
          sprintf("%s %s", x, varType[x]),
          collapse = " : "
        )
      },
      FUN.VALUE = character(1)
    )

    # Add a special "all interactions" option at the top
    allValue = "__ALL_INTERACTIONS__"
    allLabel = "All possible 2-way and 3-way interactions"

    choiceValues = c(allValue, values)
    choiceLabels = c(allLabel, labels)

    choices = setNames(choiceValues, choiceLabels)

    currentInteractions = input$interactions %||% character(0)
    pendingInteractions = rv$pendingExampleInteractions %||% character(0)
    selectedInteractions = intersect(
      unique(c(pendingInteractions, currentInteractions)),
      choiceValues
    )

    infoText = NULL
    if (length(predsAll) > 3) {
      infoText = helpText(
        sprintf(
          "You have placed %d variables into the Factors/Continuous buckets. ",
          length(predsAll)
        ),
        "Only the first 3 (",
        paste(predsLimited, collapse = ", "),
        ") are used in the model and for interactions."
      )
    }

    tagList(
      h5("Interactions (optional)"),
      infoText,
      helpText("Select 2-way or 3-way interaction terms to include in the model formula."),
      selectInput(
        inputId  = "interactions",
        label    = NULL,
        choices  = choices,
        selected = selectedInteractions,
        multiple = TRUE,
        width    = "100%"
      )
    )
  })

  registerModelHelpObservers(
    input = input,
    output = output,
    session = session,
    rv = rv
  )

  # -------------------------------------------------------------------
  observeEvent(input$interactions, {
    if (isTRUE(rv$isResetting)) {
      return(NULL)
    }

    currentInteractions = input$interactions %||% character(0)
    pendingInteractions = rv$pendingExampleInteractions %||% character(0)

    if (length(currentInteractions) == 0) {
      return(NULL)
    }

    if (length(pendingInteractions) == 0) {
      return(NULL)
    }

    if (all(pendingInteractions %in% currentInteractions)) {
      rv$pendingExampleInteractions = character(0)
    }
  }, ignoreInit = TRUE)

  # When user selects "All possible interactions", expand to all codes
  # -------------------------------------------------------------------
  observeEvent(
    input$interactions,
    {
      ints = input$interactions %||% character(0)

      if (!"__ALL_INTERACTIONS__" %in% ints) {
        return(NULL)
      }

      buckets = getCurrentBuckets()
      factors = buckets$factors
      cont = buckets$continuous
      resp = input$response_var

      setBucketState(
        factors = factors,
        continuous = cont
      )

      predsAll = unique(setdiff(c(factors, cont), resp))

      predsLimited = predsAll
      if (length(predsLimited) > 3) {
        predsLimited = predsLimited[1:3]
      }

      if (length(predsLimited) < 2) {
        updateSelectInput(
          session,
          "interactions",
          selected = character(0)
        )
        return(NULL)
      }

      combos2 = list()
      combos3 = list()

      if (length(predsLimited) >= 2) {
        combos2 = asplit(combn(predsLimited, 2), 2)
      }
      if (length(predsLimited) >= 3) {
        combos3 = asplit(combn(predsLimited, 3), 2)
      }

      combos = c(combos2, combos3)

      if (length(combos) == 0) {
        updateSelectInput(
          session,
          "interactions",
          selected = character(0)
        )
        return(NULL)
      }

      allInts = vapply(
        combos,
        function(x) {
          paste(x, collapse = ":")
        },
        FUN.VALUE = character(1)
      )

      updateSelectInput(
        session,
        "interactions",
        selected = allInts
      )
    },
    ignoreInit = TRUE
  )

  # -------------------------------------------------------------------
  # Warn + confirm when a numeric variable is moved into the Factors bucket
  # -------------------------------------------------------------------
  observeEvent(input$factors, {
    if (isTRUE(rv$isResetting)) {
      return(NULL)
    }

    if (is.null(rv$data)) {
      rv$lastFactors = character(0)
      return(NULL)
    }

    currentFactors = intersect(input$factors %||% character(0), rv$allVars %||% character(0))
    previousFactors = intersect(rv$lastFactors %||% character(0), rv$allVars %||% character(0))

    newVars = setdiff(currentFactors, previousFactors)

    if (length(newVars) == 0) {
      rv$lastFactors = currentFactors
      return(NULL)
    }

    v = newVars[1]
    col = rv$data[[v]]

    if (is.numeric(col) && !is.factor(col)) {
      rv$pendingFactorVar = v

      showModal(
        modalDialog(
          title = buildNumericFactorConfirmTitle(),
          buildNumericFactorConfirmMessage(v),
          footer = tagList(
            actionButton("cancel_factor_numeric", buildNumericFactorCancelLabel()),
            actionButton("confirm_factor_numeric", buildNumericFactorConfirmLabel())
          )
        )
      )

      return(NULL)
    }

    rv$lastFactors = currentFactors
  })

  # -------------------------------------------------------------------
  # Add derived variable to the data + refresh buckets
  # -------------------------------------------------------------------
  observeEvent(input$addDerivedVarBtn, {
    if (is.null(rv$data)) {
      showNotification(buildLoadDataFirstMessage(), type = "message")
      return(NULL)
    }

    showModal(
      modalDialog(
        title = buildAddDerivedVariableTitle(),
        easyClose = TRUE,
        tagList(
          helpText(
            buildAddDerivedVariableHelpText()
          ),
          textInput(
            inputId = "derivedVarTextModal",
            label = NULL,
            placeholder = buildAddDerivedVariablePlaceholder(),
            value = ""
          )
        ),
        footer = tagList(
          modalButton("Cancel"),
          actionButton(
            "confirmAddDerivedVarBtn",
            buildAddDerivedVariableConfirmLabel(),
            class = "btn-success"
          )
        )
      )
    )
  }, ignoreInit = TRUE)

  observeEvent(input$confirmAddDerivedVarBtn, {
    if (is.null(rv$data)) {
      showNotification(buildLoadDataFirstMessage(), type = "message")
      return(NULL)
    }

    res = addDerivedVariableToData(rv$data, input$derivedVarTextModal)

    if (!isTRUE(res$ok)) {
      showNotification(res$msg, type = "error", duration = 8)
      return(NULL)
    }

    rv$data = res$data

    # Refresh variable list used by the buckets + response picker
    rv$allVars = names(rv$data)

    # Force buckets to re-render (without losing current placements)
    rv$bucketGroupId = rv$bucketGroupId + 1L

    # Keep the current response if possible, otherwise fall back to first column
    currentResp = input$response_var
    selectedResp = if (!is.null(currentResp) && nzchar(currentResp) && currentResp %in% rv$allVars) {
      currentResp
    } else {
      rv$allVars[1]
    }

    updateSelectInput(
      session,
      "response_var",
      choices  = rv$allVars,
      selected = selectedResp
    )

    removeModal()
    showNotification(res$msg, type = "message", duration = 4)
  }, ignoreInit = TRUE)

  # -------------------------------------------------------------------
  # Response picker
  # -------------------------------------------------------------------
  output$response_picker = renderUI({

    req(is.data.frame(rv$data))
    req(input$model_type)

    vars = names(rv$data)

    labels = vapply(
      vars,
      function(v) {
        chk = validateResponseVar(rv$data, v, input$model_type)
        if (isTRUE(chk$ok)) {
          v
        } else {
          paste0("\u274C ", v)
        }
      },
      character(1)
    )

    choices = setNames(vars, labels)

    current = rv$lastResponse %||% input$response_var %||% ""
    selected = if (nzchar(current) && current %in% vars) current else vars[1]

    selectInput(
      inputId  = "response_var",
      label    = "",
      choices  = choices,
      selected = selected
    )
  })

  output$response_explain = renderUI({

    req(is.data.frame(rv$data))
    req(input$model_type)

    resp = input$response_var %||% ""
    if (!nzchar(resp) || !(resp %in% names(rv$data))) {
      return(NULL)
    }

    chk = validateResponseVar(rv$data, resp, input$model_type)

    if (isTRUE(chk$ok)) {
      return(NULL)
    }

    tags$div(
      style = "margin-top: 6px; color: #b00020;",
      paste0("\u274C ", chk$reason)
    )
  })


  # -------------------------------------------------------------------
  # Helper: current valid bucket contents for the active dataset
  # -------------------------------------------------------------------
  getCurrentBuckets = function() {
    vars = rv$allVars %||% character(0)

    factors = intersect(rv$bucketFactors %||% character(0), vars)
    cont = intersect(rv$bucketContinuous %||% character(0), vars)

    list(
      factors = factors,
      continuous = cont
    )
  }

  # -------------------------------------------------------------------
  # Auto-populate formula from buckets + optional interactions
  # (limit to 3 predictors; expert mode for compact notation)
  # -------------------------------------------------------------------
  observeEvent(
    list(
      input$response_var,
      rv$bucketFactors,
      rv$bucketContinuous,
      input$interactions,
      input$expert_mode
    ),
    {
      if (isTRUE(rv$isResetting)) {
        return(NULL)
      }

      if (is.null(rv$data)) {
        return(NULL)
      }

      resp = input$response_var
      if (is.null(resp) || resp == "" || !(resp %in% rv$allVars)) {
        return(NULL)
      }

      buckets = getCurrentBuckets()
      factors = buckets$factors
      cont = buckets$continuous
      ints = unique(c(
        rv$pendingExampleInteractions %||% character(0),
        input$interactions %||% character(0)
      ))

      predsAll = unique(setdiff(c(factors, cont), resp))

      preds = predsAll
      if (length(preds) > 3) {
        preds = preds[1:3]
      }

      if (length(preds) == 0) {
        newAuto = paste(resp, "~ 1")
        current = trimws(input$formula_text)

        if (current == "" || current == rv$autoFormula) {
          rv$autoFormula = newAuto
          updateTextInput(session, "formula_text", value = newAuto)
        } else {
          rv$autoFormula = newAuto
        }
        return(NULL)
      }

      ints = Filter(
        function(z) {
          all(strsplit(z, ":", fixed = TRUE)[[1]] %in% preds)
        },
        ints
      )

      expertRhs = NULL
      expertOn = isTRUE(input$expert_mode)

      if (expertOn) {
        if (length(preds) == 2 && length(ints) == 1) {
          varsInt = strsplit(ints[1], ":", fixed = TRUE)[[1]]
          if (length(varsInt) == 2 && setequal(varsInt, preds)) {
            expertRhs = paste(preds, collapse = " * ")
          }
        }

        if (is.null(expertRhs) && length(preds) >= 2 && length(ints) > 0) {
          lenInts = vapply(
            strsplit(ints, ":", fixed = TRUE),
            length,
            FUN.VALUE = integer(1)
          )
          pairInts = ints[lenInts == 2]
          higherInts = ints[lenInts > 2]

          if (length(higherInts) == 0) {
            allPairs = apply(
              combn(preds, 2),
              2,
              function(x) {
                paste(x, collapse = ":")
              }
            )
            if (length(pairInts) == length(allPairs) && setequal(pairInts, allPairs)) {
              expertRhs = paste0("(", paste(preds, collapse = " + "), ")^2")
            }
          }
        }
      }

      if (!is.null(expertRhs)) {
        rhs = expertRhs
      } else {
        rhsPieces = character(0)
        mainPart = paste(preds, collapse = " + ")
        rhsPieces = c(rhsPieces, mainPart)

        if (length(ints) > 0) {
          intPart = paste(ints, collapse = " + ")
          rhsPieces = c(rhsPieces, intPart)
        }

        rhs = paste(rhsPieces, collapse = " + ")
      }

      newAuto = paste(resp, "~", rhs)
      current = trimws(input$formula_text)

      if (current == "" || current == rv$autoFormula) {
        rv$autoFormula = newAuto
        updateTextInput(session, "formula_text", value = newAuto)
      } else {
        rv$autoFormula = newAuto
      }
    }
  )

  # -------------------------------------------------------------------
  # User response to numeric-as-factor confirmation
  # -------------------------------------------------------------------
  observeEvent(input$confirm_factor_numeric, {
    req(rv$pendingFactorVar)
    removeModal()

    rv$lastFactors      = input$factors %||% character(0)
    v                   = rv$pendingFactorVar
    rv$pendingFactorVar = NULL
    rv$pendingExampleInteractions = character(0)

    showNotification(
      paste0(
        "Variable '", v,
        "' will be treated as a factor when fitting the model."
      ),
      type     = "message",
      duration = 6
    )
  }, ignoreInit = TRUE)

  observeEvent(input$cancel_factor_numeric, {
    req(rv$pendingFactorVar)
    removeModal()

    rv$lastFactors = input$factors %||% character(0)

    showNotification(
      paste0(
        "If you do not want '", rv$pendingFactorVar,
        "' treated as a factor, drag it back to the Continuous bucket."
      ),
      type     = "warning",
      duration = 10
    )

    rv$pendingFactorVar = NULL
    rv$pendingExampleInteractions = character(0)
  }, ignoreInit = TRUE)

  # -------------------------------------------------------------------
  # Keep 'Variables' bucket in sync with chosen response (if needed)
  # -------------------------------------------------------------------
  observeEvent(input$response_var, {
    newResp         = input$response_var
    rv$lastResponse = newResp
  })

  # -------------------------------------------------------------------
  # Show formula validation status
  # -------------------------------------------------------------------
  output$formula_status = renderText({
    res = checkFormula()
    res$msg
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

    if (is.null(chatProvider)) {
      showNotification(
        buildNoLanguageModelAvailableMessage(),
        type     = "message",
        duration = 10
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
        packageName = pkg
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
          datasetName = "Uploaded data"
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
        datasetName = paste0(exampleName, " example")
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
      explanationAudit = buildAppExplanationAudit(model = m)

      incProgress(0.35, detail = outputMessages$updateDetail)

      rv$modelEquations = equationResults$equations
      rv$modelExplanation = explanation
      rv$modelExplanationAudit = explanationAudit
      rv$modelExplanationTutor = NULL

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
