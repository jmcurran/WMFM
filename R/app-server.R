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
#' @importFrom shiny incProgress helpText updateRadioButtons updateTextInput
#' @importFrom shiny updateSelectInput showModal removeModal modalDialog
#' @importFrom shiny renderTable tableOutput
#' @importFrom shiny radioButtons textInput modalButton actionButton
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
  # -------------------------------------------------------------------
  # Pre-populate the package selector immediately, then broaden it after
  # the first UI flush so the app feels responsive on startup.
  # -------------------------------------------------------------------
  packageChoices = reactiveVal(character(0))
  packageScanStatus = reactiveVal(NULL)
  exampleChoices = reactiveVal(character(0))
  exampleLoadStatus = reactiveVal("Choose a built-in example if you want the app to load a complete worked setup.")

  initialPackageChoices = tryCatch(
    {
      ensureS20xInstalled()
      "s20x"
    },
    error = function(e) {
      character(0)
    }
  )

  packageChoices(initialPackageChoices)

  if (length(initialPackageChoices) > 0) {
    packageScanStatus("Showing s20x now while other installed packages are checked.")
  } else {
    packageScanStatus("Checking installed packages for datasets.")
  }

  output$packageScanStatus = renderText({
    packageScanStatus() %||% ""
  })

  output$exampleLoadStatus = renderText({
    exampleLoadStatus() %||% ""
  })

  observe({
    choices = exampleChoices()
    selected = isolate(input$exampleName %||% "")

    if (!nzchar(selected) || !(selected %in% choices)) {
      selected = if (length(choices) > 0) choices[1] else ""
    }

    updateSelectInput(
      session,
      "exampleName",
      choices = choices,
      selected = selected
    )
  })

  observe({
    choices = packageChoices()
    selected = isolate(input$data_package %||% "")

    if (!nzchar(selected) || !(selected %in% choices)) {
      if ("s20x" %in% choices) {
        selected = "s20x"
      } else if (length(choices) > 0) {
        selected = choices[1]
      } else {
        selected = ""
      }
    }

    updateSelectInput(
      session,
      "data_package",
      choices = choices,
      selected = selected
    )

    if (length(choices) == 0) {
      updateSelectInput(
        session,
        "package_dataset",
        choices = character(0),
        selected = character(0)
      )
    }
  })

  session$onFlushed(function() {
    exampleChoices(listWMFMExamples(includeTestExamples = isTRUE(input$developerMode)))

    packageNames = getInstalledPackagesWithData()

    if (length(packageNames) == 0) {
      packageChoices(character(0))
      packageScanStatus("No installed packages with datasets were found.")
      return(NULL)
    }

    packageChoices(packageNames)

    if (length(setdiff(packageNames, initialPackageChoices)) > 0) {
      packageScanStatus(
        paste0(
          "Found ",
          length(packageNames),
          " installed package",
          if (length(packageNames) == 1) "" else "s",
          " with datasets."
        )
      )
    } else {
      packageScanStatus(NULL)
    }
  }, once = TRUE)

  observeEvent(input$developerMode, {
    exampleChoices(listWMFMExamples(includeTestExamples = isTRUE(input$developerMode)))
  }, ignoreInit = TRUE)

  observeEvent(input$data_package, {

    req(input$data_source == "package")

    pkg = input$data_package %||% ""

    if (identical(pkg, "s20x")) {
      s20xOk = tryCatch(
        {
          ensureS20xInstalled()
          TRUE
        },
        error = function(e) {
          showNotification(conditionMessage(e), type = "error")
          FALSE
        }
      )

      if (!isTRUE(s20xOk)) {
        updateSelectInput(
          session,
          "package_dataset",
          choices = c("s20x is not installed" = ""),
          selected = ""
        )
        return(NULL)
      }
    }

    dsNames = getPackageDatasetNames(pkg)

    if (length(dsNames) == 0) {
      updateSelectInput(
        session,
        "package_dataset",
        choices = c("No datasets found" = ""),
        selected = ""
      )
      return(NULL)
    }

    choices = c("Choose a data set..." = "", dsNames)

    updateSelectInput(
      session,
      "package_dataset",
      choices = choices,
      selected = ""
    )
  }, ignoreInit = FALSE)

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
  contrastResultText = reactiveVal("")

  refreshOllamaModelChoices = function(selected = NULL) {

    baseUrl = getOption(
      "wmfm.ollama_base_url",
      default = "http://corrin.stat.auckland.ac.nz:11434"
    )

    modelIds = tryCatch({
      res = ellmer::models_ollama(base_url = baseUrl)

      ids = NULL
      if (is.data.frame(res) && "id" %in% names(res)) {
        ids = as.character(res$id)
      } else if (is.atomic(res)) {
        ids = as.character(res)
      }

      ids = ids[!is.na(ids) & nzchar(ids)]
      ids = unique(ids)

      if (length(ids) == 0) {
        ids = "gpt-oss"
      }

      ids
    }, error = function(e) {
      showNotification(
        paste0("Could not retrieve Ollama models. Using current/default choices. Details: ", conditionMessage(e)),
        type = "warning",
        duration = 8
      )

      fallback = rv$availableOllamaModels %||% "gpt-oss"
      fallback = fallback[!is.na(fallback) & nzchar(fallback)]
      if (length(fallback) == 0) {
        fallback = "gpt-oss"
      }
      unique(as.character(fallback))
    })

    rv$availableOllamaModels = modelIds

    target = selected %||% rv$activeOllamaModel %||% getOption("wmfm.ollama_model", default = "gpt-oss")
    if (!(target %in% modelIds)) {
      target = if ("gpt-oss" %in% modelIds) "gpt-oss" else modelIds[1]
    }

    updateSelectInput(
      session,
      "ollama_model",
      choices = stats::setNames(modelIds, modelIds),
      selected = target
    )

    invisible(target)
  }

  observe({
    refreshOllamaModelChoices(selected = rv$activeOllamaModel %||% "gpt-oss")
  })

  observeEvent(input$refreshOllamaModelsBtn, {
    refreshOllamaModelChoices(selected = input$ollama_model %||% rv$activeOllamaModel %||% "gpt-oss")
  }, ignoreInit = TRUE)

  output$chatProviderStatus = renderText({
    if (identical(rv$activeChatBackend %||% "ollama", "claude")) {
      return("Current provider: Claude")
    }

    paste0(
      "Current provider: Ollama (model: ",
      rv$activeOllamaModel %||% "gpt-oss",
      ")"
    )
  })

  observeEvent(input$applyChatProviderBtn, {
    requested = tolower(trimws(input$chat_provider %||% "ollama"))

    if (!requested %in% c("ollama", "claude")) {
      updateSelectInput(session, "chat_provider", selected = rv$activeChatBackend)
      showNotification("Unknown provider selected.", type = "error", duration = 6)
      return(NULL)
    }

    if (identical(requested, "claude")) {
      passwordOk = tryCatch(
        verifyProviderSwitchPassword(input$providerSwitchPassword %||% ""),
        error = function(e) {
          showNotification(conditionMessage(e), type = "error", duration = 8)
          FALSE
        }
      )

      if (!isTRUE(passwordOk)) {
        updateSelectInput(session, "chat_provider", selected = rv$activeChatBackend)
        session$sendInputMessage("providerSwitchPassword", list(value = ""))
        showNotification("Incorrect password. Claude was not enabled.", type = "error", duration = 6)
        return(NULL)
      }
    }

    selectedModel = input$ollama_model %||% rv$activeOllamaModel %||% "gpt-oss"
    availableModels = rv$availableOllamaModels %||% "gpt-oss"
    if (length(availableModels) == 0) {
      availableModels = "gpt-oss"
    }
    if (!(selectedModel %in% availableModels)) {
      selectedModel = if ("gpt-oss" %in% availableModels) "gpt-oss" else availableModels[1]
    }

    rv$activeChatBackend = requested
    if (identical(requested, "ollama")) {
      rv$activeOllamaModel = selectedModel
    }

    session$sendInputMessage("providerSwitchPassword", list(value = ""))

    msg = if (identical(requested, "claude")) {
      "Chat provider set to Claude."
    } else {
      paste0("Chat provider set to Ollama using model '", rv$activeOllamaModel, "'.")
    }

    showNotification(
      msg,
      type = "message",
      duration = 4
    )
  }, ignoreInit = TRUE)


  # -------------------------------------------------------------------
  # Main tabs UI (conditionally includes a Contrasts tab for factor-only models)
  # -------------------------------------------------------------------
  # -------------------------------------------------------------------
  # Main tabs UI (tabset skeleton lives in UI; each tab is rendered here)
  # -------------------------------------------------------------------

  output$contrasts_content_ui = renderUI({

    m = modelFit()
    showContrasts = !is.null(m) && isFactorOnlyPredictorModel(m)

    if (showContrasts) {

      tagList(
        h4("Contrasts (factor-only models)"),
        helpText(
          "Build a small set of meaningful comparisons. ",
          "Avoid computing lots of contrasts without a clear question."
        ),

        hr(),

        radioButtons(
          inputId = "contrastCiType",
          label = "Confidence intervals",
          choices = c(
            "Standard (model-based)" = "standard",
            "Robust (sandwich)"      = "sandwich"
          ),
          selected = "standard",
          inline = TRUE
        ),

        conditionalPanel(
          condition = "input.contrastCiType == 'sandwich'",
          selectInput(
            inputId = "contrastHcType",
            label = "Robust (sandwich) type",
            choices = c("HC0", "HC3"),
            selected = "HC0"
          ),
          helpText(
            "HC3 is more conservative in small samples; HC0 is the basic robust option."
          )
        ),

        hr(),

        radioButtons(
          inputId = "contrastType",
          label = "Contrast type",
          choices = c(
            "Compare pairs of levels"        = "pairwise",
            "Level vs average of others"     = "vsAverage",
            "Custom contrast (advanced)"     = "custom"
          ),
          selected = "pairwise"
        ),

        uiOutput("contrastUi"),

        hr(),
        htmlOutput("contrastResult")
      )

    } else {

      msg = if (is.null(m)) {
        "Fit a model first to enable contrasts."
      } else {
        "Contrasts are only available when the fitted model has factor predictors only (no numeric predictors)."
      }

      tagList(
        h4("Contrasts"),
        helpText(msg),
        tags$ul(
          tags$li("Go to the Model tab and fit a factor-only model."),
          tags$li("If you need comparisons for models with numeric predictors, use predicted values or marginal effects instead.")
        )
      )
    }

  })


  setBucketState = function(factors = NULL, continuous = NULL) {
    vars = rv$allVars %||% character(0)

    nextFactors = intersect(factors %||% character(0), vars)
    nextContinuous = intersect(continuous %||% character(0), vars)

    currentFactors = rv$bucketFactors %||% character(0)
    currentContinuous = rv$bucketContinuous %||% character(0)

    changed = FALSE

    if (!identical(currentFactors, nextFactors)) {
      rv$bucketFactors = nextFactors
      changed = TRUE
    }

    if (!identical(currentContinuous, nextContinuous)) {
      rv$bucketContinuous = nextContinuous
      changed = TRUE
    }

    invisible(changed)
  }

  resetModelPage = function(resetResponse = TRUE) {

    rv$isResetting = TRUE
    on.exit({
      rv$isResetting = FALSE
    }, add = TRUE)

    # Clear fitted model + LLM outputs
    modelFit(NULL)
    rv$modelEquations = NULL
    rv$modelExplanation = NULL
    rv$modelExplanationAudit = NULL
    rv$modelExplanationTutor = NULL
    rv$modelContext = NULL

    # Reset tracking + factor prompt state
    rv$autoFormula = ""
    rv$lastResponse = NULL
    rv$lastFactors = character(0)
    rv$pendingFactorVar = NULL
    rv$pendingExampleInteractions = character(0)

    # Clear bucket state
    setBucketState(
      factors = character(0),
      continuous = character(0)
    )

    # Force buckets to re-render empty (Variables/Factors/Continuous)
    rv$bucketGroupId = rv$bucketGroupId + 1L

    # Reset model UI inputs
    updateRadioButtons(session, "model_type", selected = "lm")

    # Prevent stale widget values from fighting the reset
    freezeReactiveValue(input, "factors")
    freezeReactiveValue(input, "continuous")
    freezeReactiveValue(input, "interactions")
    freezeReactiveValue(input, "formula_text")
    freezeReactiveValue(input, "response_var")

    updateTextInput(session, "formula_text", value = "")
    updateSelectInput(session, "interactions", selected = character(0))
    updateTextInput(session, "researchQuestion", value = rv$researchQuestion %||% "")

    # Reset response var to first column of new data (optional)
    if (resetResponse && !is.null(rv$data) && length(rv$allVars) > 0) {
      updateSelectInput(
        session,
        "response_var",
        choices = rv$allVars,
        selected = rv$allVars[1]
      )
    }
  }

  applyLoadedExampleToInputs = function(exampleInfo) {

    spec = exampleInfo$spec %||% list()
    exampleFormula = stats::as.formula(spec$formula)
    responseVar = all.vars(exampleFormula[[2]])[1] %||% rv$allVars[1]
    termLabels = attr(stats::terms(exampleFormula), "term.labels") %||% character(0)
    interactionTerms = termLabels[grepl(":", termLabels, fixed = TRUE)]
    mainEffectTerms = termLabels[!grepl(":", termLabels, fixed = TRUE)]

    factorVars = mainEffectTerms[vapply(mainEffectTerms, function(varName) {
      column = rv$data[[varName]]
      is.factor(column) || is.character(column)
    }, logical(1))]

    continuousVars = setdiff(mainEffectTerms, factorVars)

    setBucketState(
      factors = intersect(factorVars, rv$allVars),
      continuous = intersect(continuousVars, rv$allVars)
    )
    rv$bucketGroupId = rv$bucketGroupId + 1L
    rv$lastFactors = rv$bucketFactors
    rv$lastResponse = responseVar
    rv$pendingExampleInteractions = interactionTerms

    updateSelectInput(
      session,
      "response_var",
      choices = rv$allVars,
      selected = responseVar
    )

    updateRadioButtons(
      session,
      "model_type",
      selected = spec$modelType %||% "lm"
    )

    updateSelectInput(
      session,
      "interactions",
      selected = interactionTerms
    )

    rv$autoFormula = spec$formula %||% ""
    updateTextInput(session, "formula_text", value = spec$formula %||% "")
    updateTextInput(session, "researchQuestion", value = exampleInfo$researchQuestion %||% "")
  }

  # -------------------------------------------------------------------
  # Plot of data + fitted model
  # -------------------------------------------------------------------
  output$plot_ci_controls_ui = renderUI({

    m = modelFit()
    req(m)

    mf = model.frame(m)

    # Exclude response column; remaining columns are predictors.
    # Identify numeric predictors (continuous covariates).
    respName = all.vars(formula(m))[1]
    predNames = setdiff(names(mf), respName)

    isNumPred = vapply(mf[, predNames, drop = FALSE], is.numeric, logical(1))
    numPredCount = sum(isNumPred)

    mode = if (identical(numPredCount, 1L)) "continuous" else "factorOnly"

    plotCiControlsUi(mode = mode)
  })

  output$model_plot = renderPlot({
    m = modelFit()
    req(m)

    res = drawModelPlot(
      model  = m,
      ciType = input$plotCiType %||% "standard",
      hcType = input$plotHcType %||% "HC0",
      showCi = isTRUE(input$plotShowCi %||% FALSE),
      level  = input$plotCiLevel %||% 0.95
    )

    if (inherits(res, "ggplot")) {
      print(res)
    } else if (inherits(res, "recordedplot")) {
      replayPlot(res)
    }

    invisible(NULL)
  })

  # ---- Contrasts UI + computation (factor-only models) ----

  output$contrastUi = renderUI({

    m = modelFit()
    req(m)

    mf = model.frame(m)
    factorPreds = getFactorOnlyPredictors(m, mf)
    req(length(factorPreds) >= 1)

    # Choose the factor to contrast
    targetFactorChoices = factorPreds

    targetFactor = isolate(input$contrastFactor)
    if (is.null(targetFactor) || !(targetFactor %in% targetFactorChoices)) {
      targetFactor = targetFactorChoices[1]
    }

    # Conditioning factors (other factors held fixed)
    condFactors = setdiff(factorPreds, targetFactor)

    condUi = lapply(condFactors, function(v) {
      selectInput(
        inputId = paste0("contrastCond_", v),
        label = paste0("Hold ", v, " at:"),
        choices = levels(mf[[v]]),
        selected = levels(mf[[v]])[1]
      )
    })

    # Levels for the target factor
    levs = levels(mf[[targetFactor]])
    levA = levs[1]
    levB = if (length(levs) >= 2) levs[2] else levs[1]

    tagList(
      if (length(targetFactorChoices) > 1) {
        selectInput(
          inputId = "contrastFactor",
          label = "Factor to compare:",
          choices = targetFactorChoices,
          selected = targetFactor
        )
      } else {
        selectInput(
          inputId = "contrastFactor",
          label = "Factor to compare:",
          choices = targetFactorChoices,
          selected = targetFactor
        )
      },

      if (length(condUi) > 0) {
        tagList(
          h5("Condition on other factors"),
          helpText("These factors are held fixed while comparing levels of the chosen factor."),
          condUi,
          hr()
        )
      },

      conditionalPanel(
        condition = "input.contrastType == 'pairwise'",

        fluidRow(
          column(
            width = 4,
            selectInput(
              inputId = "contrastLevel1",
              label = "Compare level:",
              choices = levs,
              selected = levA
            )
          ),
          column(
            width = 4,
            selectInput(
              inputId = "contrastLevel2",
              label = "Against level:",
              choices = levs,
              selected = levB
            )
          ),
          column(
            width = 4,
            div(style = "margin-top: 24px;"),
            actionButton(
              "addContrastBtn",
              "Add contrast",
              class = "btn btn-primary"
            )
          )
        ),

        div(style = "margin-top: 10px;"),
        h5("Contrasts"),
        helpText("Select one or more contrasts in the list below to remove them."),

        fluidRow(
          column(
            width = 8,
            selectInput(
              inputId = "contrastList",
              label = NULL,
              choices = contrastPairs(),
              selected = character(0),
              multiple = TRUE,
              selectize = FALSE,
              size = 6
            )
          ),
          column(
            width = 4,
            div(style = "margin-top: 6px;"),
            actionButton(
              "removeContrastBtn",
              "Remove selected",
              class = "btn btn-warning"
            ),
            div(style = "margin-top: 10px;"),
            actionButton(
              "computeContrastsBtn",
              "Compute contrasts",
              class = "btn btn-success"
            )
          )
        )
      ),

      conditionalPanel(
        condition = "input.contrastType == 'vsAverage'",

        h5("Compare averages of levels"),
        helpText("Drag levels into the boxes below. The contrast is Average(left) minus Average(right)."),

        bucket_list(
          header = NULL,
          group_name = "avgContrastLevels",
          orientation = "horizontal",
          add_rank_list(
            text = "Levels",
            input_id = "avgLevelsPool",
            labels = levs
          ),
          add_rank_list(
            text = "Average of:",
            input_id = "avgLeft",
            labels = character(0)
          ),
          add_rank_list(
            text = "To average of:",
            input_id = "avgRight",
            labels = character(0)
          )
        )
      ),

      conditionalPanel(
        condition = "input.contrastType == 'custom'",
        h5("Custom contrast weights (advanced)"),
        helpText("Enter weights for each level. The weights should sum to 0."),
        uiOutput("contrastWeightsUi"),
        uiOutput("customWeightsStatus")
      ),
      conditionalPanel(
        condition = "input.contrastType != 'pairwise'",
        actionButton(
          "computeContrastBtn",
          "Compute contrast",
          class = "btn btn-success"
        )
      )

    )
  })

  output$contrastWeightsUi = renderUI({

    m = modelFit()
    req(m)

    mf = model.frame(m)
    factorPreds = getFactorOnlyPredictors(m, mf)
    req(length(factorPreds) >= 1)

    targetFactor = input$contrastFactor
    if (is.null(targetFactor) || !(targetFactor %in% factorPreds)) {
      targetFactor = factorPreds[1]
    }

    levs = levels(mf[[targetFactor]])
    n = length(levs)

    # Default: a simple A vs B contrast if possible
    defaults = rep(0, n)
    if (n >= 2) {
      defaults[1] = 1
      defaults[2] = -1
    } else if (n == 1) {
      defaults[1] = 0
    }

    tagList(
      lapply(seq_along(levs), function(i) {
        textInput(
          inputId = paste0("contrastW_", i),
          label = levs[i],
          value = as.character(defaults[i]),
          placeholder = "e.g. 1, -1, 1/2"
        )
      })
    )
  })

  output$customWeightsStatus = renderUI({
    m = modelFit()
    req(m)

    if (!isTRUE(input$contrastType == "custom")) {
      return(NULL)
    }

    mf = model.frame(m)
    factorPreds = getFactorOnlyPredictors(m, mf)
    req(length(factorPreds) >= 1)

    targetFactor = input$contrastFactor
    if (is.null(targetFactor) || !(targetFactor %in% factorPreds)) {
      targetFactor = factorPreds[1]
    }

    levs = levels(mf[[targetFactor]])
    n = length(levs)

    w = sapply(seq_len(n), function(i) parseWeightText(input[[paste0("contrastW_", i)]]))
    if (any(is.na(w))) {
      return(tags$div(
        style = "margin-top: 8px; padding: 8px; border: 1px solid #f0ad4e; background: #fcf8e3;",
        "Weights must be numbers (decimals) or simple fractions like 1/2."
      ))
    }

    s = sum(w)
    if (abs(s) > 1e-8) {
      return(tags$div(
        style = "margin-top: 8px; padding: 8px; border: 1px solid #d9534f; background: #f2dede;",
        paste0("Weights must sum to 0. Current sum: ", formatC(s, digits = 6, format = "fg"), ".")
      ))
    }

    tags$div(
      style = "margin-top: 8px; padding: 8px; border: 1px solid #5cb85c; background: #dff0d8;",
      "Weights sum to 0."
    )
  })

  output$veSummaryUi = renderUI({
    d = rv$data
    req(d, input$veVar)

    x = d[[input$veVar]]

    if (is.numeric(x)) {
      tagList(
        h4("Numeric summary"),
        tableOutput("veNumericTable")
      )
    } else if (is.factor(x) || is.character(x)) {
      tagList(
        h4("Categorical summary"),
        uiOutput("veCatSummaryUi")
      )
    } else {
      tagList(
        h4("Summary"),
        helpText("This variable type is not currently summarised.")
      )
    }
  })

  output$veNumericTable = renderTable({
    d = rv$data
    req(d, input$veVar)

    x = d[[input$veVar]]
    validate(need(is.numeric(x), "Selected variable is not numeric."))

    nMissing = sum(is.na(x))
    xNoNa = x[!is.na(x)]
    nObs = length(xNoNa)

    if (nObs == 0) {
      return(
        data.frame(
          Metric = c("N", "N missing"),
          Value = c("0", as.character(nMissing)),
          stringsAsFactors = FALSE
        )
      )
    }

    qs = quantile(xNoNa, probs = c(0.25, 0.75), names = FALSE, type = 7)

    metric = c(
      "N",
      "N missing",
      "Min",
      "Max",
      "Mean",
      "Median",
      "SD",
      "LQ (25%)",
      "UQ (75%)"
    )

    rawValue = c(
      nObs,
      nMissing,
      min(xNoNa),
      max(xNoNa),
      mean(xNoNa),
      median(xNoNa),
      sd(xNoNa),
      qs[1],
      qs[2]
    )

    value = c(
      as.character(nObs),
      as.character(nMissing),
      vapply(rawValue[3:length(rawValue)], formatSummaryValue, character(1))
    )

    data.frame(
      Metric = metric,
      Value = value,
      stringsAsFactors = FALSE
    )
  }, striped = TRUE, bordered = TRUE, spacing = "s")

  output$veCatSummaryUi = renderUI({
    d = rv$data
    req(d, input$veVar)

    x = d[[input$veVar]]
    validate(need(is.factor(x) || is.character(x), "Selected variable is not categorical."))

    xChar = as.character(x)
    xNoNa = xChar[!is.na(xChar)]

    uniq = sort(unique(xNoNa))
    nUniq = length(uniq)
    nObs = length(xNoNa)
    nMissing = sum(is.na(x))

    tagList(
      helpText(paste0("N (non-missing): ", nObs)),
      helpText(paste0("N missing: ", nMissing)),
      helpText(paste0("Unique values: ", nUniq)),
      tableOutput("veCatTable")
    )
  })

  output$veCatTable = renderTable({
    d = rv$data
    req(d, input$veVar)

    x = d[[input$veVar]]
    validate(need(is.factor(x) || is.character(x), "Selected variable is not categorical."))

    nMissing = sum(is.na(x))

    if (is.factor(x)) {
      tab = table(x, useNA = "no")
      freq = data.frame(
        Value = names(tab),
        Count = as.integer(tab),
        stringsAsFactors = FALSE
      )
    } else {
      xChar = as.character(x)
      xNoNa = xChar[!is.na(xChar)]

      if (length(xNoNa) == 0) {
        return(NULL)
      }

      tab = table(xNoNa)
      freq = data.frame(
        Value = names(tab),
        Count = as.integer(tab),
        stringsAsFactors = FALSE
      )

      freq = freq[order(freq$Value), , drop = FALSE]
    }

    if (nrow(freq) < 10) {
      return(freq)
    }

    top = freq[seq_len(10), , drop = FALSE]
    otherCount = sum(freq$Count) - sum(top$Count)

    if (otherCount > 0) {
      top = rbind(
        top,
        data.frame(
          Value = "Other",
          Count = otherCount,
          stringsAsFactors = FALSE
        )
      )
    }

    top
  }, striped = TRUE, bordered = TRUE, spacing = "s")
  output$vePlot = renderPlot({
    d = rv$data
    req(d, input$veVar)

    x = d[[input$veVar]]

    if (!is.numeric(x)) {
      return(NULL)
    }

    xNoNa = x[!is.na(x)]

    if (length(xNoNa) < 5) {
      return(NULL)
    }

    if (length(unique(xNoNa)) < 2) {
      return(NULL)
    }

    v = var(xNoNa)
    if (is.na(v) || v == 0) {
      return(NULL)
    }

    ggplot(data.frame(x = xNoNa), aes(x = x)) +
      geom_histogram(
        aes(y = after_stat(density)),
        bins = 30,
        fill = "lightblue",
        colour = "black",
        linewidth = 0.3
      ) +
      geom_density(linewidth = 0.8) +
      labs(
        title = paste0("Distribution of ", input$veVar),
        x = input$veVar,
        y = "Density"
      ) +
      theme_bw()
  })

  # --- Keep bucket states synced

  observeEvent(input$factors, {
    if (isTRUE(rv$isResetting)) {
      return(NULL)
    }

    cur = input$factors %||% character(0)
    setBucketState(
      factors = cur,
      continuous = rv$bucketContinuous %||% character(0)
    )
  }, ignoreInit = TRUE)

  observeEvent(input$continuous, {
    if (isTRUE(rv$isResetting)) {
      return(NULL)
    }

    cur = input$continuous %||% character(0)
    setBucketState(
      factors = rv$bucketFactors %||% character(0),
      continuous = cur
    )
  }, ignoreInit = TRUE)


  # ---- Pairwise contrast list management ----

  observeEvent(input$addContrastBtn, {
    m = modelFit()
    req(m)
    req(isTRUE(input$contrastType == "pairwise"))

    a = input$contrastLevel1
    b = input$contrastLevel2
    req(!is.null(a), !is.null(b))

    if (identical(a, b)) {
      showNotification("Please choose two different levels.", type = "warning")
      return(NULL)
    }

    mf = model.frame(m)
    targetFactor = input$contrastFactor
    if (is.null(targetFactor) || !(targetFactor %in% names(mf))) {
      targetFactor = names(mf)[2]
    }

    levs = levels(mf[[targetFactor]])
    idxA = match(a, levs)
    idxB = match(b, levs)

    if (is.na(idxA) || is.na(idxB)) {
      # Fallback to alphabetical ordering
      canonical = sort(c(a, b))
      newLabel = paste0(canonical[1], " - ", canonical[2])
      revLabel = paste0(canonical[2], " - ", canonical[1])
    } else if (idxA <= idxB) {
      newLabel = paste0(a, " - ", b)
      revLabel = paste0(b, " - ", a)
    } else {
      newLabel = paste0(b, " - ", a)
      revLabel = paste0(a, " - ", b)
    }

    current = contrastPairs()
    if (newLabel %in% current || revLabel %in% current) {
      showNotification("That contrast is already in the list (possibly reversed).", type = "message")
      return(NULL)
    }

    contrastPairs(c(current, newLabel))
  })

  observeEvent(input$removeContrastBtn, {
    current = contrastPairs()
    sel = input$contrastList %||% character(0)

    if (length(sel) == 0) {
      showNotification("Select one or more contrasts to remove.", type = "message")
      return(NULL)
    }

    contrastPairs(setdiff(current, sel))
  })

  observeEvent(rv$data, {
    d = rv$data

    if (is.null(d)) {
      updateSelectInput(
        session = session,
        inputId = "veVar",
        choices = character(0),
        selected = character(0)
      )
      return()
    }

    vars = names(d)

    updateSelectInput(
      session = session,
      inputId = "veVar",
      choices = vars,
      selected = vars[1]
    )
  }, ignoreInit = TRUE)

  # Reset contrasts when the model is reset/refit
  observeEvent(modelFit(), {
    contrastPairs(character(0))
    contrastResultText("")
  })

  # ---- Contrast computation ----

  computeOneContrastText = function(m, mf, targetFactor, factorPreds, condValues, weights, label) {

    built = buildContrastNewData(mf, factorPreds, targetFactor, condValues)
    newData = built$newData
    targetLevels = built$targetLevels

    if (length(weights) != length(targetLevels)) {
      stop("weights length does not match the number of target levels.")
    }

    ciType = input$contrastCiType %||% "standard"
    hcType = input$contrastHcType %||% "HC0"

    res = computeFactorOnlyContrast(
      model = m,
      newData = newData,
      weights = weights,
      ciType = ciType,
      hcType = hcType,
      level = 0.95
    )

    isGlm = inherits(m, "glm")
    link = if (isGlm) family(m)$link else "identity"
    # ---- Step 1: detect common response transformations (lm only) ----
    respExpr = tryCatch(deparse(formula(m)[[2]]), error = function(e) "")
    respExpr = paste(respExpr, collapse = "")

    respTransform = if (isGlm) "none" else detectRespTransform(respExpr)

    # ---- Step 2: choose the effective interpretation scale ----
    # This tells the app what kind of language to use for contrasts.
    effectiveScale =
      if (isGlm) {
        link  # "identity", "log", "logit", ...
      } else if (identical(respTransform, "none")) {
        "identity"
      } else if (respTransform %in% c("log", "log10")) {
        "log"
      } else {
        "other"
      }

    nounPhrase =
      attr(m, "wmfm_response_noun_phrase", exact = TRUE) %||%
      all.vars(formula(m)[[2]])[1] %||%
      "the outcome"

    scaleRules = buildScalePhrasingRules(isGlm, effectiveScale, respTransform, nounPhrase)

    # Only show eta contrast when it is genuinely a different scale
    showEtaLine = isGlm && !identical(link, "identity")

    # Collect the detail lines (not including the heading label)
    detailLines = character(0)

    # Note for common lm() transforms
    if (!isGlm && respTransform %in% c("log", "log10")) {
      detailLines = c(
        detailLines,
        htmlEscape(
          paste0(
            "Note: The response was modelled on the ", respTransform,
            " scale; ratios of typical means are shown on the original scale."
          )
        )
      )
    } else if (!isGlm && identical(respTransform, "log1p")) {
      detailLines = c(
        detailLines,
        htmlEscape(
          "Note: The response was modelled on the log(1 + y) scale; differences are on that transformed scale."
        )
      )
    } else if (!isGlm && respTransform %in% c("sqrt", "inverse", "unknown")) {
      # keep this minimal for now; Step 3 will handle nicer language
      detailLines = c(
        detailLines,
        htmlEscape(
          "Note: The response was modelled on a transformed scale; interpret contrasts on that scale unless a back-transformation is explicitly shown."
        )
      )
    }


    if (showEtaLine) {
      detailLines = c(
        detailLines,
        htmlEscape(
          paste0(
            "eta contrast: ", fmt3(res$estEta),
            " (95% CI: ", fmt3(res$lowerEta), ", ", fmt3(res$upperEta), ")"
          )
        )
      )
    }

    if (!is.null(res$interpreted)) {

      # Default: show what computeFactorOnlyContrast gave us
      baseLine = paste0(
        res$interpreted$label, ": ", fmt3(res$interpreted$estimate),
        " (95% CI: ", fmt3(res$interpreted$lower), ", ", fmt3(res$interpreted$upper), ")"
      )

      # Step 2 enhancement: if lm() with log/log10 response, also show ratio on original scale
      if (!isGlm && respTransform %in% c("log", "log10")) {

        # back-transform difference on log/log10 scale into a ratio
        backFun = if (identical(respTransform, "log10")) function(x) 10^x else exp

        ratioEst = backFun(res$interpreted$estimate)
        ratioLo  = backFun(res$interpreted$lower)
        ratioHi  = backFun(res$interpreted$upper)

        ratioLine = paste0(
          "Ratio of typical means (original scale): ",
          fmt3(ratioEst),
          " (95% CI: ", fmt3(ratioLo), ", ", fmt3(ratioHi), ")"
        )

        # Show the ratio line (and optionally keep the transformed-scale difference line)
        detailLines = c(
          detailLines,
          htmlEscape(ratioLine),
          htmlEscape(paste0("On the ", respTransform, " scale: ", baseLine))
        )

      } else {
        detailLines = c(detailLines, htmlEscape(baseLine))
      }
    }


    # ---- LLM interpretation (optional) ----
    if (!is.null(rv$chatProvider) &&
        is.environment(rv$contrastLlmCache) &&
        requireNamespace("ellmer", quietly = TRUE) &&
        !is.null(res$interpreted) &&
        all(c("label", "estimate", "lower", "upper") %in% names(res$interpreted))) {

      ciType = input$contrastCiType %||% "standard"
      hcType = input$contrastHcType %||% "HC0"

      ciText =
        if (identical(ciType, "sandwich")) {
          paste0("robust (", hcType, ")")
        } else {
          "standard (model-based)"
        }

      userDoc = attr(m, "wmfm_dataset_doc", exact = TRUE) %||% ""
      userDoc = trimws(userDoc)

      userDocBlock = if (nzchar(userDoc)) {
        paste0("User-provided dataset context:\n", userDoc)
      } else {
        NULL
      }

      contrastPayload = paste(
        paste0("Contrast: ", label),
        paste0(
          res$interpreted$label, ": ",
          fmt3(res$interpreted$estimate),
          " (95% CI: ",
          fmt3(res$interpreted$lower), " to ",
          fmt3(res$interpreted$upper), ")"
        ),
        paste0("Confidence interval type: ", ciText),
        userDocBlock,
        sep = "\n"
      )

      contrastOutputRules = paste(
        "Contrast output rules:",
        "- Write ONE clear sentence.",
        "- You MUST mention the point estimate, the 95% confidence interval, and whether the interval is robust or standard.",
        "- Do NOT say \\'minus\\' or \\'negative\\'; use \\'higher/lower\\' (or \\'more/fewer\\') wording instead.",
        "- Avoid symbols and avoid technical jargon.",
        sep = "\n"
      )

      prompt = composeWmfmPrompt(
        context = "contrast",
        contextPayload = contrastPayload,
        scaleRules = paste(scaleRules, contrastOutputRules, sep = "\n\n")
      )

      key = paste(
        ciType, hcType, label,
        fmt3(res$interpreted$estimate),
        fmt3(res$interpreted$lower),
        fmt3(res$interpreted$upper),
        sep = "|"
      )

      llmText = NULL

      withProgress(message = "Generating interpretation...", value = 0, {

        incProgress(0.3, detail = "Sending request")

        llmText =
          if (exists(key, envir = rv$contrastLlmCache, inherits = FALSE)) {
            get(key, envir = rv$contrastLlmCache, inherits = FALSE)
          } else {
            tmp = tryCatch(
              rv$chatProvider$chat(prompt),
              error = function(e) paste("LLM error:", conditionMessage(e))
            )
            if (!is.null(tmp) && nzchar(tmp)) {
              tmp = trimws(gsub("[[:space:]]+", " ", tmp))
              assign(key, tmp, envir = rv$contrastLlmCache)
            }
            tmp
          }

        if (!is.null(llmText) && nzchar(llmText)) {
          detailLines = c(detailLines, htmlEscape(paste0("Interpretation: ", llmText)))
        }

        incProgress(1, detail = "Done")

      })
    }
    # ---- Add a simple note when the CI includes the null value ----
    if (!is.null(res$interpreted) &&
        all(c("estimate", "lower", "upper") %in% names(res$interpreted))) {

      nullValue = if (identical(effectiveScale, "identity")) 0 else 1

      ciCrossesNull =
        is.finite(res$interpreted$lower) &&
        is.finite(res$interpreted$upper) &&
        (res$interpreted$lower <= nullValue) &&
        (res$interpreted$upper >= nullValue)

      if (ciCrossesNull) {
        nullLabel = if (identical(nullValue, 0)) "0" else "1"
        detailLines = c(
          detailLines,
          paste0(
            "<span style='font-style: italic;'>",
            htmlEscape(
              paste0(
                "Note: Because the 95% CI includes ", nullLabel,
                ", the data are also consistent with there being little or no true difference."
              )
            ),
            "</span>"
          )
        )
      }
    }

    # ---- Build nice HTML block ----
    # Heading (bold label)
    headingHtml = paste0(
      "<strong>",
      htmlEscape(label),
      "</strong>"
    )

    # Body lines, indented
    if (length(detailLines) == 0) {
      bodyHtml = ""
    } else {
      bodyHtml = paste0(
        "<div style='margin-left: 1em;'>",
        paste0(detailLines, collapse = "<br>"),
        "</div>"
      )
    }

    htmlBlock = paste0(
      "<div style='margin-bottom: 1em;'>",
      headingHtml, "<br>",
      bodyHtml,
      "</div>"
    )

    htmlBlock
  }

  observeEvent(input$computeContrastsBtn, {

    m = modelFit()
    req(m)
    req(isTRUE(input$contrastType == "pairwise"))

    mf = model.frame(m)
    factorPreds = getFactorOnlyPredictors(m, mf)
    validate(need(length(factorPreds) >= 1, "No factor predictors available for contrasts."))

    targetFactor = input$contrastFactor
    if (is.null(targetFactor) || !(targetFactor %in% factorPreds)) {
      targetFactor = factorPreds[1]
    }

    # Conditioning values for other factors
    condFactors = setdiff(factorPreds, targetFactor)
    condValues = list()
    for (v in condFactors) {
      inp = input[[paste0("contrastCond_", v)]]
      if (is.null(inp)) {
        inp = levels(mf[[v]])[1]
      }
      condValues[[v]] = inp
    }

    pairs = contrastPairs()
    if (length(pairs) == 0) {
      showNotification("Add at least one contrast first.", type = "warning")
      return(NULL)
    }

    levs = levels(mf[[targetFactor]])

    htmlBlocks = lapply(pairs, function(lbl) {
      parts = strsplit(lbl, " - ", fixed = TRUE)[[1]]
      a = parts[1]
      b = parts[2]

      w = rep(0, length(levs))
      w[match(a, levs)] = 1
      w[match(b, levs)] = -1

      computeOneContrastText(
        m = m,
        mf = mf,
        targetFactor = targetFactor,
        factorPreds = factorPreds,
        condValues = condValues,
        weights = w,
        label = paste0(targetFactor, ": ", a, " - ", b)
      )
    })

    contrastResultText(paste(unlist(htmlBlocks), collapse = "<hr style='margin: 0.8em 0;'>"))
  })

  observeEvent(input$computeContrastBtn, {

    m = modelFit()
    req(m)
    req(!isTRUE(input$contrastType == "pairwise"))

    mf = model.frame(m)
    factorPreds = getFactorOnlyPredictors(m, mf)
    validate(need(length(factorPreds) >= 1, "No factor predictors available for contrasts."))

    targetFactor = input$contrastFactor
    if (is.null(targetFactor) || !(targetFactor %in% factorPreds)) {
      targetFactor = factorPreds[1]
    }

    condFactors = setdiff(factorPreds, targetFactor)
    condValues = list()
    for (v in condFactors) {
      inp = input[[paste0("contrastCond_", v)]]
      if (is.null(inp)) {
        inp = levels(mf[[v]])[1]
      }
      condValues[[v]] = inp
    }

    levs = levels(mf[[targetFactor]])
    n = length(levs)

    if (isTRUE(input$contrastType == "vsAverage")) {

      leftLevels = input$avgLeft %||% character(0)
      rightLevels = input$avgRight %||% character(0)

      if (length(leftLevels) == 0 || length(rightLevels) == 0) {
        showNotification("Drag at least one level into each average box.", type = "warning")
        return(NULL)
      }

      if (any(!(leftLevels %in% levs)) || any(!(rightLevels %in% levs))) {
        showNotification("One or more selected levels are not valid for this factor.", type = "warning")
        return(NULL)
      }

      if (length(intersect(leftLevels, rightLevels)) > 0) {
        showNotification("A level cannot appear in both average boxes.", type = "warning")
        return(NULL)
      }

      w = rep(0, n)
      w[match(leftLevels, levs)] = 1 / length(leftLevels)
      w[match(rightLevels, levs)] = -1 / length(rightLevels)

      label = paste0(
        targetFactor,
        ": avg(",
        paste(leftLevels, collapse = ", "),
        ") - avg(",
        paste(rightLevels, collapse = ", "),
        ")"
      )

    } else {

      # custom weights (allow fractions like 1/2)
      parseWeightText = function(x) {
        x = trimws(x %||% "0")
        if (x == "") {
          return(0)
        }
        if (grepl("^[-+]?[0-9]*\\.?[0-9]+$", x)) {
          return(as.numeric(x))
        }
        if (grepl("^[-+]?[0-9]+[[:space:]]*/[[:space:]]*[0-9]+$", x)) {
          parts = strsplit(gsub("\\s+", "", x), "/", fixed = TRUE)[[1]]
          num = as.numeric(parts[1])
          den = as.numeric(parts[2])
          if (is.finite(num) && is.finite(den) && den != 0) {
            return(num / den)
          }
        }
        return(NA_real_)
      }

      w = sapply(seq_len(n), function(i) parseWeightText(input[[paste0("contrastW_", i)]]))
      label = paste0(targetFactor, ": custom weights")

      if (any(is.na(w))) {
        showNotification("Custom weights must be numeric (decimals) or simple fractions like 1/2.", type = "warning")
        return(NULL)
      }

      if (abs(sum(w)) > 1e-8) {
        showNotification("Custom weights must sum to 0.", type = "warning")
        return(NULL)
      }
      if (sum(w != 0) < 2) {
        showNotification("Use at least two non-zero weights.", type = "warning")
        return(NULL)
      }
    }

    txt = computeOneContrastText(
      m = m,
      mf = mf,
      targetFactor = targetFactor,
      factorPreds = factorPreds,
      condValues = condValues,
      weights = w,
      label = label
    )

    contrastResultText(txt)
  })

  output$contrastResult = renderUI({
    HTML(contrastResultText())
  })


  # -------------------------------------------------------------------
  # Symbolic model formula (LaTeX via MathJax)
  # -------------------------------------------------------------------
  output$model_formula = renderUI({
    m = modelFit()
    if (is.null(m)) {
      return(helpText("Fit a model to see the model formula."))
    }

    mf = model.frame(m)
    response = names(mf)[1]

    # Terms object gives us main effects + interactions
    tt = terms(m)
    termLabels = attr(tt, "term.labels")
    mainLabels = termLabels[!grepl(":", termLabels)]
    intLabels  = termLabels[grepl(":", termLabels)]

    # ----- Build RHS: main effects first -----
    termsTex = c("\\beta_0")
    betaIdx = 1L

    for (lbl in mainLabels) {
      v = lbl
      x = mf[[v]]

      if (is.factor(x)) {
        lvls = levels(x)
        if (length(lvls) >= 2) {
          # One indicator per non-reference level
          for (lvl in lvls[-1]) {
            termsTex = c(
              termsTex,
              glue("\\beta_{betaIdx} \\times \\mathbf{{1}}\\{{ {v}_i = \\text{{\"{lvl}\"}} \\}}")
            )
            betaIdx = betaIdx + 1L
          }
        }
      } else {
        # Numeric predictor
        termsTex = c(
          termsTex,
          glue("\\beta_{betaIdx} \\times {v}_i")
        )
        betaIdx = betaIdx + 1L
      }
    }

    # ----- Add interaction terms -----
    for (lbl in intLabels) {
      vars = strsplit(lbl, ":", fixed = TRUE)[[1]]

      # ---------------- Two-way interactions: keep detailed expansion ----------------
      if (length(vars) == 2) {
        v1 = vars[1]
        v2 = vars[2]
        x1 = mf[[v1]]
        x2 = mf[[v2]]

        isFac1 = is.factor(x1)
        isFac2 = is.factor(x2)

        # numeric x numeric
        if (!isFac1 && !isFac2) {
          termsTex = c(
            termsTex,
            glue("\\beta_{betaIdx} \\times {v1}_i \\times {v2}_i")
          )
          betaIdx = betaIdx + 1L

          # factor x numeric
        } else if (isFac1 && !isFac2) {
          lvls1 = levels(x1)
          for (lvl1 in lvls1[-1]) {
            termsTex = c(
              termsTex,
              glue("\\beta_{betaIdx} \\times {v2}_i \\times \\mathbf{{1}}\\{{ {v1}_i = \\text{{\"{lvl1}\"}} \\}}")
            )
            betaIdx = betaIdx + 1L
          }

          # numeric x factor
        } else if (!isFac1 && isFac2) {
          lvls2 = levels(x2)
          for (lvl2 in lvls2[-1]) {
            termsTex = c(
              termsTex,
              glue("\\beta_{betaIdx} \\times {v1}_i \\times \\mathbf{{1}}\\{{ {v2}_i = \\text{{\"{lvl2}\"}} \\}}")
            )
            betaIdx = betaIdx + 1L
          }

          # factor x factor
        } else {
          lvls1 = levels(x1)
          lvls2 = levels(x2)
          for (lvl1 in lvls1[-1]) {
            for (lvl2 in lvls2[-1]) {
              termsTex = c(
                termsTex,
                glue(
                  "\\beta_{betaIdx} \\times \\mathbf{{1}}\\{{ {v1}_i = \\text{{\"{lvl1}\"}}, {v2}_i = \\text{{\"{lvl2}\"}} \\}}"
                )
              )
              betaIdx = betaIdx + 1L
            }
          }
        }

        # ---------------- Three-way interactions: generic product term ----------------
      } else if (length(vars) == 3) {
        v1 = vars[1]
        v2 = vars[2]
        v3 = vars[3]

        # For readability, we show a compact generic interaction term,
        # regardless of whether the variables are factors or numeric.
        termsTex = c(
          termsTex,
          glue("\\beta_{betaIdx} \\times {v1}_i \\times {v2}_i \\times {v3}_i")
        )
        betaIdx = betaIdx + 1L

        # Skip higher-order (4-way+) interactions if present
      } else {
        next
      }
    }

    # ----- LHS: depends on model type -----
    predictors = mainLabels       # for conditioning in E[. | .]
    if (inherits(m, "glm")) {
      fam  = m$family$family
      link = m$family$link

      if (fam == "binomial" && link == "logit") {
        lhs = "\\operatorname{logit}(p_i)"
      } else if (fam == "poisson" && link == "log") {
        lhs = "\\log(\\mu_i)"
      } else {
        if (length(predictors) > 0) {
          cond = paste0(predictors, "_i", collapse = ", ")
          lhs = glue("\\mathrm{{E}}[{response}_i \\mid {cond}]")
        } else {
          lhs = glue("\\mathrm{{E}}[{response}_i]")
        }
      }
    } else {
      if (length(predictors) > 0) {
        cond = paste0(predictors, "_i", collapse = ", ")
        lhs = glue("\\mathrm{{E}}[{response}_i \\mid {cond}]")
      } else {
        lhs = glue("\\mathrm{{E}}[{response}_i]")
      }
    }

    # ----- Layout: single line vs align* for long formulas -----
    if (length(termsTex) <= 3) {
      rhsTex = paste(termsTex, collapse = " + ")
      formulaTex = glue("$$
{lhs} = {rhsTex}
$$")
    } else {
      firstLine = glue("{lhs} &= {termsTex[1]}")
      if (length(termsTex) > 1) {
        otherLines = paste0(" &+ ", termsTex[-1], collapse = " \\\\\n")
        body = paste0(firstLine, " \\\\\n", otherLines)
      } else {
        body = firstLine
      }
      formulaTex = glue("$$
\\begin{{align*}}
{body}
\\end{{align*}}
$$")
    }

    withMathJax(HTML(formulaTex))
  })

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
  # Helper: load delimited text (csv/txt)
  # -------------------------------------------------------------------
  loadDelimited = function(sep) {
    df = tryCatch({
      read.table(
        input$file$datapath,
        sep              = sep,
        header           = TRUE,
        stringsAsFactors = FALSE,
        check.names      = TRUE,
        fill             = TRUE
      )
    }, error = function(e) {
      NULL
    })

    if (is.null(df)) {
      showNotification("Failed to read file with the chosen separator.", type = "error")
      return(NULL)
    }

    rv$data = df
    rv$allVars = names(df)
    rv$userDatasetContext = ""
    rv$researchQuestion = ""
    rv$loadedExample = NULL
    exampleLoadStatus("Choose a built-in example if you want the app to load a complete worked setup.")
    updateTextInput(session, "researchQuestion", value = "")
    resetModelPage(resetResponse = TRUE)
  }

  # -------------------------------------------------------------------
  # Load dataset when file is chosen
  # -------------------------------------------------------------------
  observeEvent(input$file, {
    req(input$data_source == "upload")
    req(input$file)

    ext = tolower(file_ext(input$file$name))

    # RDA/RData
    if (ext %in% c("rda", "rdata")) {
      e       = new.env()
      loaded  = load(input$file$datapath, envir = e)
      dfNames = loaded[sapply(loaded, function(x) {
        is.data.frame(e[[x]])
      })]

      if (length(dfNames) == 0) {
        showNotification("No data frame in RDA file.", type = "error")
        return(NULL)
      }

      df = e[[dfNames[1]]]

      rv$data = df
      rv$allVars = names(df)
      rv$userDatasetContext = ""
      rv$researchQuestion = ""
      updateTextInput(session, "researchQuestion", value = "")
      resetModelPage(resetResponse = TRUE)

      return(NULL)
    }

    # CSV or TXT
    if (ext %in% c("csv", "txt")) {

      # CSV: use comma separator directly
      if (ext == "csv") {
        loadDelimited(",")
        return(NULL)
      }

      # TXT: ask user for separator via modal
      if (ext == "txt") {
        showModal(
          modalDialog(
            title = "Choose column separator",
            radioButtons(
              "sep_input",
              "Separator:",
              choices = c(
                "Comma (,)"          = ",",
                "Tab (\\t)"          = "\t",
                "Semicolon (;)"      = ";",
                "Space ( )"          = " ",
                "Other (type below)" = "OTHER"
              )
            ),
            textInput("sep_other", "If OTHER, type separator:", value = ""),
            footer = tagList(
              modalButton("Cancel"),
              actionButton("confirm_sep", "Load file")
            )
          )
        )
        return(NULL)
      }
    }

    # Unsupported extension
    showNotification("Unsupported file type. Please upload CSV, TXT, or RDA.", type = "error")
  })

  # -------------------------------------------------------------------
  # Handle separator confirmation for TXT
  # -------------------------------------------------------------------
  observeEvent(input$confirm_sep, {
    req(input$file)

    chosen = input$sep_input
    if (identical(chosen, "OTHER")) {
      chosen = input$sep_other
    }

    if (is.null(chosen) || chosen == "") {
      showNotification("Please specify a separator.", type = "error")
      return(NULL)
    }

    removeModal()
    loadDelimited(chosen)
  })

  # -------------------------------------------------------------------
  # Load package dataset when selected
  # -------------------------------------------------------------------
  observeEvent(input$package_dataset, {
    req(input$data_source == "package")

    pkg = input$data_package %||% ""
    dsName = input$package_dataset

    if (identical(pkg, "s20x")) {
      s20xOk = tryCatch(
        {
          ensureS20xInstalled()
          TRUE
        },
        error = function(e) {
          showNotification(conditionMessage(e), type = "error")
          FALSE
        }
      )

      if (!isTRUE(s20xOk)) {
        return(NULL)
      }
    }

    if (is.null(dsName) || dsName == "") {
      return(NULL)
    }

    env = new.env()

    ok = tryCatch({
      data(list = dsName, package = pkg, envir = env)
      TRUE
    }, error = function(e) {
      FALSE
    })

    if (!ok || !exists(dsName, envir = env, inherits = FALSE)) {
      showNotification(
        paste0("Could not load dataset '", dsName, "' from package '", pkg, "'."),
        type = "error"
      )
      return(NULL)
    }

    df = env[[dsName]]

    if (!is.data.frame(df)) {
      showNotification("Selected object is not a data frame.", type = "error")
      return(NULL)
    }

    rv$data = df
    rv$allVars = names(df)
    rv$userDatasetContext = ""
    rv$researchQuestion = ""
    rv$loadedExample = NULL
    exampleLoadStatus("Choose a built-in example if you want the app to load a complete worked setup.")
    updateTextInput(session, "researchQuestion", value = "")
    resetModelPage(resetResponse = TRUE)

    # Switch to the Model tab after loading a package data set
    updateTabsetPanel(session, "main_tabs", selected = "Model")
  })

  observeEvent(input$loadExampleBtn, {
    exampleName = trimws(input$exampleName %||% "")

    if (!nzchar(exampleName)) {
      showNotification("Choose an example first.", type = "warning", duration = 6)
      return(NULL)
    }

    exampleInfo = tryCatch(
      loadExampleSpec(exampleName),
      error = function(e) {
        showNotification(conditionMessage(e), type = "error", duration = 8)
        NULL
      }
    )

    if (is.null(exampleInfo)) {
      return(NULL)
    }

    rv$data = exampleInfo$data
    rv$allVars = names(exampleInfo$data)
    rv$userDatasetContext = trimws(exampleInfo$dataContext %||% "")
    rv$researchQuestion = trimws(exampleInfo$researchQuestion %||% "")
    rv$loadedExample = utils::modifyList(
      exampleInfo,
      list(name = exampleName)
    )

    updateRadioButtons(session, "data_source", selected = "upload")
    applyLoadedExampleToInputs(exampleInfo)

    exampleLoadStatus(
      paste0(
        "Loaded example: ",
        exampleName,
        ". The data, research question, and model settings are ready on the Model tab."
      )
    )

    updateTabsetPanel(session, "main_tabs", selected = "Model")
  }, ignoreInit = TRUE)

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

  # -------------------------------------------------------------------
  # Display the R help file if using s2ox data set
  # -------------------------------------------------------------------
  datasetLoaded = reactive({
    is.data.frame(rv$data)
  })

  output$modelHelpBtnUi = renderUI({
    isReady = datasetLoaded()
    source = input$data_source %||% "upload"

    btnLabel = if (identical(source, "package")) {
      "Data description"
    } else {
      "Provide data context"
    }

    tags$button(
      id    = "modelHelpBtn",
      type  = "button",
      class = "btn btn-outline-secondary action-button",
      disabled = if (!isReady) "disabled" else NULL,
      btnLabel
    )
  })

  output$userDatasetContextUi = renderUI({

    if (!identical(input$data_source %||% "", "upload") || !datasetLoaded()) {
      return(NULL)
    }

    ctx = trimws(rv$userDatasetContext %||% "")

    if (nzchar(ctx)) {
      tags$div(
        style = "margin-top: 6px; color: #2b6a2b;",
        "Data context has been provided."
      )
    } else {
      tags$div(
        style = "margin-top: 6px; color: #666;",
        "No data context has been provided yet."
      )
    }
  })


  output$modelHelpModalBody = renderUI({

    req(is.data.frame(rv$data))

    # Always: generated variable summary
    summaryDf = buildVarSummary(rv$data)

    # Optional: s20x help HTML
    helpHtml = NULL
    pkg = input$data_package %||% ""
    if (identical(input$data_source, "package") && identical(pkg, "s20x")) {
      helpHtml = getS20xDocHtml(input$package_dataset)
    }

    tagList(
      if (identical(input$data_source, "package") && identical(pkg, "s20x")) {
        tagList(
          h4("Package help"),
          if (!is.null(helpHtml) && nzchar(helpHtml)) {
            tags$div(
              style = "max-height: 280px; overflow-y: auto; border: 1px solid #ddd; padding: 10px; border-radius: 6px;",
              htmltools::HTML(helpHtml)
            )
          } else {
            tags$div(class = "alert alert-warning", "No help page found for this dataset.")
          },
          hr()
        )
      },

      h4("Variable summary"),
      helpText("Includes factor levels and quick type/missing summaries."),
      renderVarSummaryUi(summaryDf)
    )
  })


  # -------------------------------------------------------------------
  # Model tab: Help modal (Rd text if s20x + generated summary)
  # -------------------------------------------------------------------
  observeEvent(input$modelHelpBtn, {

    if (is.null(rv$data)) {
      showNotification("Load a data set first.", type = "message")
      return(NULL)
    }

    if (identical(input$data_source %||% "", "package")) {
      titleText =
        if (!is.null(input$package_dataset) && nzchar(input$package_dataset)) {
          paste0("Data description: ", input$package_dataset)
        } else {
          "Data description"
        }

      showModal(
        modalDialog(
          title = titleText,
          size = "l",
          easyClose = TRUE,
          uiOutput("modelHelpModalBody"),
          footer = tagList(
            modalButton("Close")
          )
        )
      )

      return(NULL)
    }

    showModal(
      modalDialog(
        title = "Provide data context",
        size = "l",
        easyClose = TRUE,
        tagList(
          helpText(
            "Describe the dataset in a way that will help the model explanation. For example, explain what the variables mean, how the data were collected, and what research question you want to answer."
          ),
          tags$textarea(
            id = "userDatasetContextModal",
            class = "form-control",
            rows = 8,
            placeholder = "Example: Each row is a student. pass is 0/1. test is a score out of 20. attendance is days attended. We want to understand how test and attendance relate to passing.",
            rv$userDatasetContext %||% ""
          )
        ),
        footer = tagList(
          modalButton("Cancel"),
          actionButton("saveUserDatasetContextBtn", "Save data context", class = "btn-primary")
        )
      )
    )
  }, ignoreInit = TRUE)

  observeEvent(input$saveUserDatasetContextBtn, {
    rv$userDatasetContext = trimws(input$userDatasetContextModal %||% "")
    removeModal()

    if (nzchar(rv$userDatasetContext)) {
      showNotification("Data context saved.", type = "message", duration = 4)
    } else {
      showNotification("Data context cleared.", type = "message", duration = 4)
    }
  }, ignoreInit = TRUE)

  observeEvent(input$researchQuestion, {
    rv$researchQuestion = trimws(input$researchQuestion %||% "")
  }, ignoreInit = FALSE)


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
          title = "Treat numeric variable as factor?",
          paste0(
            "You moved '", v, "' into the Factors bucket, ",
            "but in the data it is numeric.\n\n",
            "Do you want to treat this variable as a factor in the model?"
          ),
          footer = tagList(
            actionButton("cancel_factor_numeric", "No, I'll move it back"),
            actionButton("confirm_factor_numeric", "Yes, treat as factor")
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
      showNotification("Load a data set first.", type = "message")
      return(NULL)
    }

    showModal(
      modalDialog(
        title = "Add derived variable",
        easyClose = TRUE,
        tagList(
          helpText(
            "Enter a single R expression of the form newVariable = ... . The new variable will be added to the dataset and will then be available in the response picker and variable buckets."
          ),
          textInput(
            inputId = "derivedVarTextModal",
            label = NULL,
            placeholder = "e.g. t = 1:nrow(data)    or    month = factor(rep(1:12, 12))",
            value = ""
          )
        ),
        footer = tagList(
          modalButton("Cancel"),
          actionButton("confirmAddDerivedVarBtn", "Add variable", class = "btn-success")
        )
      )
    )
  }, ignoreInit = TRUE)

  observeEvent(input$confirmAddDerivedVarBtn, {
    if (is.null(rv$data)) {
      showNotification("Load a data set first.", type = "message")
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
        model = rv$activeOllamaModel %||% "gpt-oss"
      ),
      error = function(e) {
        showNotification(
          paste(
            "Could not connect to the language model server.",
            "The model will still be fitted, but equations/explanation\n",
            "from the language model will be unavailable.\n\nDetails: ",
            conditionMessage(e)
          ),
          type     = "error",
          duration = 10
        )
        NULL
      }
    )

    if (is.null(chatProvider)) {
      showNotification(
        paste(
          "No language model is available at the moment.",
          "The model will still be fitted and deterministic equations will be shown,",
          "but no narrative explanation can be generated."
        ),
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
      showNotification(
        paste0(
          "This app only allows models with at most 3 covariates. ",
          "Your formula currently uses ", length(predNames), " predictors: ",
          paste(predNames, collapse = ", "), "."
        ),
        type = "error"
      )
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
          paste0(
            "The response variable '", respName, "' is a 2-level factor.\n",
            "For linear regression, it has been recoded to numeric.\n",
            "Coding used:  ", levs[1], " -> 0,   ", levs[2], " -> 1"
          ),
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
          showNotification(
            paste0(
              "Logistic regression requires a binary response. ",
              respName, " is a character vector with ", length(u), " distinct values."
            ),
            type = "error"
          )
          return(NULL)
        }
      }

      # ---- Case 2: Factor with exactly 2 levels ----
      if (is.factor(y)) {
        levs = levels(y)
        if (length(levs) != 2) {
          showNotification(
            paste0(
              "Logistic regression requires a factor with 2 levels. ",
              respName, " has ", length(levs), " levels."
            ),
            type = "error"
          )
          return(NULL)
        }
      }

      # ---- Case 3: Numeric 0/1 ----
      else if (is.numeric(y)) {
        uy = unique(na.omit(y))
        if (!all(uy %in% c(0, 1))) {
          showNotification(
            paste0(
              "Numeric logistic responses must be 0/1. ",
              respName, " has values: ",
              paste(head(sort(uy), 5), collapse = ", "), " ..."
            ),
            type = "error"
          )
          return(NULL)
        }
      }

      # ---- Case 4: Anything else -> reject ----
      else {
        showNotification(
          "Logistic regression requires either a binary factor, numeric 0/1, or a 2-level character vector.",
          type = "error"
        )
        return(NULL)
      }

      # ---- If we reach here: response is valid ----
      m = glm(f, data = dfMod, family = binomial(link = "logit"))

    } else if (input$model_type == "poisson") {
      if (any(na.omit(y) < 0) || any(na.omit(y) %% 1 != 0)) {
        showNotification(
          "Warning: response has negative or non-integer values. Poisson regression expects non-negative counts.",
          type = "warning"
        )
      }
      m = glm(f, data = dfMod, family = poisson(link = "log"))

    } else {
      showNotification("Unknown model type.", type = "error")
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
      explanation = cleanExplanationText(explanation)
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
  # Fitted equations / fitted means equations UI (scrollable box)
  # -------------------------------------------------------------------
  output$model_equations = renderUI({

    m = modelFit()
    # ---------------------------------------------------------------
    # If predictors are all factors, show "fitted means" equations
    # constructed from the regression coefficients
    # ---------------------------------------------------------------
    if (!is.null(m) && isFactorOnlyPredictorModel(m)) {

      info       = makeFittedMeansData(m)
      grid       = info$grid
      predictors = info$predictors
      mf         = info$mf

      makeLabel = function(oneRow) {
        if (length(predictors) == 0) {
          return("Mean")
        }
        paste0(
          "Mean(",
          paste(
            paste0(
              predictors, "=", vapply(predictors, function(v) as.character(oneRow[[v]]), "")
            ),
            collapse = ", "
          ),
          ")"
        )
      }

      eqLines = lapply(seq_len(nrow(grid)), function(i) {
        oneRow = grid[i, predictors, drop = FALSE]

        # Ensure factor columns carry the model's levels (important for model.matrix)
        for (v in predictors) {
          oneRow[[v]] = factor(oneRow[[v]], levels = levels(mf[[v]]))
        }

        makeMeanEquation(m, oneRowDf = oneRow, label = makeLabel(oneRow))
      })

      scrollStyle = "
      max-height: 300px;
      overflow-y: auto;
      overflow-x: auto;
      padding: 8px;
      border: 1px solid #ccc;
      border-radius: 6px;
      background-color: #f9f9f9;
    "

      return(div(
        style = scrollStyle,
        tagList(
          tags$p(
            tags$strong("How are the means constructed from the regression table?")
          ),
          tags$pre(
            style = "white-space: pre; margin: 0;",
            local({
              headingText = "Rounded to three significant figures for clarity"
              underline = paste(rep("-", nchar(headingText)), collapse = "")
              headingBlock = paste0(headingText, "\n", underline)

              paste(
                c(
                  headingBlock,
                  unlist(eqLines)
                ),
                collapse = "\n\n"
              )
            })
          )
        )
      ))
    }

    # ---------------------------------------------------------------
    # Otherwise, show the existing LLM-derived fitted equations
    # ---------------------------------------------------------------
    eq = rv$modelEquations
    if (is.null(eq)) {
      return(helpText("Fit a model to see the equations."))
    }

    scrollStyle = "
    max-height: 300px;
    overflow-y: auto;
    overflow-x: auto;
    padding: 8px;
    border: 1px solid #ccc;
    border-radius: 6px;
    background-color: #f9f9f9;
  "

    if (is.data.frame(eq) && all(c("condition", "equation") %in% names(eq))) {
      items = lapply(seq_len(nrow(eq)), function(i) {
        div(
          tags$p(tags$strong(eq$condition[i])),
          tags$pre(
            style = "white-space: pre; margin-top: -6px; margin-bottom: 8px;",
            eq$equation[i]
          )
        )
      })
      content = tagList(items)
    } else if (is.character(eq)) {
      content = tags$pre(
        style = "white-space: pre; margin: 0;",
        eq
      )
    } else {
      content = tags$pre(
        style = "white-space: pre; margin: 0;",
        paste(capture.output(str(eq)), collapse = "\n")
      )
    }

    div(
      style = scrollStyle,
      tagList(
        content
      )
    )
  })

  # -------------------------------------------------------------------
  # Dynamic header for fitted equations / fitted means
  # -------------------------------------------------------------------
  output$model_equations_header = renderUI({
    m = modelFit()

    if (!is.null(m) && isFactorOnlyPredictorModel(m)) {
      if (inherits(m, "glm") && identical(m$family$family, "binomial") && identical(m$family$link, "logit")) {
        h4("Fitted probabilities and odds")
      } else if (inherits(m, "glm") && identical(m$family$family, "poisson") && identical(m$family$link, "log")) {
        h4("Fitted expected counts")
      } else {
        h4("Fitted means")
      }
    } else {
      h4("Fitted equations")
    }
  })

  # -------------------------------------------------------------------
  # Fitted means UI (scrollable box)
  # -------------------------------------------------------------------
  output$fitted_means = renderUI({
    m = modelFit()
    if (is.null(m)) {
      return(helpText("Fit a model to see fitted means."))
    }

    if (!isFactorOnlyPredictorModel(m)) {
      return(helpText("Fitted means are shown when all predictors are factors."))
    }

    info = makeFittedMeansData(m)
    predictors = info$predictors
    mf = info$mf
    grid = info$grid

    layout = chooseFactorLayout(mf, predictors)

    if (layout$type == "oneWay") {
      df = grid[order(grid[[layout$rowVar]]), , drop = FALSE]
      return(tagList(
        renderOneWayTable(df, layout$rowVar, ".fit")
      ))
    }

    if (layout$type == "twoWay") {
      df = grid
      return(tagList(
        renderTwoWayTable(df, layout$rowVar, layout$colVar, ".fit")
      ))
    }

    if (layout$type == "threeWay") {
      splitVar = layout$splitVar
      splitLevels = unique(grid[[splitVar]])

      tables = lapply(splitLevels, function(s) {
        df = grid[grid[[splitVar]] == s, , drop = FALSE]
        tagList(
          tags$h5(paste0(splitVar, " = ", s)),
          renderTwoWayTable(df, layout$rowVar, layout$colVar, ".fit")
        )
      })

      return(tagList(
        tables
      ))
    }


    helpText("Fitted means table layout is only implemented for 1-3 factor predictors.")
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
            "Read the main explanation first. The sections below unpack the choices the app made so you can see why it described the model this way."
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
                renderExplanationClaimEvidenceUi(claimMap)
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
