#' Register contrast observers for the app server
#'
#' Registers the Shiny outputs and observers that build and compute
#' factor-only model contrasts. This keeps the top-level app server focused
#' on orchestration while preserving the existing contrast behaviour.
#'
#' @param input Shiny input object.
#' @param output Shiny output object.
#' @param session Shiny session object.
#' @param rv Main app reactive values object.
#' @param modelFit Reactive value containing the current fitted model.
#' @param contrastPairs Reactive value containing selected pairwise contrasts.
#' @param contrastResultText Reactive value containing rendered contrast output text.
#' @param setBucketState Function for synchronising factor and continuous bucket state.
#'
#' @return Invisibly returns NULL.
#'
#' @keywords internal
registerContrastObservers = function(input, output, session, rv, modelFit, contrastPairs, contrastResultText, setBucketState) {
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
      showNotification(buildSameContrastLevelsMessage(), type = "warning")
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
      showNotification(buildDuplicateContrastPairMessage(), type = "message")
      return(NULL)
    }
  
    contrastPairs(c(current, newLabel))
  })
  
  observeEvent(input$removeContrastBtn, {
    current = contrastPairs()
    sel = input$contrastList %||% character(0)
  
    if (length(sel) == 0) {
      showNotification(buildNoContrastSelectionMessage(), type = "message")
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
            " (95% confidence interval: ", fmt3(res$lowerEta), ", ", fmt3(res$upperEta), ")"
          )
        )
      )
    }
  
    if (!is.null(res$interpreted)) {
  
      # Default: show what computeFactorOnlyContrast gave us
      baseLine = paste0(
        res$interpreted$label, ": ", fmt3(res$interpreted$estimate),
        " (95% confidence interval: ", fmt3(res$interpreted$lower), ", ", fmt3(res$interpreted$upper), ")"
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
          " (95% confidence interval: ", fmt3(ratioLo), ", ", fmt3(ratioHi), ")"
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
          " (95% confidence interval: ",
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
                "Note: Because the 95% confidence interval includes ", nullLabel,
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
      showNotification(buildNoContrastPairsMessage(), type = "warning")
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
        showNotification(buildAverageContrastEmptyGroupMessage(), type = "warning")
        return(NULL)
      }
  
      if (any(!(leftLevels %in% levs)) || any(!(rightLevels %in% levs))) {
        showNotification(buildAverageContrastInvalidLevelMessage(), type = "warning")
        return(NULL)
      }
  
      if (length(intersect(leftLevels, rightLevels)) > 0) {
        showNotification(buildAverageContrastOverlappingLevelMessage(), type = "warning")
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
        showNotification(buildInvalidCustomContrastWeightMessage(), type = "warning")
        return(NULL)
      }
  
      if (abs(sum(w)) > 1e-8) {
        showNotification(buildCustomContrastWeightsMustSumToZeroMessage(), type = "warning")
        return(NULL)
      }
      if (sum(w != 0) < 2) {
        showNotification(buildCustomContrastTooFewWeightsMessage(), type = "warning")
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
  
  

  invisible(NULL)
}
