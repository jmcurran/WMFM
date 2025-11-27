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
#' @importFrom shiny reactiveValues reactiveVal renderPlot renderUI renderText
#' @importFrom shiny renderPrint observeEvent req showNotification withProgress
#' @importFrom shiny incProgress helpText updateRadioButtons updateTextInput
#' @importFrom shiny updateSelectInput showModal removeModal modalDialog
#' @importFrom shiny radioButtons textInput modalButton actionButton
#' @importFrom shiny updateTabsetPanel tagList selectInput div
#' @importFrom sortable bucket_list add_rank_list
#' @importFrom tools file_ext
#' @importFrom stats as.formula lm glm binomial poisson model.frame terms
#' @importFrom stats predict na.omit
#' @importFrom utils read.table capture.output str
#' @importFrom graphics plot.new text
appServer = function(input, output, session) {
  `%||%` = function(x, y) {
    if (is.null(x)) y else x
  }

  chatProvider = getChatProvider()

  rv = reactiveValues(
    data = NULL,
    allVars = character(0),
    autoFormula = "",
    modelEquations = NULL,
    modelExplanation = NULL,
    bucketGroupId = 0,
    lastResponse = NULL
  )

  modelFit = reactiveVal(NULL)

  # -------------------------------------------------------------------
  # Plot of data + fitted model
  # -------------------------------------------------------------------
  output$model_plot = renderPlot({
    m = modelFit()
    req(m)  # need a fitted model

    modelFrame = model.frame(m)
    response = names(modelFrame)[1]
    predictors = names(modelFrame)[-1]

    # Identify numeric predictors
    numericMask = sapply(modelFrame[predictors], is.numeric)
    numericPreds = predictors[numericMask]

    # If zero numeric predictors: nothing to plot
    if (length(numericPreds) == 0) {
      plot.new()
      text(
        0.5, 0.5,
        "No numeric predictors available to plot.",
        cex = 1.2
      )
      return(invisible())
    }

    # If more than one numeric predictor: do not plot, show message
    if (length(numericPreds) > 1) {
      plot.new()
      text(
        0.5, 0.5,
        "Plot is only available when there is a single numeric predictor in the model.",
        cex = 1.2
      )
      return(invisible())
    }

    xVar = numericPreds[1]

    # Optionally pick one factor for grouping
    factorMask = sapply(modelFrame[predictors], is.factor)
    factorPreds = predictors[factorMask]
    fVar = if (length(factorPreds) > 0) factorPreds[1] else NULL

    # Build grid for fitted lines
    xSeq = seq(
      min(modelFrame[[xVar]], na.rm = TRUE),
      max(modelFrame[[xVar]], na.rm = TRUE),
      length.out = 100
    )

    gridList = list()
    gridList[[xVar]] = xSeq

    if (!is.null(fVar)) {
      gridList[[fVar]] = levels(modelFrame[[fVar]])
    }

    # For any other predictors, hold them at a typical value
    otherPreds = setdiff(predictors, c(xVar, fVar))
    for (v in otherPreds) {
      x = modelFrame[[v]]
      if (is.numeric(x)) {
        gridList[[v]] = mean(x, na.rm = TRUE)
      } else if (is.factor(x)) {
        gridList[[v]] = levels(x)[1]
      }
    }

    newData = expand.grid(gridList, stringsAsFactors = FALSE)

    # Predictions on response scale
    if (inherits(m, "glm")) {
      fitVals = predict(m, newdata = newData, type = "response")
    } else {
      fitVals = predict(m, newdata = newData)
    }
    newData$fit = fitVals

    # Build plot
    if (is.null(fVar)) {
      ggplot2::ggplot() +
        ggplot2::geom_point(
          data = modelFrame,
          ggplot2::aes_string(x = xVar, y = response),
          alpha = 0.6
        ) +
        ggplot2::geom_line(
          data = newData,
          ggplot2::aes_string(x = xVar, y = "fit"),
          linewidth = 1
        ) +
        ggplot2::labs(x = xVar, y = response)
    } else {
      ggplot2::ggplot() +
        ggplot2::geom_point(
          data = modelFrame,
          ggplot2::aes_string(x = xVar, y = response, colour = fVar),
          alpha = 0.6
        ) +
        ggplot2::geom_line(
          data = newData,
          ggplot2::aes_string(x = xVar, y = "fit", colour = fVar),
          linewidth = 1
        ) +
        ggplot2::labs(x = xVar, y = response, colour = fVar)
    }
  })

  # -------------------------------------------------------------------
  # Symbolic model formula (LaTeX via MathJax)
  # -------------------------------------------------------------------
  output$model_formula = renderUI({
    m = modelFit()
    if (is.null(m)) {
      return(helpText("Fit a model to see the model formula."))
    }

    modelFrame = model.frame(m)
    response = names(modelFrame)[1]
    predictors = names(modelFrame)[-1]

    termsObj = terms(m)
    termLabels = attr(termsObj, "term.labels")
    dataClasses = attr(termsObj, "dataClasses")

    termsTex = c("\\beta_0")
    betaIndex = 1L

    # Main effects
    mainLabels = termLabels[!grepl(":", termLabels, fixed = TRUE)]
    for (lab in mainLabels) {
      v = lab
      cls = dataClasses[[v]]
      x = modelFrame[[v]]
      isCat = !is.null(cls) && cls %in% c("factor", "ordered", "character", "logical")

      if (isCat) {
        if (!is.factor(x)) {
          x = as.factor(x)
        }
        levelsX = levels(x)
        if (length(levelsX) >= 2) {
          for (lvl in levelsX[-1]) {
            termsTex = c(
              termsTex,
              glue::glue(
                "\\beta_{betaIndex} \\times \\mathbf{{1}}\\{{ {v}_i = \\text{{\"{lvl}\"}} \\}}"
              )
            )
            betaIndex = betaIndex + 1L
          }
        }
      } else {
        termsTex = c(
          termsTex,
          glue::glue("\\beta_{betaIndex} \\times {v}_i")
        )
        betaIndex = betaIndex + 1L
      }
    }

    # Interaction terms (2-way only)
    interactionLabels = termLabels[grepl(":", termLabels, fixed = TRUE)]

    for (lab in interactionLabels) {
      vars = strsplit(lab, ":", fixed = TRUE)[[1]]
      if (length(vars) != 2) {
        next
      }

      v1 = vars[1]
      v2 = vars[2]
      x1 = modelFrame[[v1]]
      x2 = modelFrame[[v2]]
      cls1 = dataClasses[[v1]]
      cls2 = dataClasses[[v2]]
      isCat1 = !is.null(cls1) && cls1 %in% c("factor", "ordered", "character", "logical")
      isCat2 = !is.null(cls2) && cls2 %in% c("factor", "ordered", "character", "logical")

      # numeric:numeric
      if (!isCat1 && !isCat2) {
        termsTex = c(
          termsTex,
          glue::glue("\\beta_{betaIndex} \\times {v1}_i \\times {v2}_i")
        )
        betaIndex = betaIndex + 1L
        next
      }

      # factor:numeric
      if (isCat1 && !isCat2) {
        facVar = v1
        numVar = v2
        facX = if (is.factor(x1)) x1 else as.factor(x1)
      } else if (!isCat1 && isCat2) {
        facVar = v2
        numVar = v1
        facX = if (is.factor(x2)) x2 else as.factor(x2)
      } else {
        # factor:factor omitted for now
        next
      }

      levelsX = levels(facX)
      if (length(levelsX) >= 2) {
        for (lvl in levelsX[-1]) {
          termsTex = c(
            termsTex,
            glue::glue(
              "\\beta_{betaIndex} \\times {numVar}_i \\times \\mathbf{{1}}\\{{ {facVar}_i = \\text{{\"{lvl}\"}} \\}}"
            )
          )
          betaIndex = betaIndex + 1L
        }
      }
    }

    rhs = paste(termsTex, collapse = " + ")

    # LHS
    if (inherits(m, "glm")) {
      fam = m$family$family
      link = m$family$link

      if (fam == "binomial" && link == "logit") {
        lhs = "\\operatorname{logit}(p_i)"
      } else if (fam == "poisson" && link == "log") {
        lhs = "\\log(\\mu_i)"
      } else {
        if (length(predictors) > 0) {
          cond = paste0(predictors, "_i", collapse = ", ")
          lhs = glue::glue("\\mathrm{{E}}[{response}_i \\mid {cond}]")
        } else {
          lhs = glue::glue("\\mathrm{{E}}[{response}_i]")
        }
      }
    } else {
      if (length(predictors) > 0) {
        cond = paste0(predictors, "_i", collapse = ", ")
        lhs = glue::glue("\\mathrm{{E}}[{response}_i \\mid {cond}]")
      } else {
        lhs = glue::glue("\\mathrm{{E}}[{response}_i]")
      }
    }

    formulaTex = glue::glue("$$
{lhs} = {rhs}
$$")

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
        ok = FALSE,
        msg = "Enter a model formula, e.g.  y ~ x1 + x2"
      ))
    }

    # basic character whitelist (allows interactions via * and :)
    if (!grepl("^[~+*:0-9A-Za-z_(). -]+$", txt)) {
      return(list(ok = FALSE, msg = "Formula contains illegal characters."))
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
    vars = all.vars(f)
    missing = setdiff(vars, names(rv$data))

    if (length(missing) > 0) {
      return(list(
        ok = FALSE,
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
        sep = sep,
        header = TRUE,
        stringsAsFactors = FALSE,
        check.names = TRUE,
        fill = TRUE
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
    rv$autoFormula = ""
    modelFit(NULL)
    rv$modelEquations = NULL
    rv$modelExplanation = NULL
    updateTextInput(session, "formula_text", value = "")
  }

  # -------------------------------------------------------------------
  # Load dataset when file is chosen
  # -------------------------------------------------------------------
  observeEvent(input$file, {
    req(input$file)

    ext = tolower(file_ext(input$file$name))

    # RDA/RData
    if (ext %in% c("rda", "rdata")) {
      e = new.env()
      loaded = load(input$file$datapath, envir = e)
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
      rv$autoFormula = ""
      modelFit(NULL)
      rv$modelEquations = NULL
      rv$modelExplanation = NULL
      updateTextInput(session, "formula_text", value = "")
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
                "Comma (,)" = ",",
                "Tab (\\t)" = "\t",
                "Semicolon (;)" = ";",
                "Space ( )" = " ",
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
  # Drag-and-drop buckets UI
  # -------------------------------------------------------------------
  output$var_buckets = renderUI({
    if (is.null(rv$data)) {
      return(helpText("Load a data set to see variables."))
    }

    # Remove the chosen response from the Variables list
    currentResp = input$response_var
    vars = rv$allVars

    if (!is.null(currentResp) && nzchar(currentResp)) {
      vars = setdiff(vars, currentResp)
    }

    bucket_list(
      header = NULL,
      group_name = paste0("vars_group_", rv$bucketGroupId),
      orientation = "horizontal",
      add_rank_list(
        text = "Variables",
        labels = vars,
        input_id = "variables"
      ),
      add_rank_list(
        text = "Factors",
        labels = character(0),
        input_id = "factors"
      ),
      add_rank_list(
        text = "Continuous",
        labels = character(0),
        input_id = "continuous"
      )
    )
  })

  # -------------------------------------------------------------------
  # Response picker
  # -------------------------------------------------------------------
  output$response_picker = renderUI({
    if (is.null(rv$data)) {
      return(NULL)
    }
    selectInput("response_var", "", choices = rv$allVars)
  })

  # -------------------------------------------------------------------
  # Auto-populate formula from buckets (additive model)
  # -------------------------------------------------------------------
  observeEvent(
    list(input$response_var, input$factors, input$continuous),
    {
      if (is.null(rv$data)) {
        return(NULL)
      }

      resp = input$response_var
      if (is.null(resp) || resp == "") {
        return(NULL)
      }

      factors = input$factors %||% character(0)
      cont = input$continuous %||% character(0)

      preds = c(factors, cont)
      preds = unique(setdiff(preds, resp))

      rhs = if (length(preds) == 0) {
        "1"
      } else {
        paste(preds, collapse = " + ")
      }

      newAuto = paste(resp, "~", rhs)
      current = trimws(input$formula_text)

      # Only overwrite if user hasn't customised the formula
      if (current == "" || current == rv$autoFormula) {
        rv$autoFormula = newAuto
        updateTextInput(session, "formula_text", value = newAuto)
      } else {
        rv$autoFormula = newAuto
      }
    }
  )

  # -------------------------------------------------------------------
  # Keep 'Variables' bucket in sync with chosen response (if needed)
  # -------------------------------------------------------------------
  observeEvent(input$response_var, {
    newResp = input$response_var
    oldResp = rv$lastResponse

    # We don't currently store separate varsBucket/factorsBucket/contBucket
    # in rv, so this block is effectively just tracking lastResponse.
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
  # Fit model when button clicked
  # -------------------------------------------------------------------
  observeEvent(input$fit_btn, {
    res = checkFormula()
    if (!res$ok) {
      showNotification(res$msg, type = "error")
      return(NULL)
    }

    f = as.formula(input$formula_text)
    respName = all.vars(f)[1]

    # Work on a copy of the data so we can safely coerce factors
    dfMod = rv$data

    # Anything in the Factors bucket should be treated as a factor
    factorVars = input$factors %||% character(0)
    for (v in factorVars) {
      if (!is.null(dfMod[[v]]) && !is.factor(dfMod[[v]])) {
        dfMod[[v]] = factor(dfMod[[v]])
      }
    }

    y = dfMod[[respName]]

    # Fit the chosen model type
    if (input$model_type == "lm") {
      m = lm(f, data = dfMod)
    } else if (input$model_type == "logistic") {
      if (!all(na.omit(y) %in% c(0, 1))) {
        showNotification(
          "Warning: response is not 0/1. Logistic regression expects a binary outcome.",
          type = "warning"
        )
      }
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

    modelFit(m)

    # Talk to the LLM with a progress bar
    withProgress(message = "Talking to the language model...", value = 0, {
      incProgress(0.3, detail = "Deriving equations...")
      eq = lmEquations(m, chatProvider)

      incProgress(0.7, detail = "Writing explanation...")
      expl = lmExplanation(m, chatProvider)

      rv$modelEquations = eq
      rv$modelExplanation = expl
      incProgress(1)
    })

    # After fitting and LLM completion, switch to the "Fitted Model" tab
    updateTabsetPanel(session, "main_tabs", selected = "Fitted Model")
  })

  # -------------------------------------------------------------------
  # Reset model
  # -------------------------------------------------------------------
  observeEvent(input$reset_btn, {
    # Clear fitted model
    modelFit(NULL)

    # Clear stored equations / explanation
    rv$modelEquations = NULL
    rv$modelExplanation = NULL

    # Reset auto-formula tracking
    rv$autoFormula = ""

    # Reset model type
    updateRadioButtons(session, "model_type", selected = "lm")

    # Reset formula input
    updateTextInput(session, "formula_text", value = "")

    # Reset the response selector to the first variable (like on initial load)
    if (!is.null(rv$data) && length(rv$allVars) > 0) {
      updateSelectInput(
        session,
        "response_var",
        label = NULL,
        choices = rv$allVars,
        selected = rv$allVars[1]
      )
    }

    rv$lastResponse = NULL
    rv$bucketGroupId = rv$bucketGroupId + 1L  # force new buckets
  })

  # -------------------------------------------------------------------
  # Fitted equations UI (scrollable box)
  # -------------------------------------------------------------------
  output$model_equations = renderUI({
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

    div(style = scrollStyle, content)
  })

  # -------------------------------------------------------------------
  # Model explanation
  # -------------------------------------------------------------------
  output$model_explanation = renderUI({
    expl = rv$modelExplanation
    if (is.null(expl)) {
      return(helpText("Fit a model to see a textual explanation."))
    }
    tags$pre(
      style = "white-space: pre-wrap; word-wrap: break-word;",
      expl
    )
  })

  # -------------------------------------------------------------------
  # Display model summary (regression table)
  # -------------------------------------------------------------------
  output$model_output = renderPrint({
    m = modelFit()
    if (is.null(m)) {
      cat("No model fitted yet.")
    } else {
      summary(m)
    }
  })
}
