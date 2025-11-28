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
#' @importFrom shiny renderPrint observeEvent observe req showNotification withProgress
#' @importFrom shiny incProgress helpText updateRadioButtons updateTextInput
#' @importFrom shiny updateSelectInput showModal removeModal modalDialog
#' @importFrom shiny radioButtons textInput modalButton actionButton
#' @importFrom shiny updateTabsetPanel tagList selectInput div tags
#' @importFrom sortable bucket_list add_rank_list
#' @importFrom tools file_ext
#' @importFrom stats as.formula lm glm binomial poisson model.frame terms
#' @importFrom stats predict na.omit
#' @importFrom utils data read.table capture.output str
#' @importFrom graphics plot.new text
#' @importFrom ggplot2 ggplot geom_point geom_line labs aes
#' @importFrom rlang .data
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
  # Populate the s20x dataset chooser (if s20x is available)
  # -------------------------------------------------------------------
  observe({
    if (requireNamespace("s20x", quietly = TRUE)) {
      dsInfo  = utils::data(package = "s20x")
      dsNames = dsInfo$results[, "Item"]
    } else {
      dsNames = character(0)
    }

    updateSelectInput(
      session,
      "s20x_dataset",
      choices  = dsNames,
      selected = if (length(dsNames) > 0) dsNames[1] else NULL
    )
  })

  # -------------------------------------------------------------------
  # Plot of data + fitted model
  # -------------------------------------------------------------------
  output$model_plot = renderPlot({
    m = modelFit()
    req(m)  # need a fitted model

    modelFrame = model.frame(m)
    response   = names(modelFrame)[1]
    predictors = names(modelFrame)[-1]

    # Identify numeric predictors
    numericMask  = sapply(modelFrame[predictors], is.numeric)
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
    factorMask  = sapply(modelFrame[predictors], is.factor)
    factorPreds = predictors[factorMask]
    fVar        = if (length(factorPreds) > 0) factorPreds[1] else NULL

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
      ggplot(
        data    = modelFrame,
        mapping = aes(
          x = .data[[xVar]],
          y = .data[[response]]
        )
      ) +
        geom_point(alpha = 0.6) +
        geom_line(
          data    = newData,
          mapping = aes(
            x = .data[[xVar]],
            y = .data[["fit"]]
          ),
          linewidth = 1
        ) +
        labs(x = xVar, y = response)
    } else {
      ggplot(
        data    = modelFrame,
        mapping = aes(
          x      = .data[[xVar]],
          y      = .data[[response]],
          colour = .data[[fVar]]
        )
      ) +
        geom_point(alpha = 0.6) +
        geom_line(
          data    = newData,
          mapping = aes(
            x      = .data[[xVar]],
            y      = .data[["fit"]],
            colour = .data[[fVar]]
          ),
          linewidth = 1
        ) +
        labs(x = xVar, y = response, colour = fVar)
    }
  })

  # -------------------------------------------------------------------
  # Symbolic model formula (LaTeX via MathJax)
  # -------------------------------------------------------------------
  # ---- Symbolic model formula (LaTeX via MathJax), with interactions ----
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
      if (length(vars) != 2) {
        # For now, skip higher-order interactions
        next
      }
      v1 = vars[1]
      v2 = vars[2]
      x1 = mf[[v1]]
      x2 = mf[[v2]]

      isFac1 = is.factor(x1)
      isFac2 = is.factor(x2)

      # numeric × numeric
      if (!isFac1 && !isFac2) {
        termsTex = c(
          termsTex,
          glue("\\beta_{betaIdx} \\times {v1}_i \\times {v2}_i")
        )
        betaIdx = betaIdx + 1L

        # factor × numeric
      } else if (isFac1 && !isFac2) {
        lvls1 = levels(x1)
        for (lvl1 in lvls1[-1]) {
          termsTex = c(
            termsTex,
            glue("\\beta_{betaIdx} \\times {v2}_i \\times \\mathbf{{1}}\\{{ {v1}_i = \\text{{\"{lvl1}\"}} \\}}")
          )
          betaIdx = betaIdx + 1L
        }

        # numeric × factor
      } else if (!isFac1 && isFac2) {
        lvls2 = levels(x2)
        for (lvl2 in lvls2[-1]) {
          termsTex = c(
            termsTex,
            glue("\\beta_{betaIdx} \\times {v1}_i \\times \\mathbf{{1}}\\{{ {v2}_i = \\text{{\"{lvl2}\"}} \\}}")
          )
          betaIdx = betaIdx + 1L
        }

        # factor × factor
      } else {
        lvls1 = levels(x1)
        lvls2 = levels(x2)
        for (lvl1 in lvls1[-1]) {
          for (lvl2 in lvls2[-1]) {
            termsTex = c(
              termsTex,
              glue("\\beta_{betaIdx} \\times \\mathbf{{1}}\\{{ {v1}_i = \\text{{\"{lvl1}\"}}, {v2}_i = \\text{{\"{lvl2}\"}} \\}}")
            )
            betaIdx = betaIdx + 1L
          }
        }
      }
    }

    # ----- LHS: depends on model type -----
    predictors = mainLabels       # for conditioning in E[· | ·]
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
        sep           = sep,
        header        = TRUE,
        stringsAsFactors = FALSE,
        check.names   = TRUE,
        fill          = TRUE
      )
    }, error = function(e) {
      NULL
    })

    if (is.null(df)) {
      showNotification("Failed to read file with the chosen separator.", type = "error")
      return(NULL)
    }

    rv$data           = df
    rv$allVars        = names(df)
    rv$autoFormula    = ""
    modelFit(NULL)
    rv$modelEquations   = NULL
    rv$modelExplanation = NULL
    updateTextInput(session, "formula_text", value = "")
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

      rv$data            = df
      rv$allVars         = names(df)
      rv$autoFormula     = ""
      modelFit(NULL)
      rv$modelEquations  = NULL
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
                "Comma (,)"   = ",",
                "Tab (\\t)"   = "\t",
                "Semicolon (;)" = ";",
                "Space ( )"   = " ",
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
  # Load s20x dataset when selected
  # -------------------------------------------------------------------
  observeEvent(input$s20x_dataset, {
    req(input$data_source == "s20x")

    if (!requireNamespace("s20x", quietly = TRUE)) {
      showNotification(
        "The s20x package is not installed. Please install it to use the example data sets.",
        type = "error"
      )
      return(NULL)
    }

    dsName = input$s20x_dataset
    if (is.null(dsName) || dsName == "") {
      return(NULL)
    }

    env = new.env()
    utils::data(list = dsName, package = "s20x", envir = env)
    df = env[[dsName]]

    if (!is.data.frame(df)) {
      showNotification("Selected object is not a data frame.", type = "error")
      return(NULL)
    }

    rv$data = df
    rv$allVars = names(df)
    rv$autoFormula = ""
    modelFit(NULL)
    rv$modelEquations = NULL
    rv$modelExplanation = NULL
    updateTextInput(session, "formula_text", value = "")

    # >>> NEW: switch to the Model tab after loading an s20x data set
    updateTabsetPanel(session, "main_tabs", selected = "Model")
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
    vars        = rv$allVars

    if (!is.null(currentResp) && nzchar(currentResp)) {
      vars = setdiff(vars, currentResp)
    }

    bucket_list(
      header     = NULL,
      group_name = paste0("vars_group_", rv$bucketGroupId),
      orientation = "horizontal",
      add_rank_list(
        text   = "Variables",
        labels = vars,
        input_id = "variables"
      ),
      add_rank_list(
        text   = "Factors",
        labels = character(0),
        input_id = "factors"
      ),
      add_rank_list(
        text   = "Continuous",
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
      cont    = input$continuous %||% character(0)

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
    newResp        = input$response_var
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

    f        = as.formula(input$formula_text)
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

      # Extract response
      respName = all.vars(f)[1]
      y = rv$data[[respName]]

      # ---- Case 1: Character with exactly 2 values → silently convert to factor ----
      if (is.character(y)) {
        u = unique(na.omit(y))
        if (length(u) == 2) {
          rv$data[[respName]] = factor(y)   # silent conversion
          y = rv$data[[respName]]           # refresh local copy
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

      # ---- Case 4: Anything else → reject ----
      else {
        showNotification(
          "Logistic regression requires either a binary factor, numeric 0/1, or a 2-level character vector.",
          type = "error"
        )
        return(NULL)
      }

      # ---- If we reach here: response is valid ----
      m = glm(f, data = rv$data, family = binomial(link = "logit"))
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

    # If this data came from s20x, attach its documentation to the model
    if (identical(input$data_source, "s20x")) {
      dsName = input$s20x_dataset
      docText = getS20xDocText(dsName)
      if (!is.null(docText)) {
        attr(m, "wmfm_dataset_doc") = docText
        attr(m, "wmfm_dataset_name") = dsName
      }
    }

    modelFit(m)

    # Talk to the LLM with a progress bar
    withProgress(message = "Talking to the language model...", value = 0, {
      incProgress(0.3, detail = "Deriving equations...")
      eq = lmEquations(m, chatProvider)

      incProgress(0.7, detail = "Writing explanation...")
      expl = lmExplanation(m, chatProvider)

      rv$modelEquations   = eq
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
    rv$modelEquations   = NULL
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
        label   = NULL,
        choices = rv$allVars,
        selected = rv$allVars[1]
      )
    }

    rv$lastResponse  = NULL
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
