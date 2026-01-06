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
#' @importFrom stats predict na.omit setNames
#' @importFrom utils data read.table capture.output str combn getFromNamespace head
#' @importFrom graphics plot.new text
#' @importFrom ggplot2 ggplot geom_point geom_line labs aes vars
#' @importFrom ggplot2 geom_boxplot position_jitter scale_y_continuous
#' @importFrom ggplot2 theme_minimal theme element_text
#' @importFrom rlang .data
appServer = function(input, output, session) {

  `%||%` = function(x, y) {
    if (is.null(x)) y else x
  }

  # -------------------------------------------------------------------
  # Cache s20x dataset names once per session
  # -------------------------------------------------------------------
  s20xNames = character(0)
  if (requireNamespace("s20x", quietly = TRUE)) {
    dsInfo    = utils::data(package = "s20x")
    s20xNames = dsInfo$results[, "Item"]
  }

  # Pre-populate s20x dataset dropdown once per session
  observe({
    if (length(s20xNames) == 0) {
      # s20x not installed or no datasets
      updateSelectInput(
        session,
        "s20x_dataset",
        choices  = character(0),
        selected = NULL
      )
      return(NULL)
    }

    # Add placeholder to avoid auto-selecting the first real dataset
    choices = c("Choose a data set..." = "", s20xNames)

    updateSelectInput(
      session,
      "s20x_dataset",
      choices  = choices,
      selected = ""
    )
  })

  rv = reactiveValues(
    data = NULL,
    allVars = character(0),
    autoFormula = "",
    modelEquations = NULL,
    modelExplanation = NULL,
    bucketGroupId = 0,
    lastResponse = NULL,
    lastFactors = character(0),
    pendingFactorVar = NULL
  )

  modelFit = reactiveVal(NULL)

  resetModelPage = function(resetResponse = TRUE) {

    # Clear fitted model + LLM outputs
    modelFit(NULL)
    rv$modelEquations   = NULL
    rv$modelExplanation = NULL

    # Reset tracking + factor prompt state
    rv$autoFormula       = ""
    rv$lastResponse      = NULL
    rv$lastFactors       = character(0)
    rv$pendingFactorVar  = NULL

    # Force buckets to re-render empty (Variables/Factors/Continuous)
    rv$bucketGroupId = rv$bucketGroupId + 1L

    # Reset model UI inputs
    updateRadioButtons(session, "model_type", selected = "lm")
    updateTextInput(session, "formula_text", value = "")

    # Clear any selected interactions (selectInput is created in renderUI)
    updateSelectInput(session, "interactions", selected = character(0))

    # Reset response var to first column of new data (optional)
    if (resetResponse && !is.null(rv$data) && length(rv$allVars) > 0) {
      updateSelectInput(
        session,
        "response_var",
        choices  = rv$allVars,
        selected = rv$allVars[1]
      )
    }
  }


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

    # If zero numeric predictors:
    # - If predictors are factors, show grouped plot (boxplot or jitter)
    # - Otherwise, show a message
    if (length(numericPreds) == 0) {
      if (isFactorOnlyModel(m, modelFrame)) {
        return(makeFactorOnlyPlot(
          model = m,
          data = modelFrame,
          ciType = input$plotCiType %||% "standard",
          hcType = input$plotHcType %||% "HC0"
        ))
      }

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

    # Factor predictors: used for colour / faceting (for non-binomial)
    factorMask  = sapply(modelFrame[predictors], is.factor)
    factorPreds = predictors[factorMask]

    facetVar  = NULL
    colourVar = NULL

    if (length(factorPreds) == 1) {
      # Single factor: use as grouping/shape/faceting helper
      colourVar = factorPreds[1]
    } else if (length(factorPreds) == 2) {
      # Two factors: facet by the one with fewer levels (ties -> second),
      # and use the other as grouping/shape
      levCounts = vapply(
        factorPreds,
        function(v) {
          nlevels(modelFrame[[v]])
        },
        FUN.VALUE = integer(1)
      )

      if (levCounts[1] < levCounts[2]) {
        facetVar  = factorPreds[1]
        colourVar = factorPreds[2]
      } else if (levCounts[2] < levCounts[1]) {
        facetVar  = factorPreds[2]
        colourVar = factorPreds[1]
      } else {
        # Same number of levels: facet on the second factor added
        facetVar  = factorPreds[2]
        colourVar = factorPreds[1]
      }
    }

    # --- Set up y for plotting (special handling for binomial glm) ---
    y       = modelFrame[[response]]
    isGlm   = inherits(m, "glm")
    isBinom = isGlm && identical(m$family$family, "binomial")
    yPlot   = y
    yBreaks = NULL
    yLabels = NULL

    # For binomial models, we build:
    #  - .yPlot      : numeric 0/1 on the y-axis
    #  - .respFactor : factor used for point colours
    if (isBinom) {
      respFactor = NULL

      if (is.factor(y)) {
        levs = levels(y)
        if (length(levs) == 2) {
          eventLevel = levs[2]  # glm's default "success"
          yPlot      = as.numeric(y == eventLevel)
          yBreaks    = c(0, 1)
          yLabels    = levs
          respFactor = y
        } else {
          # Fallback: use underlying codes
          yPlot      = as.numeric(y)
          respFactor = factor(y)
        }
      } else if (is.logical(y)) {
        yPlot      = as.numeric(y)
        yBreaks    = c(0, 1)
        yLabels    = c("FALSE", "TRUE")
        respFactor = factor(y, levels = c(FALSE, TRUE))
      } else if (is.numeric(y)) {
        yPlot = y
        uy    = sort(unique(stats::na.omit(yPlot)))
        if (identical(uy, c(0, 1))) {
          yBreaks    = c(0, 1)
          yLabels    = c("0", "1")
          respFactor = factor(yPlot, levels = c(0, 1), labels = yLabels)
        } else {
          respFactor = factor(yPlot)
        }
      } else {
        # character etc: coerce to factor and treat as 2-level factor if possible
        fac  = factor(y)
        levs = levels(fac)
        if (length(levs) == 2) {
          eventLevel = levs[2]
          yPlot      = as.numeric(fac == eventLevel)
          yBreaks    = c(0, 1)
          yLabels    = levs
          respFactor = fac
        } else {
          yPlot      = as.numeric(fac)
          respFactor = fac
        }
      }

      modelFrame$.yPlot      = yPlot
      modelFrame$.respFactor = respFactor
    }

    # Build grid for fitted lines
    xSeq = seq(
      min(modelFrame[[xVar]], na.rm = TRUE),
      max(modelFrame[[xVar]], na.rm = TRUE),
      length.out = 100
    )

    gridList = list()
    gridList[[xVar]] = xSeq

    # Use all factor predictors in the grid (if any)
    if (length(factorPreds) > 0) {
      for (v in factorPreds) {
        gridList[[v]] = levels(modelFrame[[v]])
      }
    }

    # For any other predictors, hold them at a typical value
    otherPreds = setdiff(predictors, c(xVar, factorPreds))
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
    if (isGlm) {
      fitVals = predict(m, newdata = newData, type = "response")
    } else {
      fitVals = predict(m, newdata = newData)
    }
    newData$fit = fitVals

    # ----- Build plot -----

    if (isBinom) {
      # -------- Binomial GLM: colour points by response --------
      if (is.null(colourVar)) {
        # No factor predictors: colours = response only
        p = ggplot(
          data    = modelFrame,
          mapping = aes(
            x      = .data[[xVar]],
            y      = .data[[".yPlot"]],
            colour = .data[[".respFactor"]]
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
          labs(
            x      = xVar,
            y      = response,
            colour = response
          )
      } else {
        # One (effective) factor predictor:
        #   - colour = response
        #   - shape  = factor
        #   - lines grouped by factor
        p = ggplot(
          data    = modelFrame,
          mapping = aes(
            x      = .data[[xVar]],
            y      = .data[[".yPlot"]],
            colour = .data[[".respFactor"]],
            shape  = .data[[colourVar]]
          )
        ) +
          geom_point(alpha = 0.6) +
          geom_line(
            data    = newData,
            mapping = aes(
              x     = .data[[xVar]],
              y     = .data[["fit"]],
              group = .data[[colourVar]]
            ),
            linewidth = 1
          ) +
          labs(
            x      = xVar,
            y      = response,
            colour = response,
            shape  = colourVar
          )
      }

      # Add faceting if we chose a facetVar
      if (!is.null(facetVar)) {
        p = p + ggplot2::facet_wrap(vars(.data[[facetVar]]))
      }

    } else {
      # -------- Non-binomial models: previous behaviour --------
      if (is.null(colourVar)) {
        # No factor predictors: no colour / facets
        p = ggplot(
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
        # Colour by chosen factor
        p = ggplot(
          data    = modelFrame,
          mapping = aes(
            x      = .data[[xVar]],
            y      = .data[[response]],
            colour = .data[[colourVar]]
          )
        ) +
          geom_point(alpha = 0.6) +
          geom_line(
            data    = newData,
            mapping = aes(
              x      = .data[[xVar]],
              y      = .data[["fit"]],
              colour = .data[[colourVar]]
            ),
            linewidth = 1
          ) +
          labs(
            x      = xVar,
            y      = response,
            colour = colourVar
          )

        if (!is.null(facetVar)) {
          p = p + ggplot2::facet_wrap(vars(.data[[facetVar]]))
        }
      }
    }

    # For binomial models, force y-scale to 0-1 with nice labels
    if (isBinom && !is.null(yBreaks) && !is.null(yLabels)) {
      p = p + ggplot2::scale_y_continuous(
        breaks = yBreaks,
        labels = yLabels,
        limits = c(0, 1)
      )
    }

    p
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

    rv$data             = df
    rv$allVars          = names(df)
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

      rv$data             = df
      rv$allVars          = names(df)
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
  # Load s20x dataset when selected
  # -------------------------------------------------------------------
  observeEvent(input$s20x_dataset, {
    req(input$data_source == "s20x")

    # Ignore the dummy "Choose a data set..." entry
    dsName = input$s20x_dataset
    if (is.null(dsName) || dsName == "") {
      return(NULL)
    }

    if (length(s20xNames) == 0) {
      showNotification(
        "The s20x package is not installed. Please install it to use the example data sets.",
        type = "error"
      )
      return(NULL)
    }

    env = new.env()
    utils::data(list = dsName, package = "s20x", envir = env)
    df = env[[dsName]]

    if (!is.data.frame(df)) {
      showNotification("Selected object is not a data frame.", type = "error")
      return(NULL)
    }

    rv$data             = df
    rv$allVars          = names(df)
    resetModelPage(resetResponse = TRUE)

    # Switch to the Model tab after loading an s20x data set
    updateTabsetPanel(session, "main_tabs", selected = "Model")
  })

  # -------------------------------------------------------------------
  # Drag-and-drop buckets UI
  # -------------------------------------------------------------------
  output$var_buckets = renderUI({
    if (is.null(rv$data)) {
      return(helpText("Load a data set to see variables."))
    }

    # Preserve current bucket contents across re-renders
    factors = input$factors %||% character(0)
    cont    = input$continuous %||% character(0)

    # Remove the chosen response AND anything already placed into buckets
    currentResp = input$response_var
    vars        = rv$allVars

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

    factors = input$factors %||% character(0)
    cont    = input$continuous %||% character(0)
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
        selected = character(0),
        multiple = TRUE,
        width    = "100%"
      )
    )
  })

  # -------------------------------------------------------------------
  # When user selects "All possible interactions", expand to all codes
  # -------------------------------------------------------------------
  observeEvent(
    input$interactions,
    {
      ints = input$interactions %||% character(0)

      if (!"__ALL_INTERACTIONS__" %in% ints) {
        return(NULL)
      }

      # Recompute the allowed predictors (same logic as interaction_ui)
      factors = input$factors %||% character(0)
      cont    = input$continuous %||% character(0)
      resp    = input$response_var

      predsAll = unique(setdiff(c(factors, cont), resp))

      # Respect the 3-covariate limit
      predsLimited = predsAll
      if (length(predsLimited) > 3) {
        predsLimited = predsLimited[1:3]
      }

      if (length(predsLimited) < 2) {
        # Nothing meaningful to select
        updateSelectInput(
          session,
          "interactions",
          selected = character(0)
        )
        return(NULL)
      }

      # Build all 2-way and 3-way combinations among predsLimited
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

      # Update the selectInput to select all actual interactions (no special value)
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
    if (is.null(rv$data)) {
      rv$lastFactors = input$factors %||% character(0)
      return(NULL)
    }

    currentFactors  = input$factors %||% character(0)
    previousFactors = rv$lastFactors %||% character(0)

    # New additions to the Factors bucket
    newVars = setdiff(currentFactors, previousFactors)

    # If nothing new, just update the record and exit
    if (length(newVars) == 0) {
      rv$lastFactors = currentFactors
      return(NULL)
    }

    v = newVars[1]  # usually just one at a time
    col = rv$data[[v]]

    # Only warn for numeric columns that aren't already factors
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

      # Don't update lastFactors yet; wait for user choice
      return(NULL)
    }

    # Non-numeric or already a factor: accept silently
    rv$lastFactors = currentFactors
  })

  # -------------------------------------------------------------------
  # Add derived variable to the data + refresh buckets
  # -------------------------------------------------------------------
  observeEvent(input$addDerivedVarBtn, {
    if (is.null(rv$data)) {
      output$derivedVarMsg = renderText("Load a data set first.")
      return()
    }

    res = addDerivedVariableToData(rv$data, input$derivedVarText)
    output$derivedVarMsg = renderText(res$msg)

    if (isTRUE(res$ok)) {
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

      updateTextInput(session, "derivedVarText", value = "")
    }
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
  # Auto-populate formula from buckets + optional interactions
  # (limit to 3 predictors; expert mode for compact notation)
  # -------------------------------------------------------------------
  observeEvent(
    list(
      input$response_var,
      input$factors,
      input$continuous,
      input$interactions,
      input$expert_mode
    ),
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
      ints    = input$interactions %||% character(0)

      # Main effects: all predictors from both buckets, excluding response
      predsAll = unique(setdiff(c(factors, cont), resp))

      # Hard limit: only first 3 predictors
      preds = predsAll
      if (length(preds) > 3) {
        preds = preds[1:3]
      }

      # If nothing selected: intercept-only model
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

      # Restrict interactions to those only involving the allowed predictors
      ints = Filter(
        function(z) {
          all(strsplit(z, ":", fixed = TRUE)[[1]] %in% preds)
        },
        ints
      )

      # ---------------- Expert mode shorthand (optional) ----------------
      expertRhs = NULL
      expertOn  = isTRUE(input$expert_mode)

      if (expertOn) {
        # Case 1: exactly two predictors with a single 2-way interaction -> "A * B"
        if (length(preds) == 2 && length(ints) == 1) {
          varsInt = strsplit(ints[1], ":", fixed = TRUE)[[1]]
          if (length(varsInt) == 2 && setequal(varsInt, preds)) {
            expertRhs = paste(preds, collapse = " * ")
          }
        }

        # Case 2: k >= 2 predictors, all pairwise interactions selected (no 3-way)
        if (is.null(expertRhs) && length(preds) >= 2 && length(ints) > 0) {
          lenInts = vapply(
            strsplit(ints, ":", fixed = TRUE),
            length,
            FUN.VALUE = integer(1)
          )
          pairInts   = ints[lenInts == 2]
          higherInts = ints[lenInts > 2]

          # Only consider compact "(A + B + C)^2" when there are no higher-order terms
          if (length(higherInts) == 0) {
            # All possible 2-way pairs among 'preds'
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

      # ---------------- Default explicit RHS (always colon notation) ----------------
      if (!is.null(expertRhs)) {
        rhs = expertRhs
      } else {
        rhsPieces = character(0)

        # Main effects explicitly
        mainPart = paste(preds, collapse = " + ")
        rhsPieces = c(rhsPieces, mainPart)

        # Interactions: keep ":" notation to preserve exact semantics
        if (length(ints) > 0) {
          intPart = paste(ints, collapse = " + ")
          rhsPieces = c(rhsPieces, intPart)
        }

        rhs = paste(rhsPieces, collapse = " + ")
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
  # User response to numeric-as-factor confirmation
  # -------------------------------------------------------------------
  observeEvent(input$confirm_factor_numeric, {
    req(rv$pendingFactorVar)
    removeModal()

    rv$lastFactors      = input$factors %||% character(0)
    v                   = rv$pendingFactorVar
    rv$pendingFactorVar = NULL

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
      getChatProvider(),
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
        "No language model is available at the moment. The model is still fitted, but no equations/explanation can be generated.",
        type     = "error",
        duration = 10
      )
    }

    res = checkFormula()
    if (!res$ok) {
      showNotification(res$msg, type = "error")
      return(NULL)
    }

    f        = as.formula(input$formula_text)
    respName = all.vars(f)[1]

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
    factorVars = input$factors %||% character(0)
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
      y = rv$data[[respName]]

      # ---- Case 1: Character with exactly 2 values -> silently convert to factor ----
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

      # ---- Case 4: Anything else -> reject ----
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
      dsName  = input$s20x_dataset
      docText = getS20xDocText(dsName)
      if (!is.null(docText)) {
        attr(m, "wmfm_dataset_doc")  = docText
        attr(m, "wmfm_dataset_name") = dsName
      }
    }

    modelFit(m)

    # If there is no chat provider, skip the LLM bits entirely
    if (is.null(chatProvider)) {
      rv$modelEquations   = NULL
      rv$modelExplanation = NULL

      # Still switch to the fitted model tab to show summary/plot
      updateTabsetPanel(session, "main_tabs", selected = "Fitted Model")
      return(NULL)
    }

    # Talk to the LLM with a progress bar
    withProgress(message = "Talking to the language model...", value = 0, {
      incProgress(0.3, detail = "Deriving equations...")

      eq = tryCatch(
        lmEquations(m, chatProvider),
        error = function(e) {
          showNotification(
            paste(
              "The language model request for equations failed.",
              "You can still use the fitted model and plots.",
              "\nDetails:", conditionMessage(e)
            ),
            type     = "error",
            duration = 10
          )
          return(NULL)
        }
      )

      # If equation generation failed, don't try for an explanation
      if (is.null(eq)) {
        rv$modelEquations   = NULL
        rv$modelExplanation = NULL
        return(NULL)
      }

      incProgress(0.7, detail = "Writing explanation...")

      expl = tryCatch(
        lmExplanation(m, chatProvider),
        error = function(e) {
          showNotification(
            paste(
              "The language model request for the explanation failed.",
              "Equations are available; explanation is omitted.",
              "\nDetails:", conditionMessage(e)
            ),
            type     = "error",
            duration = 10
          )
          return(NULL)
        }
      )

      # It's fine if expl is NULL - we just skip showing an explanation
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
              headingBlock = paste0(headingText, "
", underline)

              paste(
                c(
                  headingBlock,
                  unlist(eqLines)
                ),
                collapse = "

"
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

    div(style = scrollStyle, content)
  })

  # -------------------------------------------------------------------
  # Dynamic header for fitted equations / fitted means
  # -------------------------------------------------------------------
  output$model_equations_header = renderUI({
    m = modelFit()

    if (!is.null(m) && isFactorOnlyPredictorModel(m)) {
      if (inherits(m, "glm") && identical(m$family$family, "binomial") && identical(m$family$link, "logit")) {
        h4("Fitted probabilities")
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
