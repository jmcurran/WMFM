#' Register model plot observers
#'
#' Wires the original fitted-model plot and the student-facing Model plots tab.
#'
#' @param input Shiny input object.
#' @param output Shiny output object.
#' @param modelFit Reactive value containing the fitted model.
#'
#' @return No return value; called for its side effects.
#'
#' @keywords internal
#'
#' @importFrom shiny checkboxInput div downloadHandler helpText renderPlot renderUI req selectInput tags tagList validate need
#' @importFrom ggplot2 ggsave
registerModelPlotObservers = function(input, output, modelFit) {
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

    mode = if (identical(numPredCount, 1L)) {
      "continuous"
    } else {
      "factorOnly"
    }

    plotCiControlsUi(mode = mode)
  })

  output$model_plot = renderPlot({
    m = modelFit()
    req(m)

    res = drawModelPlot(
      model = m,
      ciType = input$plotCiType %||% "standard",
      hcType = input$plotHcType %||% "HC0",
      showCi = isTRUE(input$plotShowCi %||% FALSE),
      level = input$plotCiLevel %||% 0.95
    )

    if (inherits(res, "ggplot")) {
      print(res)
    } else if (inherits(res, "recordedplot")) {
      replayPlot(res)
    }

    invisible(NULL)
  })

  output$modelPlotTypeUi = renderUI({
    m = modelFit()
    req(m)

    choices = buildModelPlotTypeChoices(m)

    selectInput(
      inputId = "modelPlotType",
      label = "Plot type",
      choices = choices,
      selected = unname(choices)[1],
      width = "360px"
    )
  })

  output$modelPlotSmoothTrendUi = renderUI({
    m = modelFit()
    req(m)

    plotFamily = classifyModelPlotFamily(m)

    if (!identical(plotFamily, "lm")) {
      return(NULL)
    }

    tagList(
      div(
        class = "wmfm-model-plot-smoother-control",
        checkboxInput(
          inputId = "modelPlotShowSmoothTrend",
          label = "Show smooth trend",
          value = TRUE
        ),
        helpText(
          "The blue smooth trend is chosen automatically and can help reveal broad patterns. It is not a formal test."
        )
      )
    )
  })

  output$modelPlotSummaryUi = renderUI({
    m = modelFit()
    req(m)

    plotType = input$modelPlotType %||% "observedFitted"
    summaryText = buildModelPlotSummaryText(model = m, plotType = plotType)

    helpText(summaryText)
  })

  output$modelPlotTeachingNoteUi = renderUI({
    m = modelFit()
    req(m)

    plotType = input$modelPlotType %||% "observedFitted"
    note = buildModelPlotTeachingNote(model = m, plotType = plotType)

    div(
      class = "wmfm-model-plot-note",
      tags$div(
        class = "wmfm-model-plot-note-heading",
        note$title
      ),
      tags$dl(
        tags$dt("What this plot shows"),
        tags$dd(note$shows),
        tags$dt("What to look for"),
        tags$dd(note$lookFor),
        tags$dt("What this plot cannot prove"),
        tags$dd(note$cannotProve)
      )
    )
  })


  output$modelPlotsDownload = downloadHandler(
    filename = function() {
      plotType = input$modelPlotType %||% "observedFitted"
      buildModelPlotDownloadFilename(plotType = plotType)
    },
    content = function(file) {
      m = modelFit()
      req(m)

      plotType = input$modelPlotType %||% "observedFitted"
      showSmoothTrend = isTRUE(input$modelPlotShowSmoothTrend %||% TRUE)
      plot = plotModelPlot(
        model = m,
        plotType = plotType,
        showSmoothTrend = showSmoothTrend
      )

      validate(
        need(
          !is.null(plot),
          "Model plots are not available for this fitted model."
        )
      )

      ggsave(
        filename = file,
        plot = plot,
        width = 7,
        height = 4.5,
        units = "in",
        dpi = 300
      )
    }
  )

  output$modelPlotsPlot = renderPlot({
    m = modelFit()
    req(m)

    plotType = input$modelPlotType %||% "observedFitted"
    showSmoothTrend = isTRUE(input$modelPlotShowSmoothTrend %||% TRUE)
    plot = plotModelPlot(
      model = m,
      plotType = plotType,
      showSmoothTrend = showSmoothTrend
    )

    validate(
      need(
        !is.null(plot),
        "Model plots are not available for this fitted model."
      )
    )

    print(plot)
    invisible(NULL)
  })

  invisible(NULL)
}
