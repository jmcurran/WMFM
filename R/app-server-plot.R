#' Register model plot observers
#'
#' Wires the model plotting outputs for the fitted model tab.
#'
#' @param input Shiny input object.
#' @param output Shiny output object.
#' @param modelFit Reactive value containing the fitted model.
#'
#' @return No return value; called for its side effects.
#'
#' @keywords internal
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

  invisible(NULL)
}
