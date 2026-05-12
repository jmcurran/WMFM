#' Register model output observers for the app server
#'
#' @param input Shiny input object.
#' @param output Shiny output object.
#' @param modelFit Reactive value containing the fitted model.
#'
#' @return No return value; called for output side effects.
#'
#' @keywords internal
#'
#' @importFrom shiny checkboxInput helpText renderPrint renderUI tagList
registerModelSummaryObservers = function(input, output, modelFit) {

  output$modelSummaryControlsUi = renderUI({
    checkboxInput(
      inputId = "showAdjustmentCoefficients",
      label = "Show adjustment coefficients",
      value = FALSE
    )
  })

  output$modelSummaryAdjustmentNoteUi = renderUI({
    m = modelFit()

    if (is.null(m) || isTRUE(input$showAdjustmentCoefficients %||% FALSE)) {
      return(NULL)
    }

    adjustmentVariables = getModelAdjustmentVariables(m)
    if (length(adjustmentVariables) == 0) {
      return(NULL)
    }

    helpText(
      paste0(
        "Adjustment terms involving ",
        paste(adjustmentVariables, collapse = ", "),
        " are hidden from this summary view. They remain included in the fitted model."
      )
    )
  })

  output$model_output = renderPrint({
    m = modelFit()
    if (is.null(m)) {
      cat("No model fitted yet.")
      return()
    }

    modelSummary = summary(m)
    adjustmentVariables = getModelAdjustmentVariables(m)

    modelSummary$coefficients = filterSummaryCoefficientRows(
      coefficientsMatrix = modelSummary$coefficients,
      adjustmentVariables = adjustmentVariables,
      showAdjustmentCoefficients = isTRUE(input$showAdjustmentCoefficients %||% FALSE)
    )

    out = capture.output(print(modelSummary))
    idx = grep("^Coefficients:", out)
    out = out[idx:length(out)]

    cat(out, sep = "\n")
  })

  invisible(NULL)
}
