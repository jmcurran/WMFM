#' Register model output observers for the app server
#'
#' @param output Shiny output object.
#' @param modelFit Reactive value containing the fitted model.
#'
#' @return No return value; called for output side effects.
#'
#' @keywords internal
#'
#' @importFrom shiny renderPrint
#' @importFrom utils capture.output
registerModelSummaryObservers = function(output, modelFit) {
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

  invisible(NULL)
}
