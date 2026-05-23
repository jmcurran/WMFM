#' Determine whether adjustment-output controls should be shown
#'
#' @param model Fitted model object or \code{NULL}.
#'
#' @return \code{TRUE} when the fitted model includes at least one adjustment
#'   variable; otherwise \code{FALSE}.
#'
#' @keywords internal
shouldShowAdjustmentOutputControls = function(model) {
  if (is.null(model)) {
    return(FALSE)
  }

  adjustmentVariables = getModelAdjustmentVariables(model)
  length(adjustmentVariables) > 0
}
