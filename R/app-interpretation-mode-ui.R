#' Build interpretation-mode banner text for adjustment workflows
#'
#' Returns a compact interpretation-mode label shown near explanation surfaces
#' when adjustment variables are selected.
#'
#' @param adjustmentVariables Character vector of selected adjustment variables.
#'
#' @return Character scalar banner text, or `NULL` when no adjustment variables
#'   are selected.
#'
#' @keywords internal
buildInterpretationModeLabel = function(adjustmentVariables) {
  selectedVariables = unique(as.character(adjustmentVariables %||% character(0)))
  selectedVariables = selectedVariables[nzchar(selectedVariables)]

  if (length(selectedVariables) == 0) {
    return(NULL)
  }

  paste0(
    "Interpretation mode: primary effects adjusted for ",
    paste(selectedVariables, collapse = ", ")
  )
}
