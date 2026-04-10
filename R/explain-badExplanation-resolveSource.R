#' Resolve the source explanation for bad explanation generation
#'
#' @param x A `wmfmModel` object.
#' @param explanation Optional user-supplied explanation.
#'
#' @return A character scalar giving the base explanation.
#' @keywords internal
resolveBadExplanationSource = function(x, explanation = NULL) {

  if (!is.null(explanation)) {
    if (!nzchar(trimws(explanation))) {
      stop("`explanation` must not be empty.", call. = FALSE)
    }

    return(explanation)
  }

  objectExplanation = x$explanation

  if (!is.character(objectExplanation) ||
      length(objectExplanation) != 1L ||
      is.na(objectExplanation) ||
      !nzchar(trimws(objectExplanation))) {
    stop(
      paste(
        "`generateBadExplanation()` requires either a supplied `explanation`",
        "or a non-empty `x$explanation`."
      ),
      call. = FALSE
    )
  }

  objectExplanation
}
