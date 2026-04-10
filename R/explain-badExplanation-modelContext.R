#' Build compact model context for bad explanation generation
#'
#' @param x A `wmfmModel` object.
#'
#' @return A character scalar.
#' @keywords internal
buildBadExplanationModelContext = function(x) {

  formulaText = paste(deparse(x$formula), collapse = "")
  responseName = tryCatch(
    all.vars(stats::formula(x$model))[1],
    error = function(e) {
      NA_character_
    }
  )

  factorPredictors = tryCatch(
    getFactorPredictors(x$model, x$data),
    error = function(e) {
      character(0)
    }
  )

  interactionTerms = x$interactionTerms %||% character(0)
  interactionTerms = interactionTerms[!is.na(interactionTerms) & nzchar(interactionTerms)]

  parts = c(
    paste0("Model type: ", safeWmfmScalar(x$modelType)),
    paste0("Formula: ", formulaText),
    paste0("Response: ", safeWmfmScalar(responseName)),
    if (length(factorPredictors) > 0L) {
      paste0("Factor predictors: ", paste(factorPredictors, collapse = ", "))
    } else {
      "Factor predictors: none"
    },
    if (length(interactionTerms) > 0L) {
      paste0("Interaction terms: ", paste(interactionTerms, collapse = ", "))
    } else {
      "Interaction terms: none"
    },
    buildBadExplanationReferenceLevels(x),
    if (!is.null(x$dataContext) && !is.na(x$dataContext) && nzchar(trimws(x$dataContext))) {
      paste0("Data context: ", x$dataContext)
    } else {
      NULL
    }
  )

  paste(parts, collapse = "\n")
}
