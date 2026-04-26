#' Get success and failure labels for a binomial response
#'
#' For a two-level factor response, R's binomial GLM parameterisation targets the
#' second factor level as the fitted probability. This helper extracts readable
#' labels for the success and failure outcomes so the app can write
#'
#' `Pr(Pass = Pass)` and `Odds(Pass = Pass)` rather than a generic `p`.
#'
#' @param model A fitted model object.
#'
#' @return A list with `successLabel`, `failureLabel`, and `responseName`.
#' @keywords internal
getBinomialOutcomeLabels = function(model) {

  mf = stats::model.frame(model)
  responseName = names(mf)[1]
  y = mf[[1]]

  if (is.factor(y) && nlevels(y) == 2) {
    return(list(
      successLabel = as.character(levels(y)[2]),
      failureLabel = as.character(levels(y)[1]),
      responseName = responseName
    ))
  }

  if (is.logical(y)) {
    return(list(
      successLabel = "TRUE",
      failureLabel = "FALSE",
      responseName = responseName
    ))
  }

  if (is.numeric(y) || is.integer(y)) {
    values = sort(unique(stats::na.omit(y)))

    if (length(values) == 2) {
      return(list(
        successLabel = as.character(values[2]),
        failureLabel = as.character(values[1]),
        responseName = responseName
      ))
    }
  }

  list(
    successLabel = "Success",
    failureLabel = "Failure",
    responseName = responseName
  )
}

#' Format a probability label for a binomial model
#'
#' @param model A fitted binomial model.
#' @param outcome Either `"success"` or `"failure"`.
#' @param indexed Logical. If `TRUE`, append `_i` to the response name.
#'
#' @return A character scalar such as `"Pr(Pass = Pass)"` or `"Pr(Pass_i = Pass)"`.
#' @keywords internal
formatBinomialProbabilityLabel = function(model, outcome = c("success", "failure"), indexed = FALSE) {

  outcome = match.arg(outcome)
  labels = getBinomialOutcomeLabels(model)
  outcomeLabel = if (identical(outcome, "success")) labels$successLabel else labels$failureLabel
  responseLabel = labels$responseName

  if (indexed) {
    responseLabel = paste0(responseLabel, "_i")
  }

  paste0("Pr(", responseLabel, " = ", outcomeLabel, ")")
}

#' Format an odds label for a binomial model
#'
#' @param model A fitted binomial model.
#' @param outcome Either `"success"` or `"failure"`.
#' @param indexed Logical. If `TRUE`, append `_i` to the response name.
#'
#' @return A character scalar such as `"Odds(Pass = Pass)"` or `"Odds(Pass_i = Pass)"`.
#' @keywords internal
formatBinomialOddsLabel = function(model, outcome = c("success", "failure"), indexed = FALSE) {

  outcome = match.arg(outcome)
  labels = getBinomialOutcomeLabels(model)
  outcomeLabel = if (identical(outcome, "success")) labels$successLabel else labels$failureLabel
  responseLabel = labels$responseName

  if (indexed) {
    responseLabel = paste0(responseLabel, "_i")
  }

  paste0("Odds(", responseLabel, " = ", outcomeLabel, ")")
}
