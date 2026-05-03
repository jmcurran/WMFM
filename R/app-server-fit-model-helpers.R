#' Build message for a failed chat provider connection.
#'
#' @param details Character scalar error details.
#'
#' @return A character scalar notification message.
#'
#' @keywords internal
buildChatProviderConnectionFailedMessage = function(details) {
  paste(
    "Could not connect to the language model server.",
    "The model will still be fitted, but equations/explanation\n",
    "from the language model will be unavailable.\n\nDetails: ",
    details
  )
}

#' Build message for missing language model fallback.
#'
#' @return A character scalar notification message.
#'
#' @keywords internal
buildNoLanguageModelAvailableMessage = function() {
  paste(
    "No language model is available at the moment.",
    "The model will still be fitted and deterministic equations will be shown,",
    "but no narrative explanation can be generated."
  )
}

#' Build message for too many predictors in the fitted model formula.
#'
#' @param predictorNames Character vector of predictor names.
#'
#' @return A character scalar notification message.
#'
#' @keywords internal
buildTooManyPredictorsMessage = function(predictorNames) {
  paste0(
    "This app only allows models with at most 3 covariates. ",
    "Your formula currently uses ", length(predictorNames), " predictors: ",
    paste(predictorNames, collapse = ", "), "."
  )
}

#' Build message for recoding a binary factor response for linear regression.
#'
#' @param responseName Character scalar response variable name.
#' @param levels Character vector containing the two factor levels.
#'
#' @return A character scalar notification message.
#'
#' @keywords internal
buildLinearModelBinaryFactorRecodingMessage = function(responseName, levels) {
  paste0(
    "The response variable '", responseName, "' is a 2-level factor.\n",
    "For linear regression, it has been recoded to numeric.\n",
    "Coding used:  ", levels[1], " -> 0,   ", levels[2], " -> 1"
  )
}

#' Build message for non-binary character logistic responses.
#'
#' @param responseName Character scalar response variable name.
#' @param nValues Integer number of distinct values.
#'
#' @return A character scalar notification message.
#'
#' @keywords internal
buildLogisticCharacterResponseMessage = function(responseName, nValues) {
  paste0(
    "Logistic regression requires a binary response. ",
    responseName, " is a character vector with ", nValues, " distinct values."
  )
}

#' Build message for non-binary factor logistic responses.
#'
#' @param responseName Character scalar response variable name.
#' @param nLevels Integer number of factor levels.
#'
#' @return A character scalar notification message.
#'
#' @keywords internal
buildLogisticFactorResponseMessage = function(responseName, nLevels) {
  paste0(
    "Logistic regression requires a factor with 2 levels. ",
    responseName, " has ", nLevels, " levels."
  )
}

#' Build message for numeric logistic responses with values other than 0/1.
#'
#' @param responseName Character scalar response variable name.
#' @param values Numeric vector of observed response values.
#'
#' @return A character scalar notification message.
#'
#' @keywords internal
buildLogisticNumericResponseMessage = function(responseName, values) {
  paste0(
    "Numeric logistic responses must be 0/1. ",
    responseName, " has values: ",
    paste(head(sort(values), 5), collapse = ", "), " ..."
  )
}

#' Build message for unsupported logistic response types.
#'
#' @return A character scalar notification message.
#'
#' @keywords internal
buildLogisticUnsupportedResponseMessage = function() {
  "Logistic regression requires either a binary factor, numeric 0/1, or a 2-level character vector."
}

#' Build message for Poisson response warnings.
#'
#' @return A character scalar notification message.
#'
#' @keywords internal
buildPoissonResponseWarningMessage = function() {
  "Warning: response has negative or non-integer values. Poisson regression expects non-negative counts."
}

#' Build message for unknown model type errors.
#'
#' @return A character scalar notification message.
#'
#' @keywords internal
buildUnknownModelTypeMessage = function() {
  "Unknown model type."
}
