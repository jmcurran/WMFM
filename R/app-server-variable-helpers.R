#' Build title for numeric-as-factor confirmation modal.
#'
#' @return A character scalar modal title.
#'
#' @keywords internal
buildNumericFactorConfirmTitle = function() {
  "Treat numeric variable as factor?"
}

#' Build body text for numeric-as-factor confirmation modal.
#'
#' @param variableName Character scalar variable name.
#'
#' @return A character scalar modal body.
#'
#' @keywords internal
buildNumericFactorConfirmMessage = function(variableName) {
  paste0(
    "You moved '", variableName, "' into the Factors bucket, ",
    "but in the data it is numeric.\n\n",
    "Do you want to treat this variable as a factor in the model?"
  )
}

#' Build cancel label for numeric-as-factor confirmation modal.
#'
#' @return A character scalar button label.
#'
#' @keywords internal
buildNumericFactorCancelLabel = function() {
  "No, I'll move it back"
}

#' Build confirm label for numeric-as-factor confirmation modal.
#'
#' @return A character scalar button label.
#'
#' @keywords internal
buildNumericFactorConfirmLabel = function() {
  "Yes, treat as factor"
}

#' Build title for add-derived-variable modal.
#'
#' @return A character scalar modal title.
#'
#' @keywords internal
buildAddDerivedVariableTitle = function() {
  "Add derived variable"
}

#' Build help text for add-derived-variable modal.
#'
#' @return A character scalar help text.
#'
#' @keywords internal
buildAddDerivedVariableHelpText = function() {
  "Enter a single R expression of the form newVariable = ... . The new variable will be added to the dataset and will then be available in the response picker and variable buckets."
}

#' Build placeholder for add-derived-variable expression input.
#'
#' @return A character scalar placeholder.
#'
#' @keywords internal
buildAddDerivedVariablePlaceholder = function() {
  "e.g. t = 1:nrow(data)    or    month = factor(rep(1:12, 12))"
}

#' Build confirm label for add-derived-variable modal.
#'
#' @return A character scalar button label.
#'
#' @keywords internal
buildAddDerivedVariableConfirmLabel = function() {
  "Add variable"
}
