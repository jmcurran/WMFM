#' Return required top-level fields for a WMFM explanation audit
#'
#' @return A character vector of required top-level field names.
#' @keywords internal
getWmfmExplanationAuditRequiredFields = function() {

  c(
    "transparencyNote",
    "overview",
    "promptInputs",
    "promptRules",
    "interpretationScale",
    "numericAnchor",
    "referenceLevels",
    "confidenceIntervals",
    "baselineEvidence",
    "effectEvidence",
    "coefficientTable",
    "rawPromptIngredients"
  )
}

#' Validate a WMFM explanation audit object
#'
#' Checks that an object has the expected class and required top-level fields
#' used by the deterministic explanation-audit workflow.
#'
#' @param x Object to validate.
#' @param allowNull Logical. If `TRUE`, `NULL` is accepted and returned
#'   invisibly. Defaults to `FALSE`.
#'
#' @return Invisibly returns `TRUE` when validation succeeds.
#' @keywords internal
validateWmfmExplanationAudit = function(x, allowNull = FALSE) {

  if (is.null(x)) {
    if (isTRUE(allowNull)) {
      return(invisible(TRUE))
    }

    stop("`x` must not be NULL.", call. = FALSE)
  }

  if (!inherits(x, "wmfmExplanationAudit")) {
    stop("`x` must inherit from `wmfmExplanationAudit`.", call. = FALSE)
  }

  if (!is.list(x)) {
    stop("`x` must be a list-like `wmfmExplanationAudit` object.", call. = FALSE)
  }

  requiredFields = getWmfmExplanationAuditRequiredFields()
  missingFields = setdiff(requiredFields, names(x))

  if (length(missingFields) > 0) {
    stop(
      paste0(
        "`x` is missing required explanation-audit fields: ",
        paste(missingFields, collapse = ", "),
        "."
      ),
      call. = FALSE
    )
  }

  invisible(TRUE)
}
