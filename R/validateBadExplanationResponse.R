#' Validate parsed bad explanation output
#'
#' @param parsed Parsed JSON object.
#' @param plan Plan object produced by `buildBadExplanationPlan()`.
#'
#' @return Validated parsed object.
#' @keywords internal
validateBadExplanationResponse = function(parsed, plan) {

  if (!is.list(parsed) || length(parsed) != length(plan$explanationNames)) {
    stop(
      "Bad explanation response did not return the expected number of explanations.",
      call. = FALSE
    )
  }

  parsed = lapply(parsed, function(item) {

    if (!is.list(item)) {
      stop("Each generated explanation must be a JSON object.", call. = FALSE)
    }

    requiredFields = c("name", "text", "errorTypes", "severity")
    missingFields = setdiff(requiredFields, names(item))

    if (length(missingFields) > 0) {
      stop(
        paste0(
          "Generated explanation is missing required field(s): ",
          paste(missingFields, collapse = ", ")
        ),
        call. = FALSE
      )
    }

    itemName = safeWmfmScalar(item$name, naString = "")
    itemText = safeWmfmScalar(item$text, naString = "")
    itemSeverity = safeWmfmScalar(item$severity, naString = "")

    errorTypes = item$errorTypes

    if (is.list(errorTypes)) {
      errorTypes = unlist(errorTypes, use.names = FALSE)
    }

    if (is.null(errorTypes)) {
      errorTypes = character(0)
    }

    errorTypes = as.character(errorTypes)
    errorTypes = errorTypes[!is.na(errorTypes)]
    errorTypes = trimws(errorTypes)
    errorTypes = errorTypes[nzchar(errorTypes)]

    if (!nzchar(itemName)) {
      stop("Each generated explanation must have a valid `name` field.", call. = FALSE)
    }

    if (!nzchar(itemText)) {
      stop("Each generated explanation must have non-empty `text`.", call. = FALSE)
    }

    if (length(errorTypes) < 1) {
      stop(
        "Each generated explanation must have at least one valid `errorTypes` entry.",
        call. = FALSE
      )
    }

    if (!nzchar(itemSeverity)) {
      stop("Each generated explanation must have a valid `severity` field.", call. = FALSE)
    }

    list(
      name = itemName,
      text = itemText,
      errorTypes = errorTypes,
      severity = itemSeverity
    )
  })

  returnedNames = vapply(parsed, function(item) item$name, character(1))

  if (!identical(returnedNames, plan$explanationNames)) {
    stop(
      paste(
        "Generated explanation names did not match the requested plan names."
      ),
      call. = FALSE
    )
  }

  parsed
}
