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

  validated = lapply(seq_along(parsed), function(i) {
    item = parsed[[i]]

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

    nameValue = safeWmfmScalar(item$name, naString = NA_character_)
    textValue = safeWmfmScalar(item$text, naString = NA_character_)
    severityValue = safeWmfmScalar(item$severity, naString = NA_character_)

    errorTypesValue = item$errorTypes

    if (is.list(errorTypesValue)) {
      errorTypesValue = unlist(errorTypesValue, use.names = FALSE)
    }

    errorTypesValue = as.character(errorTypesValue)
    errorTypesValue = errorTypesValue[!is.na(errorTypesValue)]
    errorTypesValue = trimws(errorTypesValue)
    errorTypesValue = errorTypesValue[nzchar(errorTypesValue)]

    if (is.na(nameValue) || !nzchar(trimws(nameValue))) {
      stop("Each generated explanation must have a valid `name` field.", call. = FALSE)
    }

    if (is.na(textValue) || !nzchar(trimws(textValue))) {
      stop("Each generated explanation must have non-empty `text`.", call. = FALSE)
    }

    if (length(errorTypesValue) < 1L) {
      stop(
        "Each generated explanation must have at least one valid `errorTypes` entry.",
        call. = FALSE
      )
    }

    if (is.na(severityValue) || !nzchar(trimws(severityValue))) {
      stop("Each generated explanation must have a valid `severity` field.", call. = FALSE)
    }

    list(
      name = nameValue,
      text = textValue,
      errorTypes = errorTypesValue,
      severity = severityValue
    )
  })

  responseNames = vapply(validated, function(item) item$name, character(1))

  if (!setequal(responseNames, plan$explanationNames)) {
    stop(
      "Generated explanation names did not match the requested explanation names.",
      call. = FALSE
    )
  }

  orderedIdx = match(plan$explanationNames, responseNames)
  validated = validated[orderedIdx]

  validated
}
