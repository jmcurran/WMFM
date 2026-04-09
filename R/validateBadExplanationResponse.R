#' Validate parsed bad explanation output
#'
#' @param parsed Parsed JSON object.
#' @param plan A plan object produced by `buildBadExplanationPlan()`.
#'
#' @return A validated and reordered parsed object.
#' @keywords internal
validateBadExplanationResponse = function(parsed, plan) {

  if (!is.list(parsed) || length(parsed) != length(plan$explanationNames)) {
    stop(
      "Bad explanation response did not return the expected number of explanations.",
      call. = FALSE
    )
  }

  itemNames = vapply(parsed, function(item) {
    if (is.list(item) && !is.null(item$name) && is.character(item$name) && length(item$name) == 1L) {
      item$name
    } else {
      NA_character_
    }
  }, character(1))

  if (anyNA(itemNames)) {
    stop("Each generated explanation must include a valid `name` field.", call. = FALSE)
  }

  missingNames = setdiff(plan$explanationNames, itemNames)
  extraNames = setdiff(itemNames, plan$explanationNames)

  if (length(missingNames) > 0L || length(extraNames) > 0L) {
    stop(
      paste0(
        "Generated explanation names did not match the requested plan. ",
        if (length(missingNames) > 0L) {
          paste0("Missing: ", paste(missingNames, collapse = ", "), ". ")
        } else {
          ""
        },
        if (length(extraNames) > 0L) {
          paste0("Unexpected: ", paste(extraNames, collapse = ", "), ".")
        } else {
          ""
        }
      ),
      call. = FALSE
    )
  }

  parsed = parsed[match(plan$explanationNames, itemNames)]

  for (i in seq_along(parsed)) {
    item = parsed[[i]]

    if (!is.list(item)) {
      stop("Each generated explanation must be a JSON object.", call. = FALSE)
    }

    requiredFields = c("name", "text", "errorTypes", "severity")
    missingFields = setdiff(requiredFields, names(item))

    if (length(missingFields) > 0L) {
      stop(
        paste0(
          "Generated explanation is missing required field(s): ",
          paste(missingFields, collapse = ", ")
        ),
        call. = FALSE
      )
    }

    if (!is.character(item$text) || length(item$text) != 1L || is.na(item$text)) {
      stop("Each generated explanation must have a valid `text` field.", call. = FALSE)
    }

    if (!nzchar(trimws(item$text))) {
      stop("Each generated explanation must have non-empty `text`.", call. = FALSE)
    }

    if (!is.character(item$errorTypes) || length(item$errorTypes) < 1L || anyNA(item$errorTypes)) {
      stop(
        "Each generated explanation must have at least one valid `errorTypes` entry.",
        call. = FALSE
      )
    }

    unsupportedTypes = setdiff(item$errorTypes, listBadExplanationTypes())

    if (length(unsupportedTypes) > 0L) {
      stop(
        paste0(
          "Generated explanation returned unsupported error type(s): ",
          paste(unsupportedTypes, collapse = ", ")
        ),
        call. = FALSE
      )
    }

    if (!is.character(item$severity) || length(item$severity) != 1L || is.na(item$severity)) {
      stop("Each generated explanation must have a valid `severity` field.", call. = FALSE)
    }

    if (!item$severity %in% c("subtle", "moderate", "severe")) {
      stop(
        "Each generated explanation must have severity 'subtle', 'moderate', or 'severe'.",
        call. = FALSE
      )
    }
  }

  parsed
}
