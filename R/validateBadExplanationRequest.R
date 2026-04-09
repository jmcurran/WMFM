#' Validate a bad explanation generation request
#'
#' @param x A `wmfmModel` object.
#' @param explanation Optional character scalar giving the base explanation.
#' @param type Character vector of bad explanation types, or `"auto"`.
#' @param severity Character scalar.
#' @param n Integer scalar.
#' @param mixTypes Logical scalar.
#' @param labelErrors Logical scalar.
#'
#' @return Invisibly returns `TRUE` when validation succeeds.
#' @keywords internal
validateBadExplanationRequest = function(
    x,
    explanation,
    type,
    severity,
    n,
    mixTypes,
    labelErrors
) {

  if (!inherits(x, "wmfmModel")) {
    stop("`x` must inherit from `wmfmModel`.", call. = FALSE)
  }

  if (!is.null(explanation)) {
    if (!is.character(explanation) || length(explanation) != 1L || is.na(explanation)) {
      stop(
        "`explanation` must be NULL or a single non-missing character string.",
        call. = FALSE
      )
    }
  }

  if (!is.character(type) || length(type) < 1L || anyNA(type)) {
    stop("`type` must be a non-empty character vector.", call. = FALSE)
  }

  if (!is.character(severity) || length(severity) != 1L || is.na(severity)) {
    stop("`severity` must be a single non-missing character string.", call. = FALSE)
  }

  if (!severity %in% c("subtle", "moderate", "severe")) {
    stop(
      "`severity` must be one of 'subtle', 'moderate', or 'severe'.",
      call. = FALSE
    )
  }

  n = as.integer(n)[1]

  if (is.na(n) || n < 1L) {
    stop("`n` must be an integer greater than or equal to 1.", call. = FALSE)
  }

  if (!is.logical(mixTypes) || length(mixTypes) != 1L || is.na(mixTypes)) {
    stop("`mixTypes` must be TRUE or FALSE.", call. = FALSE)
  }

  if (!is.logical(labelErrors) || length(labelErrors) != 1L || is.na(labelErrors)) {
    stop("`labelErrors` must be TRUE or FALSE.", call. = FALSE)
  }

  if (!identical(type, "auto")) {
    unsupportedTypes = setdiff(type, listBadExplanationTypes())

    if (length(unsupportedTypes) > 0L) {
      stop(
        paste0(
          "Unsupported bad explanation type(s): ",
          paste(unsupportedTypes, collapse = ", ")
        ),
        call. = FALSE
      )
    }
  }

  invisible(TRUE)
}
