#' Normalise one or more WMFM explanations
#'
#' Internal helper that accepts a character vector or a list of character
#' scalars and returns a named character vector suitable for batch grading.
#'
#' @param explanation Character vector or list of character scalars.
#'
#' @return A named character vector.
#' @keywords internal
normalizeWmfmExplanations = function(explanation) {

  if (is.character(explanation)) {
    explanationVec = explanation
  } else if (is.list(explanation)) {
    if (!all(vapply(explanation, function(x) {
      is.character(x) && length(x) == 1 && !is.na(x)
    }, logical(1)))) {
      stop(
        "When `explanation` is a list, each element must be a single non-missing character string.",
        call. = FALSE
      )
    }

    explanationVec = unlist(explanation, use.names = TRUE)
  } else {
    stop(
      "`explanation` must be a character vector or a list of character scalars.",
      call. = FALSE
    )
  }

  if (length(explanationVec) < 1) {
    stop("`explanation` must contain at least one explanation.", call. = FALSE)
  }

  if (anyNA(explanationVec) || any(!nzchar(explanationVec))) {
    stop(
      "All explanations must be non-missing, non-empty character strings.",
      call. = FALSE
    )
  }

  explanationNames = names(explanationVec)
  missingNames = is.null(explanationNames) | !nzchar(explanationNames)

  if (is.null(explanationNames)) {
    explanationNames = rep("", length(explanationVec))
    missingNames = rep(TRUE, length(explanationVec))
  }

  if (any(missingNames)) {
    width = nchar(as.character(length(explanationVec)))
    autoNames = paste0(
      "explanation_",
      formatC(seq_len(length(explanationVec)), width = width, flag = "0")
    )
    explanationNames[missingNames] = autoNames[missingNames]
  }

  explanationNames = make.unique(explanationNames, sep = "_")
  names(explanationVec) = explanationNames

  explanationVec
}
