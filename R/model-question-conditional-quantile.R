#' Classify conditional-value questions requiring distributional modelling
#'
#' Identifies questions that ask what value would count as relatively low,
#' high, cheap, expensive, good, or unusual for a case with specified
#' characteristics. These questions require a conditional distribution rather
#' than inspection of residuals from an ordinary conditional-mean model.
#'
#' @param normalizedText Lower-case normalized question text.
#'
#' @return A named list containing `matched`, `reasonCode`, and
#'   `deterministicResponse`.
#' @keywords internal
#' @noRd
classifyConditionalQuantileQuestion = function(normalizedText) {
  text = trimws(as.character(normalizedText %||% ""))

  result = list(
    matched = FALSE,
    reasonCode = NULL,
    deterministicResponse = NULL
  )

  if (!nzchar(text)) {
    return(result)
  }

  valueTargetPattern = paste(
    c(
      "\\b(price|cost|value|mark|score|result|outcome|response|amount|level)\\b",
      "\\bhow much\\b"
    ),
    collapse = "|"
  )

  relativeThresholdPattern = paste(
    c(
      "\\bgood deal\\b",
      "\\bbargain\\b",
      "\\bcheap(?:er)?\\b",
      "\\bexpensive\\b",
      "\\brelatively (low|high|cheap|expensive)\\b",
      "\\bwhat (?:would )?count as (?:a )?(low|high|cheap|expensive|good)\\b",
      "\\bwhat is (?:a )?(low|high|cheap|expensive|good)\\b",
      "\\bhow (low|high|cheap|expensive)\\b",
      "\\b(bottom|lower|upper|top) (quarter|quartile|decile|percentile)\\b",
      "\\b(10th|25th|50th|75th|90th) percentile\\b",
      "\\bconditional (quantile|percentile|distribution)\\b"
    ),
    collapse = "|"
  )

  caseConditionPattern = paste(
    c(
      "\\bfor\\b.+\\b[[:alnum:]][[:alnum:]_.-]*\\b",
      "\\bwith\\b.+\\b[[:alnum:]][[:alnum:]_.-]*\\b",
      "\\bwhen\\b.+\\b[[:alnum:]][[:alnum:]_.-]*\\b",
      "\\bat\\b.+\\b[[:digit:]]+(?:\\.[[:digit:]]+)?\\b",
      "\\b[[:digit:]]+(?:\\.[[:digit:]]+)?[- ]?(carat|caret|unit|year|point)\\b"
    ),
    collapse = "|"
  )

  coefficientPattern = "\\b(coefficient|slope|effect|estimate|parameter)\\b"
  existingObservationPattern = paste(
    c(
      "\\bwhich (observations|cases|rows|students|diamonds|points)\\b",
      "\\bidentify (the )?(observations|cases|rows|students|diamonds|points)\\b",
      "\\brank (the )?(observations|cases|rows|students|diamonds|points)\\b"
    ),
    collapse = "|"
  )

  hasValueTarget = grepl(valueTargetPattern, text, perl = TRUE) ||
    grepl("\\b(good deal|bargain)\\b", text, perl = TRUE)
  hasRelativeThreshold = grepl(relativeThresholdPattern, text, perl = TRUE)
  hasCaseCondition = grepl(caseConditionPattern, text, perl = TRUE)
  isCoefficientQuestion = grepl(coefficientPattern, text, perl = TRUE)
  isExistingObservationQuestion = grepl(existingObservationPattern, text, perl = TRUE)

  if (hasValueTarget && hasRelativeThreshold && hasCaseCondition &&
      !isCoefficientQuestion && !isExistingObservationQuestion) {
    result$matched = TRUE
    result$reasonCode = "conditional_distribution_required"
    result$deterministicResponse = paste(
      "This question asks for a value in a lower or upper part of the conditional distribution for a specified case.",
      "An ordinary linear model estimates the conditional mean, so residual inspection does not directly answer it.",
      "A method that estimates conditional quantiles, such as quantile regression, would be needed after the intended percentile has been defined."
    )
  }

  result
}
