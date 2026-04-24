#' Format a number for student-facing explanation text
#'
#' Formats numeric values using a small significant-figures rule for prose and
#' prompt inputs. This helper is intended for explanation quantities rather than
#' developer-facing coefficient tables.
#'
#' @param x Numeric vector.
#' @param sigDigits Number of significant figures to keep. Defaults to 2.
#'
#' @return A character vector with one formatted value per input value.
#'
#' @keywords internal
formatExplanationNumber = function(x, sigDigits = 2) {

  validateExplanationSigDigits(sigDigits)

  if (!is.numeric(x)) {
    stop("`x` must be numeric.", call. = FALSE)
  }

  vapply(
    x,
    formatExplanationNumberScalar,
    character(1),
    sigDigits = as.integer(sigDigits),
    USE.NAMES = FALSE
  )
}

#' Format a confidence interval for student-facing explanation text
#'
#' Applies the same significant-figures rule to an estimate and its confidence
#' interval bounds so that all three values are visually consistent.
#'
#' @param estimate Numeric scalar estimate.
#' @param lower Numeric scalar lower interval bound.
#' @param upper Numeric scalar upper interval bound.
#' @param confidenceLevel Numeric scalar confidence level. Defaults to 0.95.
#' @param sigDigits Number of significant figures to keep. Defaults to 2.
#'
#' @return A named list with formatted estimate, lower, upper, level, and text.
#'
#' @keywords internal
formatExplanationConfidenceInterval = function(
    estimate,
    lower,
    upper,
    confidenceLevel = 0.95,
    sigDigits = 2) {

  validateExplanationNumericScalar(estimate, "estimate")
  validateExplanationNumericScalar(lower, "lower")
  validateExplanationNumericScalar(upper, "upper")
  validateExplanationNumericScalar(confidenceLevel, "confidenceLevel")
  validateExplanationSigDigits(sigDigits)

  if (confidenceLevel <= 0 || confidenceLevel >= 1) {
    stop("`confidenceLevel` must be between 0 and 1.", call. = FALSE)
  }

  levelText = paste0(round(confidenceLevel * 100), "%")
  estimateText = formatExplanationNumber(estimate, sigDigits = sigDigits)
  lowerText = formatExplanationNumber(lower, sigDigits = sigDigits)
  upperText = formatExplanationNumber(upper, sigDigits = sigDigits)

  list(
    estimate = estimateText,
    lower = lowerText,
    upper = upperText,
    confidenceLevel = levelText,
    text = paste0(
      estimateText,
      ", with a ",
      levelText,
      " confidence interval from ",
      lowerText,
      " to ",
      upperText
    )
  )
}

#' Format a probability for student-facing explanation text
#'
#' Formats probabilities as percentages by default, using whole percentages for
#' ordinary values and simple boundary text for very small or very large
#' non-zero probabilities.
#'
#' @param probability Numeric vector of probabilities on the 0 to 1 scale.
#' @param asPercent Logical scalar. If `TRUE`, return percentages. If `FALSE`,
#'   return probabilities using `formatExplanationNumber()`.
#' @param sigDigits Number of significant figures to keep when `asPercent` is
#'   `FALSE`.
#'
#' @return A character vector.
#'
#' @keywords internal
formatExplanationProbability = function(probability, asPercent = TRUE, sigDigits = 2) {

  validateExplanationSigDigits(sigDigits)

  if (!is.numeric(probability)) {
    stop("`probability` must be numeric.", call. = FALSE)
  }

  if (!is.logical(asPercent) || length(asPercent) != 1 || is.na(asPercent)) {
    stop("`asPercent` must be TRUE or FALSE.", call. = FALSE)
  }

  vapply(
    probability,
    formatExplanationProbabilityScalar,
    character(1),
    asPercent = asPercent,
    sigDigits = as.integer(sigDigits),
    USE.NAMES = FALSE
  )
}

#' Format odds for student-facing explanation text
#'
#' Formats odds as odds rather than plain decimals. Ordinary odds are displayed
#' as `odds:1`; very small odds are displayed as `1:N`.
#'
#' @param odds Numeric vector of non-negative odds.
#' @param sigDigits Number of significant figures to keep for ordinary odds.
#'   Defaults to 2.
#' @param smallThreshold Positive numeric threshold below which odds are shown
#'   as `1:N`. Defaults to 0.1.
#'
#' @return A character vector.
#'
#' @keywords internal
formatExplanationOdds = function(odds, sigDigits = 2, smallThreshold = 0.1) {

  validateExplanationSigDigits(sigDigits)
  validateExplanationNumericScalar(smallThreshold, "smallThreshold")

  if (smallThreshold <= 0) {
    stop("`smallThreshold` must be positive.", call. = FALSE)
  }

  if (!is.numeric(odds)) {
    stop("`odds` must be numeric.", call. = FALSE)
  }

  vapply(
    odds,
    formatExplanationOddsScalar,
    character(1),
    sigDigits = as.integer(sigDigits),
    smallThreshold = smallThreshold,
    USE.NAMES = FALSE
  )
}

#' Format a multiplicative change for student-facing explanation text
#'
#' @param multiplier Numeric vector.
#' @param sigDigits Number of significant figures to keep. Defaults to 2.
#'
#' @return A character vector.
#'
#' @keywords internal
formatExplanationMultiplier = function(multiplier, sigDigits = 2) {
  formatExplanationNumber(multiplier, sigDigits = sigDigits)
}

#' Format an anchor value for student-facing explanation text
#'
#' @param anchor Numeric vector.
#' @param sigDigits Number of significant figures to keep. Defaults to 2.
#'
#' @return A character vector.
#'
#' @keywords internal
formatExplanationAnchor = function(anchor, sigDigits = 2) {
  formatExplanationNumber(anchor, sigDigits = sigDigits)
}

#' Format a named explanation quantity
#'
#' Routes common explanation quantities through the deterministic formatting
#' helper that matches their interpretation scale.
#'
#' @param value Numeric vector.
#' @param quantityType Character scalar. One of `number`, `probability`, `odds`,
#'   `multiplier`, or `anchor`.
#' @param sigDigits Number of significant figures to keep where applicable.
#'
#' @return A character vector.
#'
#' @keywords internal
formatExplanationQuantity = function(
    value,
    quantityType = c("number", "probability", "odds", "multiplier", "anchor"),
    sigDigits = 2) {

  quantityType = match.arg(quantityType)

  switch(
    quantityType,
    number = formatExplanationNumber(value, sigDigits = sigDigits),
    probability = formatExplanationProbability(value, sigDigits = sigDigits),
    odds = formatExplanationOdds(value, sigDigits = sigDigits),
    multiplier = formatExplanationMultiplier(value, sigDigits = sigDigits),
    anchor = formatExplanationAnchor(value, sigDigits = sigDigits)
  )
}

formatExplanationNumberScalar = function(x, sigDigits = 2) {

  if (is.na(x)) {
    return(NA_character_)
  }

  if (!is.finite(x)) {
    return(as.character(x))
  }

  roundedValue = signif(x, sigDigits)

  if (isTRUE(all.equal(roundedValue, round(roundedValue)))) {
    return(format(round(roundedValue), trim = TRUE, scientific = FALSE))
  }

  out = format(roundedValue, trim = TRUE, scientific = FALSE)

  if (grepl("\\.", out)) {
    out = sub("0+$", "", out)
    out = sub("\\.$", "", out)
  }

  out
}

formatExplanationProbabilityScalar = function(probability, asPercent = TRUE, sigDigits = 2) {

  if (is.na(probability)) {
    return(NA_character_)
  }

  if (!is.finite(probability) || probability < 0 || probability > 1) {
    stop("`probability` values must be between 0 and 1.", call. = FALSE)
  }

  if (!isTRUE(asPercent)) {
    return(formatExplanationNumber(probability, sigDigits = sigDigits))
  }

  percentValue = probability * 100

  if (probability > 0 && percentValue < 1) {
    return("<1%")
  }

  if (probability < 1 && percentValue > 99) {
    return(">99%")
  }

  paste0(round(percentValue), "%")
}

formatExplanationOddsScalar = function(odds, sigDigits = 2, smallThreshold = 0.1) {

  if (is.na(odds)) {
    return(NA_character_)
  }

  if (!is.finite(odds) || odds < 0) {
    stop("`odds` values must be non-negative and finite.", call. = FALSE)
  }

  if (odds == 0) {
    return("0:1")
  }

  if (odds < smallThreshold) {
    denominator = round(1 / odds)
    denominatorText = format(denominator, trim = TRUE, scientific = FALSE)
    return(paste0("1:", denominatorText))
  }

  paste0(formatExplanationNumber(odds, sigDigits = sigDigits), ":1")
}

validateExplanationNumericScalar = function(x, name) {

  if (!is.numeric(x) || length(x) != 1 || is.na(x)) {
    stop("`", name, "` must be a single non-missing numeric value.", call. = FALSE)
  }

  invisible(TRUE)
}

validateExplanationSigDigits = function(sigDigits) {

  if (!is.numeric(sigDigits) || length(sigDigits) != 1 || is.na(sigDigits)) {
    stop("`sigDigits` must be a single non-missing numeric value.", call. = FALSE)
  }

  sigDigits = as.integer(sigDigits)

  if (sigDigits < 1) {
    stop("`sigDigits` must be at least 1.", call. = FALSE)
  }

  invisible(TRUE)
}
