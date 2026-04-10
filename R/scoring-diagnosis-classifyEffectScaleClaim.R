#' Classify the effect scale described in a WMFM explanation
#'
#' Uses simple text heuristics to classify the scale on which the explanation
#' describes the main effect. The intent is to identify the dominant scale used
#' for coefficient interpretation, rather than unrelated numeric summaries such
#' as model-fit percentages.
#'
#' The classifier is deliberately conservative about assigning
#' \code{"mixed_or_unclear"}. In particular, generic percentage language such as
#' an \eqn{R^2} statement should not by itself force a multiplicative label when
#' the coefficient interpretation is otherwise clearly additive.
#'
#' @param text Character scalar explanation text.
#'
#' @return One of \code{"additive"}, \code{"multiplicative"},
#'   \code{"probability_or_odds"}, \code{"mixed_or_unclear"}, or
#'   \code{"not_stated"}.
#' @export
classifyEffectScaleClaim = function(text) {

  detectPatternLocal = function(text, pattern) {
    if (is.na(text) || !nzchar(trimws(text))) {
      return(FALSE)
    }

    grepl(pattern, text, ignore.case = TRUE, perl = TRUE)
  }

  if (length(text) == 0) {
    return("not_stated")
  }

  text = as.character(text)[1]

  if (is.na(text) || !nzchar(trimws(text))) {
    return("not_stated")
  }

  additivePattern = paste(
    "\\bpoint(s)?\\b",
    "\\bunit(s)?\\b",
    "\\bhigher by\\b",
    "\\blower by\\b",
    "\\braises? .* by\\b",
    "\\bincrease(s|d)? .* by\\b",
    "\\bdecrease(s|d)? .* by\\b",
    "\\bboost(s|ed)? .* by\\b",
    "\\bassociated with .* points? higher\\b",
    "\\bassociated with .* points? lower\\b",
    "\\babout .* points? higher\\b",
    "\\babout .* points? lower\\b",
    "\\beach (extra|additional) point\\b",
    "\\bfor each (extra|additional|one|1)[ -]?unit\\b",
    "\\bfor each (extra|additional) point\\b",
    "\\bdifference of\\b",
    "\\bincrease of\\b",
    "\\bdecrease of\\b",
    sep = "|"
  )

  multiplicativePattern = paste(
    "\\bmultipl(?:y|ies|ied|ier|iers)?\\b",
    "\\bmultiplier\\b",
    "\\btimes\\b",
    "\\bfold\\b",
    "\\brate ratio\\b",
    "\\bodds ratio\\b",
    "\\brelative risk\\b",
    "\\brisk ratio\\b",
    "\\bincidence rate ratio\\b",
    "\\bchanges? by .*%\\b",
    "\\bincrease(s|d)? by .*%\\b",
    "\\bdecrease(s|d)? by .*%\\b",
    sep = "|"
  )

  probabilityPattern = paste(
    "\\bodds\\b",
    "\\bprobability\\b",
    "\\bchance\\b",
    "\\blikelihood\\b",
    sep = "|"
  )

  additiveDetected = detectPatternLocal(text, additivePattern)
  multiplicativeDetected = detectPatternLocal(text, multiplicativePattern)
  probabilityDetected = detectPatternLocal(text, probabilityPattern)

  nDetected = sum(c(additiveDetected, multiplicativeDetected, probabilityDetected))

  if (nDetected > 1) {
    return("mixed_or_unclear")
  }

  if (additiveDetected) {
    return("additive")
  }

  if (multiplicativeDetected) {
    return("multiplicative")
  }

  if (probabilityDetected) {
    return("probability_or_odds")
  }

  "not_stated"
}
