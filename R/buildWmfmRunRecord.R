#' Build a single run record
#'
#' Creates a structured record for one model run including
#' derived text features.
#'
#' @param runId Integer.
#' @param exampleName Character.
#' @param package Character.
#' @param modelType Character.
#' @param formula Character.
#' @param equationsText Character.
#' @param explanationText Character.
#' @param errorMessage Character.
#'
#' @return Named list.
#' @export
buildWmfmRunRecord = function(
    runId,
    exampleName,
    package,
    modelType,
    formula,
    equationsText,
    explanationText,
    errorMessage = NA_character_
) {

  list(
    runId = runId,
    timestamp = as.character(Sys.time()),
    exampleName = exampleName,
    package = package,
    modelType = modelType,
    formula = formula,
    equationsText = equationsText,
    explanationText = explanationText,
    normalizedExplanation = normalizeWmfmText(explanationText),
    wordCount = countWmfmWords(explanationText),
    sentenceCount = countWmfmSentences(explanationText),
    mentionsCi = detectWmfmPattern(explanationText, "confidence interval|95%"),
    usesPercentLanguage = detectWmfmPattern(explanationText, "percent|%"),
    mentionsInteraction = detectWmfmPattern(explanationText, "interaction"),
    hasError = !is.na(errorMessage),
    errorMessage = errorMessage
  )
}
