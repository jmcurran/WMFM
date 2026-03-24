#' Score repeated WMFM explanation runs
#'
#' Applies an explanation-focused scoring rubric to repeated WMFM runs.
#'
#' This function accepts either a data.frame of run records or a list returned
#' by `runWMFMPackageExampleRepeated()`.
#'
#' The score focuses on:
#' \itemize{
#'   \item whether an explanation was returned
#'   \item whether the run completed without error
#'   \item whether the explanation length is within a preferred range
#'   \item whether optional explanation features are present
#'   \item whether the explanation is an exact duplicate of another run
#' }
#'
#' Duplicate detection is based on `normalizedExplanation` when available,
#' otherwise on trimmed explanation text.
#'
#' @param runsDf A data.frame of repeated-run outputs, or a list containing a
#'   `runsDf` element.
#' @param preferredMinWords Integer. Lower bound for preferred explanation
#'   length in words.
#' @param preferredMaxWords Integer. Upper bound for preferred explanation
#'   length in words.
#' @param requireConfidenceInterval Logical. If `TRUE`, runs are rewarded for
#'   mentioning confidence intervals.
#' @param preferPercentLanguage Logical. If `TRUE`, runs are rewarded for
#'   using percentage language.
#' @param requireReferenceGroup Logical. If `TRUE`, runs are rewarded for
#'   reference-group language.
#' @param requireInteraction Logical. If `TRUE`, runs are rewarded for
#'   interaction language.
#' @param penaliseDuplicates Logical. Should exact duplicate explanations
#'   receive a penalty?
#' @param duplicatePenalty Integer. Penalty applied to runs whose explanation
#'   appears more than once.
#'
#' @return A data.frame with additional scoring columns.
#' @export
scoreWmfmRepeatedRuns = function(
    runsDf,
    preferredMinWords = 80L,
    preferredMaxWords = 220L,
    requireConfidenceInterval = TRUE,
    preferPercentLanguage = TRUE,
    requireReferenceGroup = FALSE,
    requireInteraction = FALSE,
    penaliseDuplicates = TRUE,
    duplicatePenalty = -1L
) {

  extractRunsDf = function(x) {
    if (is.data.frame(x)) {
      return(x)
    }

    if (is.list(x) && "runsDf" %in% names(x) && is.data.frame(x$runsDf)) {
      return(x$runsDf)
    }

    stop(
      "`runsDf` must be a data.frame or a list containing a data.frame named `runsDf`.",
      call. = FALSE
    )
  }

  getExplanationText = function(df) {
    if ("explanationText" %in% names(df)) {
      x = df$explanationText
    } else if ("explanation" %in% names(df)) {
      x = df$explanation
    } else {
      stop(
        "Run records must contain either `explanationText` or `explanation`.",
        call. = FALSE
      )
    }

    x = as.character(x)
    x[is.na(x)] = ""
    x
  }

  countWordsLocal = function(x) {
    x = trimws(x)

    if (!nzchar(x)) {
      return(0L)
    }

    length(strsplit(x, "[[:space:]]+")[[1]])
  }

  getWordCount = function(df, explanationText) {
    if ("wordCount" %in% names(df)) {
      out = suppressWarnings(as.integer(df$wordCount))
      missingIdx = which(is.na(out))

      if (length(missingIdx) > 0) {
        out[missingIdx] = vapply(explanationText[missingIdx], countWordsLocal, integer(1))
      }

      return(out)
    }

    vapply(explanationText, countWordsLocal, integer(1))
  }

  getHasError = function(df) {
    if ("hasError" %in% names(df)) {
      out = as.logical(df$hasError)
      out[is.na(out)] = FALSE
      return(out)
    }

    if ("errorMessage" %in% names(df)) {
      msg = as.character(df$errorMessage)
      msg[is.na(msg)] = ""
      return(nzchar(trimws(msg)))
    }

    rep(FALSE, nrow(df))
  }

  getLogicalColumn = function(df, columnName) {
    if (!(columnName %in% names(df))) {
      return(rep(FALSE, nrow(df)))
    }

    out = as.logical(df[[columnName]])
    out[is.na(out)] = FALSE
    out
  }

  getExplanationKey = function(df, explanationText) {
    if ("normalizedExplanation" %in% names(df)) {
      key = as.character(df$normalizedExplanation)
      key[is.na(key)] = ""
      return(trimws(key))
    }

    trimws(explanationText)
  }

  runsDf = extractRunsDf(runsDf)
  explanationText = getExplanationText(runsDf)
  wordCount = getWordCount(runsDf, explanationText)
  hasError = getHasError(runsDf)

  explanationPresent = nzchar(trimws(explanationText))
  hasExplanationScore = ifelse(explanationPresent, 1L, 0L)

  mentionsConfidenceInterval = getLogicalColumn(runsDf, "mentionsConfidenceInterval")
  usesPercentLanguage = getLogicalColumn(runsDf, "usesPercentLanguage")
  mentionsReferenceGroup = getLogicalColumn(runsDf, "mentionsReferenceGroup")
  mentionsInteraction = getLogicalColumn(runsDf, "mentionsInteraction")

  mentionsConfidenceIntervalScore = if (isTRUE(requireConfidenceInterval) &&
                                        "mentionsConfidenceInterval" %in% names(runsDf)) {
    ifelse(mentionsConfidenceInterval, 1L, 0L)
  } else {
    rep(0L, nrow(runsDf))
  }

  usesPercentLanguageScore = if (isTRUE(preferPercentLanguage) &&
                                 "usesPercentLanguage" %in% names(runsDf)) {
    ifelse(usesPercentLanguage, 1L, 0L)
  } else {
    rep(0L, nrow(runsDf))
  }

  mentionsReferenceGroupScore = if (isTRUE(requireReferenceGroup) &&
                                    "mentionsReferenceGroup" %in% names(runsDf)) {
    ifelse(mentionsReferenceGroup, 1L, 0L)
  } else {
    rep(0L, nrow(runsDf))
  }

  mentionsInteractionScore = if (isTRUE(requireInteraction) &&
                                 "mentionsInteraction" %in% names(runsDf)) {
    ifelse(mentionsInteraction, 1L, 0L)
  } else {
    rep(0L, nrow(runsDf))
  }

  lengthScore = ifelse(
    wordCount >= preferredMinWords & wordCount <= preferredMaxWords,
    1L,
    0L
  )

  errorPenalty = ifelse(hasError, -2L, 0L)

  explanationKey = getExplanationKey(runsDf, explanationText)
  keyTable = table(explanationKey)
  duplicateCount = as.integer(keyTable[match(explanationKey, names(keyTable))])
  duplicateCount[!explanationPresent] = 0L

  isExactDuplicate = duplicateCount > 1L

  duplicatePenaltyApplied = if (isTRUE(penaliseDuplicates)) {
    ifelse(isExactDuplicate, as.integer(duplicatePenalty), 0L)
  } else {
    rep(0L, nrow(runsDf))
  }

  totalScore =
    hasExplanationScore +
    mentionsConfidenceIntervalScore +
    usesPercentLanguageScore +
    mentionsReferenceGroupScore +
    mentionsInteractionScore +
    lengthScore +
    errorPenalty +
    duplicatePenaltyApplied

  scoredDf = runsDf
  scoredDf$explanationTextDerived = explanationText
  scoredDf$wordCountDerived = wordCount
  scoredDf$hasErrorDerived = hasError
  scoredDf$hasExplanationScore = hasExplanationScore
  scoredDf$mentionsConfidenceIntervalScore = mentionsConfidenceIntervalScore
  scoredDf$usesPercentLanguageScore = usesPercentLanguageScore
  scoredDf$mentionsReferenceGroupScore = mentionsReferenceGroupScore
  scoredDf$mentionsInteractionScore = mentionsInteractionScore
  scoredDf$lengthScore = lengthScore
  scoredDf$errorPenalty = errorPenalty
  scoredDf$duplicateCount = duplicateCount
  scoredDf$isExactDuplicate = isExactDuplicate
  scoredDf$duplicatePenalty = duplicatePenaltyApplied
  scoredDf$totalScore = totalScore

  scoredDf
}
