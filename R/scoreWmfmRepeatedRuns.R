#' Score repeated WMFM explanation runs
#'
#' Applies an explanation-focused scoring rubric to outputs produced by
#' repeated WMFM runs.
#'
#' This function is tolerant of different input shapes. `runsDf` may be either:
#' \itemize{
#'   \item a data.frame of run records, or
#'   \item a list returned by `runWMFMPackageExampleRepeated()` containing a
#'   `runsDf` element.
#' }
#'
#' If derived columns such as `wordCount` or `hasError` are not present, they
#' are computed from available fields where possible.
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
#' @param runsDf A data.frame of repeated-run outputs, or a list containing
#'   a `runsDf` element.
#' @param preferredMinWords Integer. Lower bound for preferred explanation
#'   length in words.
#' @param preferredMaxWords Integer. Upper bound for preferred explanation
#'   length in words.
#' @param requireCi Logical. If `TRUE`, runs are rewarded for mentioning
#'   confidence intervals when the `mentionsCi` column is available.
#' @param preferPercent Logical. If `TRUE`, runs are rewarded for using
#'   percentage language when the `usesPercentLanguage` column is available.
#' @param requireReference Logical. If `TRUE`, runs are rewarded for using
#'   reference or baseline language when the `mentionsReferenceGroup` column
#'   is available.
#' @param requireInteraction Logical. If `TRUE`, runs are rewarded for using
#'   interaction language when the `mentionsInteraction` column is available.
#' @param penaliseDuplicates Logical. Should exact duplicate explanations
#'   receive a penalty?
#' @param duplicatePenalty Integer. Penalty applied to runs whose explanation
#'   appears more than once.
#'
#' @return A data.frame equal to the run records with additional scoring
#'   columns.
#' @export
scoreWmfmRepeatedRuns = function(
    runsDf,
    preferredMinWords = 80L,
    preferredMaxWords = 220L,
    requireCi = TRUE,
    preferPercent = TRUE,
    requireReference = FALSE,
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

  runsDf = extractRunsDf(runsDf)

  if (!is.numeric(preferredMinWords) || length(preferredMinWords) != 1 || is.na(preferredMinWords)) {
    stop("`preferredMinWords` must be a single number.", call. = FALSE)
  }

  if (!is.numeric(preferredMaxWords) || length(preferredMaxWords) != 1 || is.na(preferredMaxWords)) {
    stop("`preferredMaxWords` must be a single number.", call. = FALSE)
  }

  if (preferredMinWords > preferredMaxWords) {
    stop("`preferredMinWords` must be less than or equal to `preferredMaxWords`.", call. = FALSE)
  }

  if (!is.logical(penaliseDuplicates) || length(penaliseDuplicates) != 1 || is.na(penaliseDuplicates)) {
    stop("`penaliseDuplicates` must be TRUE or FALSE.", call. = FALSE)
  }

  if (!is.numeric(duplicatePenalty) || length(duplicatePenalty) != 1 || is.na(duplicatePenalty)) {
    stop("`duplicatePenalty` must be a single number.", call. = FALSE)
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

  countWords = function(x) {
    x = trimws(x)

    if (!nzchar(x)) {
      return(0L)
    }

    parts = strsplit(x, "[[:space:]]+")[[1]]
    length(parts)
  }

  getWordCount = function(df, explanationText) {
    if ("wordCount" %in% names(df)) {
      out = suppressWarnings(as.integer(df$wordCount))
      missingIdx = which(is.na(out))

      if (length(missingIdx) > 0) {
        out[missingIdx] = vapply(explanationText[missingIdx], countWords, integer(1))
      }

      return(out)
    }

    vapply(explanationText, countWords, integer(1))
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

  explanationText = getExplanationText(runsDf)
  wordCount = getWordCount(runsDf, explanationText)
  hasError = getHasError(runsDf)

  explanationPresent = nzchar(trimws(explanationText))
  hasExplanationScore = ifelse(explanationPresent, 1L, 0L)

  mentionsCi = getLogicalColumn(runsDf, "mentionsCi")
  usesPercentLanguage = getLogicalColumn(runsDf, "usesPercentLanguage")
  mentionsReferenceGroup = getLogicalColumn(runsDf, "mentionsReferenceGroup")
  mentionsInteraction = getLogicalColumn(runsDf, "mentionsInteraction")

  mentionsCiScore = if (isTRUE(requireCi) && "mentionsCi" %in% names(runsDf)) {
    ifelse(mentionsCi, 1L, 0L)
  } else {
    rep(0L, nrow(runsDf))
  }

  percentLanguageScore = if (isTRUE(preferPercent) && "usesPercentLanguage" %in% names(runsDf)) {
    ifelse(usesPercentLanguage, 1L, 0L)
  } else {
    rep(0L, nrow(runsDf))
  }

  referenceLanguageScore = if (isTRUE(requireReference) && "mentionsReferenceGroup" %in% names(runsDf)) {
    ifelse(mentionsReferenceGroup, 1L, 0L)
  } else {
    rep(0L, nrow(runsDf))
  }

  interactionLanguageScore = if (isTRUE(requireInteraction) && "mentionsInteraction" %in% names(runsDf)) {
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
    mentionsCiScore +
    percentLanguageScore +
    referenceLanguageScore +
    interactionLanguageScore +
    lengthScore +
    errorPenalty +
    duplicatePenaltyApplied

  scoredDf = runsDf
  scoredDf$explanationTextDerived = explanationText
  scoredDf$wordCountDerived = wordCount
  scoredDf$hasErrorDerived = hasError
  scoredDf$hasExplanationScore = hasExplanationScore
  scoredDf$mentionsCiScore = mentionsCiScore
  scoredDf$percentLanguageScore = percentLanguageScore
  scoredDf$referenceLanguageScore = referenceLanguageScore
  scoredDf$interactionLanguageScore = interactionLanguageScore
  scoredDf$lengthScore = lengthScore
  scoredDf$errorPenalty = errorPenalty
  scoredDf$duplicateCount = duplicateCount
  scoredDf$isExactDuplicate = isExactDuplicate
  scoredDf$duplicatePenalty = duplicatePenaltyApplied
  scoredDf$totalScore = totalScore

  scoredDf
}
