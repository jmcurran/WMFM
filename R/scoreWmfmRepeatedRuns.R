#' Score repeated WMFM explanation runs
#'
#' Applies a simple rubric to the explanation outputs produced by
#' `runWMFMPackageExampleRepeated()`. This is intended as a lightweight,
#' rule-based quality check for repeated model explanations.
#'
#' The scoring is deliberately simple and transparent. Each run is scored on:
#' \describe{
#'   \item{hasExplanationScore}{Whether an explanation was returned}
#'   \item{hasEquationScore}{Whether equations were returned}
#'   \item{mentionsCiScore}{Whether confidence interval language is mentioned}
#'   \item{percentLanguageScore}{Whether percentage language is used}
#'   \item{referenceLanguageScore}{Whether baseline or reference language is used}
#'   \item{interactionLanguageScore}{Whether interaction language is used}
#'   \item{lengthScore}{Whether the explanation length falls in a preferred range}
#'   \item{errorPenalty}{Penalty applied if the run produced an error}
#' }
#'
#' The function then computes a `totalScore` by summing the component scores.
#'
#' This function does not attempt to verify deep statistical correctness.
#' Instead, it provides a quick first-pass screen for consistency and
#' presence of important explanation features. It is most useful when
#' comparing many repeated runs of the same model explanation.
#'
#' @param runsDf A data.frame produced by `runWMFMPackageExampleRepeated()`.
#' @param preferredMinWords Integer. Lower bound for preferred explanation
#'   length in words.
#' @param preferredMaxWords Integer. Upper bound for preferred explanation
#'   length in words.
#' @param requireCi Logical. If `TRUE`, runs are rewarded for mentioning
#'   confidence intervals.
#' @param preferPercent Logical. If `TRUE`, runs are rewarded for using
#'   percentage language.
#' @param requireReference Logical. If `TRUE`, runs are rewarded for using
#'   baseline or reference language.
#' @param requireInteraction Logical. If `TRUE`, runs are rewarded for using
#'   interaction language.
#'
#' @return A data.frame equal to `runsDf` with additional scoring columns.
#'
#' @examples
#' \dontrun{
#' repeated = runWMFMPackageExampleRepeated(
#'   name = "Course",
#'   package = "WMFM",
#'   nRuns = 10
#' )
#'
#' scored = scoreWmfmRepeatedRuns(repeated$runsDf)
#'
#' scored[, c(
#'   "runId",
#'   "totalScore",
#'   "hasExplanationScore",
#'   "mentionsCiScore",
#'   "percentLanguageScore",
#'   "errorPenalty"
#' )]
#' }
#'
#' @export
scoreWmfmRepeatedRuns = function(
    runsDf,
    preferredMinWords = 80L,
    preferredMaxWords = 220L,
    requireCi = TRUE,
    preferPercent = TRUE,
    requireReference = FALSE,
    requireInteraction = FALSE
) {
  requiredColumns = c(
    "equationsText",
    "explanationText",
    "wordCount",
    "mentionsCi",
    "usesPercentLanguage",
    "mentionsReferenceGroup",
    "mentionsInteraction",
    "hasError"
  )

  missingColumns = setdiff(requiredColumns, names(runsDf))

  if (length(missingColumns) > 0) {
    stop(
      "runsDf is missing required columns: ",
      paste(missingColumns, collapse = ", "),
      call. = FALSE
    )
  }

  hasExplanationScore = ifelse(
    !is.na(runsDf$explanationText) & nzchar(trimws(runsDf$explanationText)),
    1L,
    0L
  )

  hasEquationScore = ifelse(
    !is.na(runsDf$equationsText) & nzchar(trimws(runsDf$equationsText)),
    1L,
    0L
  )

  mentionsCiScore = if (isTRUE(requireCi)) {
    ifelse(isTRUE(runsDf$mentionsCi) | runsDf$mentionsCi, 1L, 0L)
  } else {
    rep(0L, nrow(runsDf))
  }

  percentLanguageScore = if (isTRUE(preferPercent)) {
    ifelse(isTRUE(runsDf$usesPercentLanguage) | runsDf$usesPercentLanguage, 1L, 0L)
  } else {
    rep(0L, nrow(runsDf))
  }

  referenceLanguageScore = if (isTRUE(requireReference)) {
    ifelse(
      isTRUE(runsDf$mentionsReferenceGroup) | runsDf$mentionsReferenceGroup,
      1L,
      0L
    )
  } else {
    rep(0L, nrow(runsDf))
  }

  interactionLanguageScore = if (isTRUE(requireInteraction)) {
    ifelse(
      isTRUE(runsDf$mentionsInteraction) | runsDf$mentionsInteraction,
      1L,
      0L
    )
  } else {
    rep(0L, nrow(runsDf))
  }

  lengthScore = ifelse(
    !is.na(runsDf$wordCount) &
      runsDf$wordCount >= preferredMinWords &
      runsDf$wordCount <= preferredMaxWords,
    1L,
    0L
  )

  errorPenalty = ifelse(isTRUE(runsDf$hasError) | runsDf$hasError, -2L, 0L)

  totalScore =
    hasExplanationScore +
    hasEquationScore +
    mentionsCiScore +
    percentLanguageScore +
    referenceLanguageScore +
    interactionLanguageScore +
    lengthScore +
    errorPenalty

  scoredDf = runsDf

  scoredDf$hasExplanationScore = hasExplanationScore
  scoredDf$hasEquationScore = hasEquationScore
  scoredDf$mentionsCiScore = mentionsCiScore
  scoredDf$percentLanguageScore = percentLanguageScore
  scoredDf$referenceLanguageScore = referenceLanguageScore
  scoredDf$interactionLanguageScore = interactionLanguageScore
  scoredDf$lengthScore = lengthScore
  scoredDf$errorPenalty = errorPenalty
  scoredDf$totalScore = totalScore

  scoredDf
}
