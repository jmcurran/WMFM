#' Summarise a WMFM runs object
#'
#' Produces a concise summary of a raw `wmfmRuns` object. The summary focuses on
#' run-level variability, text metrics, timing, duplication, and extracted claim
#' frequencies. Judged fields and score summaries are intentionally excluded and
#' belong to `wmfmScores` objects instead.
#'
#' @param object A `wmfmRuns` object.
#' @param ... Reserved for future extensions.
#'
#' @return An object of class `summary.wmfmRuns`.
#' @export
summary.wmfmRuns = function(object, ...) {
  if (!inherits(object, "wmfmRuns")) {
    stop("`object` must inherit from `wmfmRuns`.", call. = FALSE)
  }

  if (!is.list(object$runs) || length(object$runs) == 0) {
    stop("`object$runs` must be a non-empty list of run records.", call. = FALSE)
  }

  runsDf = do.call(
    rbind,
    lapply(
      object$runs,
      function(run) {
        as.data.frame(run, stringsAsFactors = FALSE)
      }
    )
  )

  rownames(runsDf) = NULL

  nRuns = nrow(runsDf)

  if (!("normalizedExplanation" %in% names(runsDf))) {
    runsDf$normalizedExplanation = NA_character_
  }

  if (!("runElapsedSeconds" %in% names(runsDf))) {
    runsDf$runElapsedSeconds = NA_real_
  }

  if (!("wordCount" %in% names(runsDf))) {
    runsDf$wordCount = NA_real_
  }

  if (!("sentenceCount" %in% names(runsDf))) {
    runsDf$sentenceCount = NA_real_
  }

  normalizedExplanation = as.character(runsDf$normalizedExplanation)
  normalizedExplanation[is.na(normalizedExplanation)] = ""

  nonEmptyNormalized = normalizedExplanation[nzchar(normalizedExplanation)]
  nUniqueExplanations = length(unique(nonEmptyNormalized))

  duplicateCount = length(nonEmptyNormalized) - nUniqueExplanations
  duplicateRate =
    if (length(nonEmptyNormalized) == 0) {
      NA_real_
    } else {
      duplicateCount / length(nonEmptyNormalized)
    }

  claimSummary = getWmfmRunsClaimsData(object)
  claimSummary = claimSummary[order(-claimSummary$proportionPresent, claimSummary$claim), ]

  out = list(
    nRuns = nRuns,
    nUniqueExplanations = nUniqueExplanations,
    duplicateCount = duplicateCount,
    duplicateRate = duplicateRate,
    wordCount = list(
      mean = mean(as.numeric(runsDf$wordCount), na.rm = TRUE),
      min = min(as.numeric(runsDf$wordCount), na.rm = TRUE),
      max = max(as.numeric(runsDf$wordCount), na.rm = TRUE)
    ),
    sentenceCount = list(
      mean = mean(as.numeric(runsDf$sentenceCount), na.rm = TRUE),
      min = min(as.numeric(runsDf$sentenceCount), na.rm = TRUE),
      max = max(as.numeric(runsDf$sentenceCount), na.rm = TRUE)
    ),
    runElapsedSeconds = list(
      mean = mean(as.numeric(runsDf$runElapsedSeconds), na.rm = TRUE),
      min = min(as.numeric(runsDf$runElapsedSeconds), na.rm = TRUE),
      max = max(as.numeric(runsDf$runElapsedSeconds), na.rm = TRUE)
    ),
    claimSummary = claimSummary
  )

  class(out) = "summary.wmfmRuns"
  out
}
