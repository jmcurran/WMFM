#' Classify run-level score disagreement for heatmap plotting
#'
#' Converts paired run-level values from `buildWmfmComparisonPairs()` into a
#' categorical disagreement label that can be used for a heatmap.
#'
#' Classification rules are:
#' \itemize{
#'   \item binary metrics: `"exact"` or `"different"`;
#'   \item ordinal metrics: `"exact"`, `"adjacent"`, `"moderate"`, or `"large"`;
#'   \item continuous metrics: excluded by default in the heatmap and returned
#'   as `NA` unless `includeContinuous = TRUE`, in which case they are binned as
#'   `"exact"`, `"small"`, `"moderate"`, or `"large"` using absolute
#'   differences.
#' }
#'
#' For ordinal metrics, ordered levels are taken from the registry stored on the
#' comparison object.
#'
#' @param x A `wmfmScoreComparison` object.
#' @param includeContinuous Logical. Should continuous metrics be classified?
#' @param continuousBreaks Numeric vector of length 3 giving cut points for the
#'   absolute difference bins used for continuous metrics.
#'
#' @return A data frame like `x$pairData` with an added
#'   `disagreementClass` column.
#' @keywords internal
classifyWmfmScoreDisagreement = function(
    x,
    includeContinuous = FALSE,
    continuousBreaks = c(0.10, 0.35, 0.75)
) {
  if (!inherits(x, "wmfmScoreComparison")) {
    stop("`x` must inherit from `wmfmScoreComparison`.", call. = FALSE)
  }

  pairData = x$pairData

  if (!is.data.frame(pairData) || nrow(pairData) == 0) {
    return(data.frame())
  }

  registry = x$registry
  orderedLookup = stats::setNames(registry$orderedLevels, registry$metricName)

  classifyOne = function(metric, metricType, leftValue, rightValue) {
    if (is.na(leftValue) || is.na(rightValue)) {
      return(NA_character_)
    }

    if (identical(metricType, "binary")) {
      return(if (identical(as.logical(leftValue), as.logical(rightValue))) "exact" else "different")
    }

    if (identical(metricType, "ordinal")) {
      levels = orderedLookup[[metric]]
      left = match(as.character(leftValue), levels)
      right = match(as.character(rightValue), levels)

      if (is.na(left) || is.na(right)) {
        return(NA_character_)
      }

      absDiff = abs(right - left)

      if (absDiff == 0) {
        return("exact")
      }

      if (absDiff == 1) {
        return("adjacent")
      }

      if (absDiff == 2) {
        return("moderate")
      }

      return("large")
    }

    if (!isTRUE(includeContinuous)) {
      return(NA_character_)
    }

    absDiff = abs(as.numeric(rightValue) - as.numeric(leftValue))

    if (is.na(absDiff)) {
      return(NA_character_)
    }

    if (absDiff <= continuousBreaks[1]) {
      return("exact")
    }

    if (absDiff <= continuousBreaks[2]) {
      return("small")
    }

    if (absDiff <= continuousBreaks[3]) {
      return("moderate")
    }

    "large"
  }

  pairData$disagreementClass = vapply(
    seq_len(nrow(pairData)),
    FUN = function(i) {
      classifyOne(
        metric = pairData$metric[i],
        metricType = pairData$metricType[i],
        leftValue = pairData$leftValue[i],
        rightValue = pairData$rightValue[i]
      )
    },
    FUN.VALUE = character(1)
  )

  pairData
}
