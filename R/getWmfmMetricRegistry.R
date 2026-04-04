#' Return the WMFM metric registry
#'
#' Defines metric metadata used by WMFM score comparison, stability
#' summaries, and plotting. The registry is intended to be the single
#' source of truth for which metrics are available and how they should
#' be treated.
#'
#' The default registry below includes the metrics that are currently
#' known from the scoring framework. Add rows here as new metrics are
#' introduced so that downstream methods automatically pick them up.
#'
#' @return A data frame containing metric metadata.
#' @export
getWmfmMetricRegistry = function() {
  registry = data.frame(
    metricName = c(
      "overallScore",
      "clarityAdequate",
      "effectDirectionCorrect",
      "interactionDirectionCorrect",
      "uncertaintyAppropriate"
    ),
    metricType = c(
      "continuous",
      "ordinal",
      "ordinal",
      "ordinal",
      "binary"
    ),
    label = c(
      "Overall score",
      "Clarity adequate",
      "Effect direction correct",
      "Interaction direction correct",
      "Uncertainty appropriate"
    ),
    group = c(
      "overall",
      "communication",
      "interpretation",
      "interpretation",
      "inference"
    ),
    includeInComparison = c(
      TRUE,
      TRUE,
      TRUE,
      TRUE,
      TRUE
    ),
    includeInStability = c(
      TRUE,
      TRUE,
      TRUE,
      TRUE,
      TRUE
    ),
    includeInPlots = c(
      TRUE,
      TRUE,
      TRUE,
      TRUE,
      TRUE
    ),
    stringsAsFactors = FALSE
  )

  registry$orderedLevels = I(list(
    NULL,
    c("inadequate", "mixed_or_unclear", "adequate"),
    c("incorrect", "mixed_or_unclear", "correct"),
    c("incorrect", "mixed_or_unclear", "correct"),
    NULL
  ))

  validateWmfmMetricRegistry(registry)

  registry
}
