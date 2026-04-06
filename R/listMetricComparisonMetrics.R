#' List metrics that can be compared in wmfmScores objects
#'
#' Internal helper used by comparison and diagnosis workflows.
#'
#' @param scores A \code{wmfmScores} object.
#'
#' @return A character vector of metric names present in both deterministic and
#'   LLM score records and having at least one non-missing numeric value on
#'   each side.
#'
#' @keywords internal
listMetricComparisonMetrics = function(scores) {
  if (!inherits(scores, "wmfmScores")) {
    stop("scores must be a wmfmScores object.")
  }

  if (is.null(scores$scores) || !is.list(scores$scores)) {
    stop("scores$scores must exist and be a list.")
  }

  detList = scores$scores$deterministic
  llmList = scores$scores$llm

  if (is.null(detList) || is.null(llmList)) {
    stop(
      "Both deterministic and llm scores must be present in scores$scores ",
      "to compare metrics."
    )
  }

  detNames = unique(unlist(lapply(detList, names), use.names = FALSE))
  llmNames = unique(unlist(lapply(llmList, names), use.names = FALSE))
  metricNames = intersect(detNames, llmNames)

  excluded = c(
    "llmScored",
    "llmScoringModel",
    "llmScoringRaw",
    "llmScoringSummary",
    "primaryScoringMethod",
    "interactionEvidenceAppropriate"
  )

  metricNames = setdiff(metricNames, excluded)
  metricNames = metricNames[!is.na(metricNames) & nzchar(metricNames)]

  extractMetricValues = function(runList, metricName) {
    vapply(
      runList,
      function(x) {
        if (!is.list(x) || is.null(names(x)) || !(metricName %in% names(x))) {
          return(NA_character_)
        }

        as.character(x[[metricName]])
      },
      FUN.VALUE = character(1)
    )
  }

  hasUsableNumericValues = function(metricName) {
    detVals = suppressWarnings(as.numeric(extractMetricValues(detList, metricName)))
    llmVals = suppressWarnings(as.numeric(extractMetricValues(llmList, metricName)))

    any(!is.na(detVals)) && any(!is.na(llmVals))
  }

  metricNames = metricNames[vapply(metricNames, hasUsableNumericValues, logical(1))]
  sort(metricNames)
}
