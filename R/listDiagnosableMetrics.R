#' List metrics that can be diagnosed from a wmfmScores object
#'
#' Internal helper for \code{diagnose()}.
#'
#' @param scores A \code{wmfmScores} object.
#'
#' @return A character vector of metric names present in both deterministic and
#'   LLM score records.
#'
#' @keywords internal
listDiagnosableMetrics = function(scores) {
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
      "to diagnose disagreement."
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
    "primaryScoringMethod"
  )

  metricNames = setdiff(metricNames, excluded)
  metricNames = metricNames[!is.na(metricNames) & nzchar(metricNames)]
  sort(metricNames)
}
