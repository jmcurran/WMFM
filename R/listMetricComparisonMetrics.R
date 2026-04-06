#' List metrics that can be diagnosed from a wmfmScores object
#'
#' Internal helper for \code{diagnose()}.
#'
#' @param scores A \code{wmfmScores} object.
#'
#' @return A character vector of metric names present in both deterministic and
#'   LLM score records and having at least one non-missing numeric value on
#'   each side.
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

  hasUsableNumericValues = function(metricName) {
    detVals = suppressWarnings(as.numeric(vapply(
      detList,
      function(x) {
        if (metricName %in% names(x)) {
          x[[metricName]]
        } else {
          NA_real_
        }
      },
      FUN.VALUE = numeric(1)
    )))

    llmVals = suppressWarnings(as.numeric(vapply(
      llmList,
      function(x) {
        if (metricName %in% names(x)) {
          x[[metricName]]
        } else {
          NA_real_
        }
      },
      FUN.VALUE = numeric(1)
    )))

    any(!is.na(detVals)) && any(!is.na(llmVals))
  }

  metricNames = metricNames[vapply(metricNames, hasUsableNumericValues, logical(1))]
  sort(metricNames)
}
