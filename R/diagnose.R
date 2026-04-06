#' Diagnose disagreement for a single scoring metric
#'
#' Diagnoses systematic disagreement between deterministic and LLM scoring
#' for a single metric. The function summarises the disagreement pattern,
#' classifies the likely source of disagreement using simple heuristics,
#' and returns run-level records for inspection.
#'
#' This function is intended to sit downstream of \code{score()} and
#' complements \code{compare()} by focusing on explanation rather than
#' only measurement.
#'
#' @param x A \code{wmfmScores} object.
#' @param metric A single metric name.
#' @param cmp Optional \code{wmfmScoreComparison} object. If supplied,
#'   run-level paired score data may be taken from this object.
#' @param maxExamples Maximum number of flagged disagreement examples to
#'   retain in the \code{flaggedRuns} component.
#' @param ... Unused.
#'
#' @return An object of class \code{wmfmMetricDiagnosis} with elements:
#' \describe{
#'   \item{metric}{Metric name.}
#'   \item{summary}{One-row data frame summarising disagreement pattern.}
#'   \item{runDiagnosis}{Run-level data frame including scores, differences,
#'   disagreement indicators, explanation text, and any available scoring
#'   detail columns.}
#'   \item{flaggedRuns}{Subset of \code{runDiagnosis} containing the most
#'   informative disagreement cases.}
#'   \item{call}{Matched call.}
#' }
#'
#' @examples
#' \dontrun{
#' repeated = runExample("Course", nRuns = 20)
#' scores = score(repeated, method = "both")
#' cmp = compare(scores)
#'
#' dx = diagnose(scores, metric = "numericExpressionAdequate", cmp = cmp)
#' print(dx)
#' }
#'
#' @export
diagnose = function(x, ...) {
  UseMethod("diagnose")
}

#' @export
diagnose.wmfmScores = function(x,
                               metric,
                               cmp = NULL,
                               maxExamples = 5,
                               ...) {
  if (!is.character(metric) || length(metric) != 1L || is.na(metric)) {
    stop("metric must be a single non-missing character string.")
  }

  if (!is.null(cmp) && !inherits(cmp, "wmfmScoreComparison")) {
    stop("cmp must be NULL or a wmfmScoreComparison object.")
  }

  metricDf = resolveMetricDiagnosisData(scores = x, metric = metric, cmp = cmp)
  summaryDf = buildMetricDiagnosisSummary(metricDf = metricDf, metric = metric)
  summaryDf$diagnosisClass = classifyMetricDisagreement(summaryDf)
  flaggedRuns = selectFlaggedMetricRuns(metricDf = metricDf, maxExamples = maxExamples)

  out = list(
    metric = metric,
    summary = summaryDf,
    runDiagnosis = metricDf,
    flaggedRuns = flaggedRuns,
    call = match.call()
  )

  class(out) = "wmfmMetricDiagnosis"
  out
}

#' @export
diagnose.default = function(x, ...) {
  stop("No diagnose() method for objects of class: ", paste(class(x), collapse = ", "))
}
