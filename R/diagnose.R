#' Diagnose scoring disagreement
#'
#' S3 generic for diagnosing disagreement between deterministic and LLM
#' scoring outputs.
#'
#' @param x An object to diagnose.
#' @param ... Additional arguments passed to methods.
#'
#' @return Method-specific diagnosis output.
#' @export
diagnose = function(x, ...) {
  UseMethod("diagnose")
}

#' Diagnose disagreement for a WMFM scores object
#'
#' Diagnoses systematic disagreement between deterministic and LLM scoring
#' for a single metric. The function summarises disagreement patterns,
#' classifies the likely source of disagreement using simple heuristics,
#' and optionally attaches run-level context from the original
#' \code{wmfmRuns} object.
#'
#' @param x A \code{wmfmScores} object.
#' @param metric A single metric name.
#' @param runs Optional \code{wmfmRuns} object or compatible run container
#'   used to provide explanation text and claim-level context.
#' @param cmp Optional \code{wmfmScoreComparison} object.
#' @param maxExamples Maximum number of flagged disagreement examples to
#'   retain in the \code{flaggedRuns} component.
#' @param ... Reserved for future method-specific arguments.
#'
#' @return An object of class \code{wmfmMetricDiagnosis}.
#' @export
diagnose.wmfmScores = function(x,
                               metric,
                               runs = NULL,
                               cmp = NULL,
                               maxExamples = 5,
                               ...) {
  if (!is.character(metric) || length(metric) != 1L || is.na(metric)) {
    stop("metric must be a single non-missing character string.")
  }

  if (!is.null(cmp) && !inherits(cmp, "wmfmScoreComparison")) {
    stop("cmp must be NULL or a wmfmScoreComparison object.")
  }

  if (is.null(runs)) {
    warning(
      "No `runs` object supplied. Diagnosis can summarise score disagreement, ",
      "but will not include explanation text or claim-level context.",
      call. = FALSE
    )
  }

  metricDf = resolveMetricDiagnosisData(
    scores = x,
    metric = metric,
    cmp = cmp,
    runs = runs
  )

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
