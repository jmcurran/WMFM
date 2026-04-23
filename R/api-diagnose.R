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
#' Diagnoses disagreement between deterministic and LLM scoring for either a
#' single metric or all available metrics.
#'
#' When \code{metric} is supplied, the function returns a
#' \code{wmfmMetricDiagnosis} object. When \code{metric = NULL}, it returns a
#' \code{wmfmScoresDiagnosis} object summarising all metrics that can be
#' compared.
#'
#' @param x A \code{wmfmScores} object.
#' @param metric Optional single metric name. If \code{NULL}, diagnose all
#'   comparable metrics.
#' @param runs Optional \code{wmfmRuns} object or compatible run container used
#'   to provide explanation text and claim-level context.
#' @param cmp Optional \code{wmfmScoreComparison} object.
#' @param maxExamples Maximum number of flagged disagreement examples to retain
#'   for each metric diagnosis.
#' @param ... Reserved for future method-specific arguments.
#'
#' @return A \code{wmfmMetricDiagnosis} or \code{wmfmScoresDiagnosis} object.
#' @export
diagnose.wmfmScores = function(x,
                               metric = NULL,
                               runs = NULL,
                               cmp = NULL,
                               maxExamples = 5,
                               ...) {
  if (!is.null(metric)) {
    if (!is.character(metric) || length(metric) != 1L || is.na(metric)) {
      stop("metric must be NULL or a single non-missing character string.")
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
    evidenceSummary = buildMetricEvidenceSummary(metricDf = metricDf, metric = metric)
    flaggedRuns = selectFlaggedMetricRuns(metricDf = metricDf, maxExamples = maxExamples)

    out = list(
      metric = metric,
      summary = summaryDf,
      evidenceSummary = evidenceSummary,
      runDiagnosis = metricDf,
      flaggedRuns = flaggedRuns,
      call = match.call()
    )

    class(out) = "wmfmMetricDiagnosis"
    return(out)
  }

  metricNames = listDiagnosableMetrics(x)

  metricDiagnoses = stats::setNames(
    lapply(metricNames, function(metricName) {
      diagnose.wmfmScores(
        x = x,
        metric = metricName,
        runs = runs,
        cmp = NULL,
        maxExamples = maxExamples,
        ...
      )
    }),
    metricNames
  )

  summaryTable = do.call(
    rbind,
    lapply(metricDiagnoses, function(dx) {
      summaryDf = dx$summary
      evidenceDf = dx$evidenceSummary

      out = summaryDf

      if (!is.null(evidenceDf) &&
          is.data.frame(evidenceDf) &&
          nrow(evidenceDf) > 0L &&
          "likelyIssue" %in% names(evidenceDf)) {
        out$likelyIssue = evidenceDf$likelyIssue[[1L]]
      } else {
        out$likelyIssue = NA_character_
      }

      out
    })
  )

  summaryTable$priorityScore = computeDiagnosisPriorityScore(summaryTable)

  flaggedMetrics = summaryTable[
    order(
      -summaryTable$priorityScore,
      -summaryTable$disagreementRate,
      -abs(summaryTable$meanLlmMinusDet)
    ),
    ,
    drop = FALSE
  ]

  out = list(
    summaryTable = summaryTable,
    flaggedMetrics = flaggedMetrics,
    metricDiagnoses = metricDiagnoses,
    call = match.call()
  )

  class(out) = "wmfmScoresDiagnosis"
  out
}

#' @export
diagnose.default = function(x, ...) {
  stop("No diagnose() method for objects of class: ", paste(class(x), collapse = ", "))
}
