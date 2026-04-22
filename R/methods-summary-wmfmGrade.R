#' Summarise a WMFM grade object
#'
#' Produces a compact summary for a `wmfmGrade` object. When repeated LLM
#' grading has been used, the summary includes run-to-run mark variability and
#' per-dimension ranges.
#'
#' @param object A `wmfmGrade` object.
#' @param method Optional character. One of `"deterministic"` or `"llm"`.
#' @param ... Unused.
#'
#' @return An object of class `summary.wmfmGrade`.
#' @export
summary.wmfmGrade = function(
    object,
    method = NULL,
    ...
) {

  if (!inherits(object, "wmfmGrade")) {
    stop("`object` must inherit from `wmfmGrade`.", call. = FALSE)
  }

  availableMethods = names(object$scores$byMethod %||% list())

  if (length(availableMethods) < 1) {
    stop("`object` has not been scored yet.", call. = FALSE)
  }

  if (is.null(method)) {
    method = object$meta$lastScoredMethod %||% availableMethods[1]
  } else {
    method = match.arg(method, c("deterministic", "llm"))
  }

  if (!method %in% availableMethods) {
    stop("No grade available for method `", method, "`.", call. = FALSE)
  }

  block = object$scores$byMethod[[method]]
  runs = block$runs %||% list()
  nRuns = length(runs)

  out = list(
    method = method,
    nRuns = if (nRuns > 0) nRuns else 1L,
    mark = NULL,
    overallScore = NULL,
    dimensionSummary = NULL,
    elapsedSeconds = block$elapsedSeconds %||% NA_real_,
    meanSecondsPerRun = block$meanSecondsPerRun %||% NA_real_,
    source = object
  )

  if (nRuns < 1) {
    markValue = suppressWarnings(as.numeric(block$mark))
    overallValue = suppressWarnings(as.numeric(block$overallScore))

    out$mark = list(
      mean = markValue,
      median = markValue,
      sd = NA_real_,
      min = markValue,
      max = markValue
    )
    out$overallScore = list(
      mean = overallValue,
      median = overallValue,
      sd = NA_real_,
      min = overallValue,
      max = overallValue
    )
  } else {
    markValues = vapply(runs, function(run) {
      suppressWarnings(as.numeric(run$mark)[1])
    }, numeric(1))
    overallValues = vapply(runs, function(run) {
      suppressWarnings(as.numeric(run$overallScore)[1])
    }, numeric(1))

    summariseVec = function(values) {
      list(
        mean = mean(values, na.rm = TRUE),
        median = stats::median(values, na.rm = TRUE),
        sd = stats::sd(values, na.rm = TRUE),
        min = min(values, na.rm = TRUE),
        max = max(values, na.rm = TRUE)
      )
    }

    out$mark = summariseVec(markValues)
    out$overallScore = summariseVec(overallValues)

    metricSummaryList = lapply(runs, function(run) run$metricSummary)
    metricSummaryList = Filter(is.data.frame, metricSummaryList)

    if (length(metricSummaryList) > 0) {
      metrics = unique(unlist(lapply(metricSummaryList, function(df) df$metric)))
      metrics = metrics[metrics %in% c(
        "factualScore",
        "inferenceScore",
        "completenessScore",
        "clarityScore",
        "calibrationScore"
      )]

      dimensionSummary = lapply(metrics, function(metric) {
        values = vapply(metricSummaryList, function(df) {
          row = df[df$metric == metric, , drop = FALSE]
          if (nrow(row) < 1) {
            return(NA_real_)
          }
          suppressWarnings(as.numeric(row$studentValue[1]))
        }, numeric(1))

        firstRow = NULL
        for (df in metricSummaryList) {
          row = df[df$metric == metric, , drop = FALSE]
          if (nrow(row) > 0) {
            firstRow = row[1, , drop = FALSE]
            break
          }
        }

        data.frame(
          metric = metric,
          label = as.character(firstRow$label[1]),
          mean = mean(values, na.rm = TRUE),
          median = stats::median(values, na.rm = TRUE),
          min = min(values, na.rm = TRUE),
          max = max(values, na.rm = TRUE),
          maxValue = suppressWarnings(as.numeric(firstRow$maxValue[1])),
          stringsAsFactors = FALSE
        )
      })

      out$dimensionSummary = do.call(rbind, dimensionSummary)
      rownames(out$dimensionSummary) = NULL
    }
  }

  class(out) = c("summary.wmfmGrade", "list")
  out
}
