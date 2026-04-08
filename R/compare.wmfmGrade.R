#' Compare grading methods for a wmfmGrade object
#'
#' Compares two scoring methods already present on a `wmfmGrade` object,
#' typically deterministic versus llm.
#'
#' @param x A scored `wmfmGrade` object.
#' @param methods Character vector of length 2 naming the methods to compare.
#'   Defaults to `c("deterministic", "llm")`.
#' @param ... Unused. Included for S3 compatibility.
#'
#' @return An object of class `wmfmGradeComparison`.
#' @export
compare.wmfmGrade = function(
    x,
    methods = c("deterministic", "llm"),
    ...
) {

  if (!inherits(x, "wmfmGrade")) {
    stop("`x` must inherit from `wmfmGrade`.", call. = FALSE)
  }

  if (!isTRUE(x$meta$scored)) {
    stop("`x` has not been scored yet.", call. = FALSE)
  }

  if (!is.character(methods) || length(methods) != 2 || anyNA(methods)) {
    stop("`methods` must be a character vector of length 2.", call. = FALSE)
  }

  methods = as.character(methods)
  availableMethods = names(x$scores$byMethod %||% list())

  if (!all(methods %in% availableMethods)) {
    stop(
      "Both requested methods must already be present on `x`. Available methods: ",
      paste(availableMethods, collapse = ", "),
      call. = FALSE
    )
  }

  leftMethod = methods[1]
  rightMethod = methods[2]

  leftBlock = x$scores$byMethod[[leftMethod]]
  rightBlock = x$scores$byMethod[[rightMethod]]

  leftMetricSummary = leftBlock$metricSummary
  rightMetricSummary = rightBlock$metricSummary

  if (!is.data.frame(leftMetricSummary) || !is.data.frame(rightMetricSummary)) {
    stop("Both scoring methods must contain a metricSummary data frame.", call. = FALSE)
  }

  keepMetrics = c(
    "factualScore",
    "inferenceScore",
    "completenessScore",
    "clarityScore",
    "calibrationScore"
  )

  leftDf = leftMetricSummary[
    leftMetricSummary$metric %in% keepMetrics,
    c("metric", "label", "studentValue", "maxValue"),
    drop = FALSE
  ]
  names(leftDf)[names(leftDf) == "studentValue"] = "leftValue"

  rightDf = rightMetricSummary[
    rightMetricSummary$metric %in% keepMetrics,
    c("metric", "label", "studentValue", "maxValue"),
    drop = FALSE
  ]
  names(rightDf)[names(rightDf) == "studentValue"] = "rightValue"
  rightDf = rightDf[, c("metric", "rightValue"), drop = FALSE]

  metricComparison = merge(
    leftDf,
    rightDf,
    by = "metric",
    all = TRUE,
    sort = FALSE
  )

  metricOrder = match(metricComparison$metric, keepMetrics)
  metricComparison = metricComparison[order(metricOrder), , drop = FALSE]
  rownames(metricComparison) = NULL

  metricComparison$leftMethod = leftMethod
  metricComparison$rightMethod = rightMethod
  metricComparison$difference = suppressWarnings(
    as.numeric(metricComparison$rightValue) - as.numeric(metricComparison$leftValue)
  )
  metricComparison$absDifference = abs(metricComparison$difference)

  metricComparison = metricComparison[
    ,
    c(
      "metric",
      "label",
      "leftMethod",
      "leftValue",
      "rightMethod",
      "rightValue",
      "difference",
      "absDifference",
      "maxValue"
    ),
    drop = FALSE
  ]

  summary = list(
    leftMethod = leftMethod,
    rightMethod = rightMethod,
    leftMark = suppressWarnings(as.numeric(leftBlock$mark)),
    rightMark = suppressWarnings(as.numeric(rightBlock$mark)),
    markDifference = suppressWarnings(as.numeric(rightBlock$mark) - as.numeric(leftBlock$mark)),
    leftOverallScore = suppressWarnings(as.numeric(leftBlock$overallScore)),
    rightOverallScore = suppressWarnings(as.numeric(rightBlock$overallScore)),
    overallDifference = suppressWarnings(as.numeric(rightBlock$overallScore) - as.numeric(leftBlock$overallScore))
  )

  advisoryComparison = list(
    left = x$feedback$byMethod[[leftMethod]]$advisoryFlags,
    right = x$feedback$byMethod[[rightMethod]]$advisoryFlags
  )

  newWmfmGradeComparison(
    summary = summary,
    metricComparison = metricComparison,
    advisoryComparison = advisoryComparison,
    comparedMethods = methods,
    sourceGrade = x
  )
}
