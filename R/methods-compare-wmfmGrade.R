#' Compare grading results for wmfmGrade objects
#'
#' Compares deterministic and llm grading either within a single `wmfmGrade`
#' object that contains both methods, or across two separate `wmfmGrade`
#' objects.
#'
#' @param x A scored `wmfmGrade` object.
#' @param y Optional second scored `wmfmGrade` object.
#' @param methods Character vector of length 2 naming the methods to compare
#'   when comparing within a single object. Defaults to
#'   `c("deterministic", "llm")`.
#' @param ... Unused. Included for S3 compatibility.
#'
#' @return An object of class `wmfmGradeComparison`.
#' @export
compare.wmfmGrade = function(
    x,
    y = NULL,
    methods = c("deterministic", "llm"),
    ...
) {

  `%||%` = function(a, b) if (is.null(a)) b else a

  if (!inherits(x, "wmfmGrade")) {
    stop("`x` must inherit from `wmfmGrade`.", call. = FALSE)
  }

  if (!isTRUE(x$meta$scored)) {
    stop("`x` has not been scored yet.", call. = FALSE)
  }

  if (!is.null(y) && !inherits(y, "wmfmGrade")) {
    stop("`y` must be NULL or inherit from `wmfmGrade`.", call. = FALSE)
  }

  if (!is.character(methods) || length(methods) != 2 || anyNA(methods)) {
    stop("`methods` must be a character vector of length 2.", call. = FALSE)
  }

  extractSingleMethod = function(g) {
    availableMethods = names(g$scores$byMethod %||% list())

    if (length(availableMethods) != 1) {
      stop(
        "When comparing two wmfmGrade objects, each object should normally contain exactly one scored method. ",
        "Found methods: ", paste(availableMethods, collapse = ", "),
        call. = FALSE
      )
    }

    list(
      method = availableMethods[1],
      score = g$scores$byMethod[[availableMethods[1]]],
      feedback = g$feedback$byMethod[[availableMethods[1]]]
    )
  }

  if (is.null(y)) {
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
    leftFeedback = x$feedback$byMethod[[leftMethod]]
    rightFeedback = x$feedback$byMethod[[rightMethod]]
    sourceGrade = x
  } else {
    if (!isTRUE(y$meta$scored)) {
      stop("`y` has not been scored yet.", call. = FALSE)
    }

    xInfo = extractSingleMethod(x)
    yInfo = extractSingleMethod(y)

    leftMethod = xInfo$method
    rightMethod = yInfo$method
    leftBlock = xInfo$score
    rightBlock = yInfo$score
    leftFeedback = xInfo$feedback
    rightFeedback = yInfo$feedback
    sourceGrade = list(x = x, y = y)
  }

  leftMetricSummary = leftBlock$metricSummary
  rightMetricSummary = rightBlock$metricSummary

  if (!is.data.frame(leftMetricSummary) || !is.data.frame(rightMetricSummary)) {
    stop("Both compared methods must contain a metricSummary data frame.", call. = FALSE)
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
    c("metric", "studentValue"),
    drop = FALSE
  ]
  names(rightDf)[names(rightDf) == "studentValue"] = "rightValue"

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
    left = leftFeedback$advisoryFlags,
    right = rightFeedback$advisoryFlags
  )

  newWmfmGradeComparison(
    summary = summary,
    metricComparison = metricComparison,
    advisoryComparison = advisoryComparison,
    comparedMethods = c(leftMethod, rightMethod),
    sourceGrade = sourceGrade
  )
}
