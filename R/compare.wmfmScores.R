#' Compare WMFM score results
#'
#' Compares score results either:
#' \itemize{
#'   \item within a single `wmfmScores` object containing two methods, or
#'   \item between two `wmfmScores` objects.
#' }
#'
#' When `y = NULL`, `x` must contain both deterministic and LLM scoring results.
#' In that case the two methods are compared within the same score object.
#'
#' When `y` is supplied, each object must contain exactly one scoring method, or
#' the requested methods must be specified explicitly using `xMethod` and
#' `yMethod`.
#'
#' @param x A `wmfmScores` object.
#' @param y Optional second `wmfmScores` object.
#' @param xMethod Optional character string naming the method to use from `x`.
#' @param yMethod Optional character string naming the method to use from `y`.
#' @param ... Unused. Included for S3 compatibility.
#'
#' @return An object of class `wmfmScoreComparison`.
#' @export
compare.wmfmScores = function(
    x,
    y = NULL,
    xMethod = NULL,
    yMethod = NULL,
    ...
) {

  if (!inherits(x, "wmfmScores")) {
    stop("`x` must inherit from `wmfmScores`.", call. = FALSE)
  }

  getSingleMethod = function(obj, method = NULL, argName = "method") {
    available = obj$methods %||% character(0)

    if (length(available) == 0) {
      stop("No scoring methods are available in the supplied object.", call. = FALSE)
    }

    if (!is.null(method)) {
      if (!is.character(method) || length(method) != 1 || is.na(method)) {
        stop("`", argName, "` must be a single character string.", call. = FALSE)
      }

      if (!method %in% available) {
        stop(
          "Requested method `", method,
          "` is not present. Available methods: ",
          paste(available, collapse = ", "),
          call. = FALSE
        )
      }

      return(method)
    }

    if (length(available) != 1) {
      stop(
        "Method selection is ambiguous. Please specify `", argName,
        "`. Available methods: ",
        paste(available, collapse = ", "),
        call. = FALSE
      )
    }

    available[1]
  }

  scoreDfForMethod = function(obj, method) {
    df = as.data.frame(obj, format = "long")
    df = df[df$method == method, , drop = FALSE]

    if (nrow(df) == 0) {
      stop("No rows were found for method `", method, "`.", call. = FALSE)
    }

    rownames(df) = NULL
    df
  }

  buildComparison = function(leftDf, rightDf, leftMethod, rightMethod, sourceLabel) {
    mergedDf = merge(
      leftDf,
      rightDf,
      by = "runId",
      suffixes = c(".x", ".y"),
      all = FALSE,
      sort = TRUE
    )

    if (nrow(mergedDf) == 0) {
      stop("No overlapping run IDs were found to compare.", call. = FALSE)
    }

    metricFields = c(
      "effectDirectionCorrect",
      "effectScaleAppropriate",
      "referenceGroupHandledCorrectly",
      "interactionCoverageAdequate",
      "interactionSubstantiveCorrect",
      "uncertaintyHandlingAppropriate",
      "inferentialRegisterAppropriate",
      "mainEffectCoverageAdequate",
      "referenceGroupCoverageAdequate",
      "clarityAdequate",
      "numericExpressionAdequate",
      "comparisonStructureClear",
      "factualScore",
      "inferenceScore",
      "completenessScore",
      "clarityScore",
      "calibrationScore",
      "overallScore",
      "fatalFlawDetected",
      "overallPass"
    )

    metricAgreement = data.frame(
      metric = character(0),
      nCompared = integer(0),
      nEqual = integer(0),
      proportionEqual = numeric(0),
      meanDifference = numeric(0),
      stringsAsFactors = FALSE
    )

    pairedOverallScores = NULL

    for (field in metricFields) {
      leftName = paste0(field, ".x")
      rightName = paste0(field, ".y")

      if (!(leftName %in% names(mergedDf)) || !(rightName %in% names(mergedDf))) {
        next
      }

      leftVec = mergedDf[[leftName]]
      rightVec = mergedDf[[rightName]]

      ok = !(is.na(leftVec) | is.na(rightVec))
      nCompared = sum(ok)

      if (nCompared == 0) {
        next
      }

      leftOk = leftVec[ok]
      rightOk = rightVec[ok]
      nEqual = sum(leftOk == rightOk)

      meanDifference =
        if (is.numeric(leftOk) && is.numeric(rightOk)) {
          mean(rightOk - leftOk)
        } else if (is.logical(leftOk) && is.logical(rightOk)) {
          mean(as.integer(rightOk) - as.integer(leftOk))
        } else {
          NA_real_
        }

      metricAgreement = rbind(
        metricAgreement,
        data.frame(
          metric = field,
          nCompared = nCompared,
          nEqual = nEqual,
          proportionEqual = nEqual / nCompared,
          meanDifference = meanDifference,
          stringsAsFactors = FALSE
        )
      )

      if (identical(field, "overallScore")) {
        pairedOverallScores = data.frame(
          leftOverallScore = as.numeric(leftOk),
          rightOverallScore = as.numeric(rightOk),
          meanOverallScore = (as.numeric(leftOk) + as.numeric(rightOk)) / 2,
          differenceOverallScore = as.numeric(rightOk) - as.numeric(leftOk),
          stringsAsFactors = FALSE
        )
      }
    }

    overallSummary = NULL

    if (!is.null(pairedOverallScores) && nrow(pairedOverallScores) > 0) {
      diffVec = pairedOverallScores$differenceOverallScore
      overallSummary = list(
        meanLeftOverallScore = mean(pairedOverallScores$leftOverallScore),
        meanRightOverallScore = mean(pairedOverallScores$rightOverallScore),
        meanDifferenceRightMinusLeft = mean(diffVec),
        sdDifferenceRightMinusLeft = stats::sd(diffVec)
      )
    }

    out = list(
      source = sourceLabel,
      leftMethod = leftMethod,
      rightMethod = rightMethod,
      nRunsCompared = nrow(mergedDf),
      metricAgreement = metricAgreement,
      overallSummary = overallSummary,
      pairedOverallScores = pairedOverallScores
    )

    class(out) = c("wmfmScoreComparison", class(out))
    out
  }

  if (is.null(y)) {
    available = x$methods %||% character(0)

    if (!all(c("deterministic", "llm") %in% available)) {
      stop(
        "When `y = NULL`, `x` must contain both `deterministic` and `llm` scores.",
        call. = FALSE
      )
    }

    leftMethod = "deterministic"
    rightMethod = "llm"

    leftDf = scoreDfForMethod(x, leftMethod)
    rightDf = scoreDfForMethod(x, rightMethod)

    return(
      buildComparison(
        leftDf = leftDf,
        rightDf = rightDf,
        leftMethod = leftMethod,
        rightMethod = rightMethod,
        sourceLabel = "within_object"
      )
    )
  }

  if (!inherits(y, "wmfmScores")) {
    stop("`y` must inherit from `wmfmScores`.", call. = FALSE)
  }

  leftMethod = getSingleMethod(x, method = xMethod, argName = "xMethod")
  rightMethod = getSingleMethod(y, method = yMethod, argName = "yMethod")

  leftDf = scoreDfForMethod(x, leftMethod)
  rightDf = scoreDfForMethod(y, rightMethod)

  buildComparison(
    leftDf = leftDf,
    rightDf = rightDf,
    leftMethod = leftMethod,
    rightMethod = rightMethod,
    sourceLabel = "between_objects"
  )
}
