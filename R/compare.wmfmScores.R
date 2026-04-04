#' Compare WMFM score results
#'
#' Compares score results either:
#' \itemize{
#'   \item within a single `wmfmScores` object containing two methods, or
#'   \item between two `wmfmScores` objects.
#' }
#'
#' The comparison summarises agreement separately for binary, ordinal, and
#' continuous score fields.
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

  computeWeightedKappa = function(leftVec, rightVec, weightType = "quadratic") {
    ok = !(is.na(leftVec) | is.na(rightVec))
    leftVec = suppressWarnings(as.integer(leftVec[ok]))
    rightVec = suppressWarnings(as.integer(rightVec[ok]))

    ok2 = !(is.na(leftVec) | is.na(rightVec))
    leftVec = leftVec[ok2]
    rightVec = rightVec[ok2]

    if (length(leftVec) == 0) {
      return(NA_real_)
    }

    categories = sort(unique(c(leftVec, rightVec)))
    nCat = length(categories)

    if (nCat <= 1) {
      return(1)
    }

    leftIdx = match(leftVec, categories)
    rightIdx = match(rightVec, categories)

    observed = matrix(0, nrow = nCat, ncol = nCat)

    for (i in seq_along(leftIdx)) {
      observed[leftIdx[i], rightIdx[i]] = observed[leftIdx[i], rightIdx[i]] + 1
    }

    observed = observed / sum(observed)

    leftMarginal = rowSums(observed)
    rightMarginal = colSums(observed)
    expected = outer(leftMarginal, rightMarginal)

    weights = outer(
      seq_len(nCat),
      seq_len(nCat),
      function(i, j) {
        d = abs(i - j) / (nCat - 1)

        if (identical(weightType, "quadratic")) {
          d^2
        } else {
          d
        }
      }
    )

    observedWeighted = sum(weights * observed)
    expectedWeighted = sum(weights * expected)

    if (isTRUE(all.equal(expectedWeighted, 0))) {
      return(NA_real_)
    }

    1 - (observedWeighted / expectedWeighted)
  }

  summarizeBinaryField = function(leftVec, rightVec, field) {
    ok = !(is.na(leftVec) | is.na(rightVec))
    nCompared = sum(ok)

    if (nCompared == 0) {
      return(NULL)
    }

    leftOk = as.logical(leftVec[ok])
    rightOk = as.logical(rightVec[ok])

    data.frame(
      metric = field,
      nCompared = nCompared,
      nEqual = sum(leftOk == rightOk),
      proportionEqual = mean(leftOk == rightOk),
      positiveRateLeft = mean(leftOk),
      positiveRateRight = mean(rightOk),
      meanDifference = mean(as.integer(rightOk) - as.integer(leftOk)),
      stringsAsFactors = FALSE
    )
  }

  summarizeOrdinalField = function(leftVec, rightVec, field) {
    ok = !(is.na(leftVec) | is.na(rightVec))
    nCompared = sum(ok)

    if (nCompared == 0) {
      return(NULL)
    }

    leftOk = suppressWarnings(as.numeric(leftVec[ok]))
    rightOk = suppressWarnings(as.numeric(rightVec[ok]))
    diff = rightOk - leftOk

    data.frame(
      metric = field,
      nCompared = nCompared,
      nEqual = sum(diff == 0),
      proportionEqual = mean(diff == 0),
      proportionAdjacent = mean(abs(diff) <= 1),
      meanDifference = mean(diff),
      meanAbsoluteDifference = mean(abs(diff)),
      weightedKappa = computeWeightedKappa(leftOk, rightOk, weightType = "quadratic"),
      stringsAsFactors = FALSE
    )
  }

  summarizeContinuousField = function(leftVec, rightVec, field) {
    ok = !(is.na(leftVec) | is.na(rightVec))
    nCompared = sum(ok)

    if (nCompared == 0) {
      return(NULL)
    }

    leftOk = suppressWarnings(as.numeric(leftVec[ok]))
    rightOk = suppressWarnings(as.numeric(rightVec[ok]))
    diff = rightOk - leftOk

    corr =
      if (length(leftOk) >= 2 && stats::sd(leftOk) > 0 && stats::sd(rightOk) > 0) {
        stats::cor(leftOk, rightOk)
      } else {
        NA_real_
      }

    data.frame(
      metric = field,
      nCompared = nCompared,
      meanLeft = mean(leftOk),
      meanRight = mean(rightOk),
      meanDifference = mean(diff),
      sdDifference = stats::sd(diff),
      meanAbsoluteDifference = mean(abs(diff)),
      correlation = corr,
      stringsAsFactors = FALSE
    )
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

    binaryFields = c(
      "fatalFlawDetected",
      "overallPass"
    )

    ordinalFields = c(
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
      "comparisonStructureClear"
    )

    continuousFields = c(
      "factualScore",
      "inferenceScore",
      "completenessScore",
      "clarityScore",
      "calibrationScore",
      "overallScore"
    )

    collectSummaries = function(fields, summarizer) {
      pieces = lapply(fields, function(field) {
        leftName = paste0(field, ".x")
        rightName = paste0(field, ".y")

        if (!(leftName %in% names(mergedDf)) || !(rightName %in% names(mergedDf))) {
          return(NULL)
        }

        summarizer(
          leftVec = mergedDf[[leftName]],
          rightVec = mergedDf[[rightName]],
          field = field
        )
      })

      pieces = Filter(Negate(is.null), pieces)

      if (length(pieces) == 0) {
        return(data.frame())
      }

      out = do.call(rbind, pieces)
      rownames(out) = NULL
      out
    }

    binaryAgreement = collectSummaries(binaryFields, summarizeBinaryField)
    ordinalAgreement = collectSummaries(ordinalFields, summarizeOrdinalField)
    continuousAgreement = collectSummaries(continuousFields, summarizeContinuousField)

    pairedOverallScores = NULL

    if ("overallScore.x" %in% names(mergedDf) && "overallScore.y" %in% names(mergedDf)) {
      leftOverall = suppressWarnings(as.numeric(mergedDf$overallScore.x))
      rightOverall = suppressWarnings(as.numeric(mergedDf$overallScore.y))
      ok = !(is.na(leftOverall) | is.na(rightOverall))

      if (any(ok)) {
        pairedOverallScores = data.frame(
          runId = mergedDf$runId[ok],
          leftOverallScore = leftOverall[ok],
          rightOverallScore = rightOverall[ok],
          meanOverallScore = (leftOverall[ok] + rightOverall[ok]) / 2,
          differenceOverallScore = rightOverall[ok] - leftOverall[ok],
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
        sdDifferenceRightMinusLeft = stats::sd(diffVec),
        meanAbsoluteDifference = mean(abs(diffVec))
      )
    }

    out = list(
      source = sourceLabel,
      leftMethod = leftMethod,
      rightMethod = rightMethod,
      nRunsCompared = nrow(mergedDf),
      binaryAgreement = binaryAgreement,
      ordinalAgreement = ordinalAgreement,
      continuousAgreement = continuousAgreement,
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
