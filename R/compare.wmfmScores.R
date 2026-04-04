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
      stop("No scoring methods are available.", call. = FALSE)
    }

    if (!is.null(method)) {
      if (!method %in% available) {
        stop("Requested method not present.", call. = FALSE)
      }
      return(method)
    }

    if (length(available) != 1) {
      stop("Ambiguous method selection.", call. = FALSE)
    }

    available[1]
  }

  scoreDfForMethod = function(obj, method) {
    df = as.data.frame(obj, format = "long")
    df = df[df$method == method, , drop = FALSE]

    if (nrow(df) == 0) {
      stop("No rows for method.", call. = FALSE)
    }

    rownames(df) = NULL
    df
  }

  computeWeightedKappa = function(leftVec, rightVec) {
    ok = !(is.na(leftVec) | is.na(rightVec))
    leftVec = as.integer(leftVec[ok])
    rightVec = as.integer(rightVec[ok])

    if (length(leftVec) == 0) return(NA_real_)

    cats = sort(unique(c(leftVec, rightVec)))
    k = length(cats)

    if (k <= 1) return(1)

    li = match(leftVec, cats)
    ri = match(rightVec, cats)

    obs = matrix(0, k, k)
    for (i in seq_along(li)) {
      obs[li[i], ri[i]] = obs[li[i], ri[i]] + 1
    }
    obs = obs / sum(obs)

    rowM = rowSums(obs)
    colM = colSums(obs)
    exp = outer(rowM, colM)

    w = outer(
      seq_len(k), seq_len(k),
      function(i, j) ((abs(i - j) / (k - 1))^2)
    )

    ow = sum(w * obs)
    ew = sum(w * exp)

    if (ew == 0) return(NA_real_)

    1 - (ow / ew)
  }

  summarizeOrdinalField = function(leftVec, rightVec, field) {
    ok = !(is.na(leftVec) | is.na(rightVec))
    if (sum(ok) == 0) return(NULL)

    left = as.numeric(leftVec[ok])
    right = as.numeric(rightVec[ok])
    diff = right - left

    data.frame(
      metric = field,
      nCompared = length(diff),
      proportionEqual = mean(diff == 0),
      proportionAdjacent = mean(abs(diff) <= 1),
      meanDifference = mean(diff),
      meanAbsoluteDifference = mean(abs(diff)),
      weightedKappa = computeWeightedKappa(left, right),
      stringsAsFactors = FALSE
    )
  }

  summarizeContinuousField = function(leftVec, rightVec, field) {
    ok = !(is.na(leftVec) | is.na(rightVec))
    if (sum(ok) == 0) return(NULL)

    left = as.numeric(leftVec[ok])
    right = as.numeric(rightVec[ok])
    diff = right - left

    corVal =
      if (length(left) > 1) stats::cor(left, right) else NA_real_

    data.frame(
      metric = field,
      meanDifference = mean(diff),
      meanAbsoluteDifference = mean(abs(diff)),
      correlation = corVal,
      stringsAsFactors = FALSE
    )
  }

  buildComparison = function(leftDf, rightDf, leftMethod, rightMethod, sourceLabel) {

    merged = merge(
      leftDf, rightDf,
      by = "runId",
      suffixes = c(".x", ".y")
    )

    ordinalFields = c("effectDirectionCorrect", "clarityAdequate")
    continuousFields = c("overallScore")

    collect = function(fields, fun) {
      out = lapply(fields, function(f) {
        lx = paste0(f, ".x")
        ry = paste0(f, ".y")
        if (!(lx %in% names(merged) && ry %in% names(merged))) return(NULL)
        fun(merged[[lx]], merged[[ry]], f)
      })
      out = Filter(Negate(is.null), out)
      if (length(out) == 0) return(data.frame())
      do.call(rbind, out)
    }

    ordinalAgreement = collect(ordinalFields, summarizeOrdinalField)
    continuousAgreement = collect(continuousFields, summarizeContinuousField)

    paired = NULL
    if ("overallScore.x" %in% names(merged)) {
      l = as.numeric(merged$overallScore.x)
      r = as.numeric(merged$overallScore.y)
      ok = !(is.na(l) | is.na(r))
      paired = data.frame(
        runId = merged$runId[ok],
        leftOverallScore = l[ok],
        rightOverallScore = r[ok],
        meanOverallScore = (l[ok] + r[ok]) / 2,
        differenceOverallScore = r[ok] - l[ok]
      )
    }

    overallSummary = NULL
    if (!is.null(paired) && nrow(paired) > 0) {
      d = paired$differenceOverallScore
      md = mean(d)
      sdv = stats::sd(d)
      overallSummary = list(
        meanDifferenceRightMinusLeft = md,
        sdDifferenceRightMinusLeft = sdv,
        loaLower = md - 1.96 * sdv,
        loaUpper = md + 1.96 * sdv
      )
    }

    structure(
      list(
        source = sourceLabel,
        leftMethod = leftMethod,
        rightMethod = rightMethod,
        ordinalAgreement = ordinalAgreement,
        continuousAgreement = continuousAgreement,
        overallSummary = overallSummary,
        pairedOverallScores = paired
      ),
      class = "wmfmScoreComparison"
    )
  }

  if (is.null(y)) {
    leftDf = scoreDfForMethod(x, "deterministic")
    rightDf = scoreDfForMethod(x, "llm")

    return(buildComparison(leftDf, rightDf, "deterministic", "llm", "within_object"))
  }

  leftMethod = getSingleMethod(x, xMethod)
  rightMethod = getSingleMethod(y, yMethod)

  buildComparison(
    scoreDfForMethod(x, leftMethod),
    scoreDfForMethod(y, rightMethod),
    leftMethod,
    rightMethod,
    "between_objects"
  )
}
