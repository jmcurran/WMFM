#' Build a `wmfmScoreComparison` object
#'
#' Internal helper that constructs agreement summaries and run-level
#' paired data for two score data frames.
#'
#' @param leftDf Long-form score data for the left method.
#' @param rightDf Long-form score data for the right method.
#' @param leftMethod Name of the left method.
#' @param rightMethod Name of the right method.
#' @param sourceLabel Character label describing the comparison source.
#' @param registry Metric registry.
#'
#' @return An object of class `wmfmScoreComparison`.
#' @keywords internal
buildWmfmScoreComparison = function(
  leftDf,
  rightDf,
  leftMethod,
  rightMethod,
  sourceLabel,
  registry = getWmfmMetricRegistry()
) {
  validateWmfmMetricRegistry(registry)

  pairData = buildWmfmComparisonPairs(
    leftDf = leftDf,
    rightDf = rightDf,
    registry = registry,
    leftMethod = leftMethod,
    rightMethod = rightMethod
  )

  registry = registry[registry$includeInComparison, , drop = FALSE]

  collect = function(metricType, summarizer) {
    rows = registry$metricType == metricType

    if (!any(rows)) {
      return(data.frame())
    }

    out = lapply(seq_len(nrow(registry[rows, , drop = FALSE])), function(i) {
      metricRow = registry[rows, , drop = FALSE][i, , drop = FALSE]
      metricName = metricRow$metricName
      leftName = metricName
      rightName = metricName

      if (!(leftName %in% names(leftDf) && rightName %in% names(rightDf))) {
        return(NULL)
      }

      merged = merge(
        leftDf[c("runId", leftName)],
        rightDf[c("runId", rightName)],
        by = "runId",
        suffixes = c(".x", ".y")
      )

      summarizer(
        leftVec = merged[[paste0(metricName, ".x")]],
        rightVec = merged[[paste0(metricName, ".y")]],
        metricRow = metricRow
      )
    })

    out = Filter(Negate(is.null), out)

    if (length(out) == 0) {
      return(data.frame())
    }

    do.call(rbind, out)
  }

  binaryAgreement = collect("binary", summariseBinaryAgreement)
  ordinalAgreement = collect("ordinal", summariseOrdinalAgreement)
  continuousAgreement = collect("continuous", summariseContinuousAgreement)

  pairedOverallScores = data.frame()
  overallSummary = NULL

  if ("overallScore" %in% names(leftDf) && "overallScore" %in% names(rightDf)) {
    mergedOverall = merge(
      leftDf[c("runId", "overallScore")],
      rightDf[c("runId", "overallScore")],
      by = "runId",
      suffixes = c(".x", ".y")
    )

    l = as.numeric(mergedOverall$overallScore.x)
    r = as.numeric(mergedOverall$overallScore.y)
    ok = !(is.na(l) | is.na(r))

    pairedOverallScores = data.frame(
      runId = mergedOverall$runId[ok],
      leftOverallScore = l[ok],
      rightOverallScore = r[ok],
      meanOverallScore = (l[ok] + r[ok]) / 2,
      differenceOverallScore = r[ok] - l[ok],
      stringsAsFactors = FALSE
    )

    if (nrow(pairedOverallScores) > 0) {
      d = pairedOverallScores$differenceOverallScore
      md = mean(d)
      sdv = if (length(d) > 1) stats::sd(d) else NA_real_

      overallSummary = list(
        meanDifferenceRightMinusLeft = md,
        sdDifferenceRightMinusLeft = sdv,
        loaLower = md - 1.96 * sdv,
        loaUpper = md + 1.96 * sdv
      )
    }
  }

  structure(
    list(
      source = sourceLabel,
      leftMethod = leftMethod,
      rightMethod = rightMethod,
      registry = registry,
      binaryAgreement = binaryAgreement,
      ordinalAgreement = ordinalAgreement,
      continuousAgreement = continuousAgreement,
      pairData = pairData,
      overallSummary = overallSummary,
      pairedOverallScores = pairedOverallScores
    ),
    class = "wmfmScoreComparison"
  )
}


#' Classify likely source of metric disagreement
#'
#' Internal heuristic classifier for \code{diagnose()}.
#'
#' @param summaryDf One-row summary data frame produced by
#'   \code{buildMetricDiagnosisSummary()}.
#'
#' @return A single character string describing the likely disagreement
#'   class.
#'
#' @keywords internal
classifyMetricDisagreement = function(summaryDf) {
  stopifnot(is.data.frame(summaryDf), nrow(summaryDf) == 1L)

  disagreementRate = summaryDf$disagreementRate[[1L]]
  directionConsistency = summaryDf$directionConsistency[[1L]]

  detConstant = isTRUE(summaryDf$detConstant[[1L]])
  llmConstant = isTRUE(summaryDf$llmConstant[[1L]])

  detMin = summaryDf$detMin[[1L]]
  detMax = summaryDf$detMax[[1L]]
  llmMin = summaryDf$llmMin[[1L]]
  llmMax = summaryDf$llmMax[[1L]]
  meanLlmMinusDet = summaryDf$meanLlmMinusDet[[1L]]

  if (disagreementRate == 0) {
    return("noSystematicDisagreement")
  }

  if (disagreementRate >= 0.8 &&
      directionConsistency >= 0.9 &&
      detConstant &&
      llmConstant &&
      detMax < llmMin) {
    return("deterministicRuleLikelyTooStrict")
  }

  if (disagreementRate >= 0.6 &&
      directionConsistency >= 0.8 &&
      detConstant &&
      !llmConstant &&
      meanLlmMinusDet > 0) {
    return("deterministicRuleLikelyMissingNuance")
  }

  if (disagreementRate >= 0.6 &&
      directionConsistency >= 0.8 &&
      llmConstant &&
      !detConstant &&
      meanLlmMinusDet > 0) {
    return("llmMayBeOverInterpreting")
  }

  if (disagreementRate >= 0.5 && directionConsistency < 0.8) {
    return("rubricOrMetricMayBeAmbiguous")
  }

  if (meanLlmMinusDet > 0) {
    return("llmGenerallyHigherThanDeterministic")
  }

  if (meanLlmMinusDet < 0) {
    return("deterministicGenerallyHigherThanLlm")
  }

  "unclassified"
}


#' Classify run-level score disagreement for heatmap plotting
#'
#' Converts paired run-level values from `buildWmfmComparisonPairs()` into a
#' categorical disagreement label that can be used for a heatmap.
#'
#' Classification rules are:
#' \itemize{
#'   \item binary metrics: `"exact"` or `"different"`;
#'   \item ordinal metrics: `"exact"`, `"adjacent"`, `"moderate"`, or `"large"`;
#'   \item continuous metrics: excluded by default in the heatmap and returned
#'   as `NA` unless `includeContinuous = TRUE`, in which case they are binned as
#'   `"exact"`, `"small"`, `"moderate"`, or `"large"` using absolute
#'   differences.
#' }
#'
#' For ordinal metrics, ordered levels are taken from the registry stored on the
#' comparison object.
#'
#' @param x A `wmfmScoreComparison` object.
#' @param includeContinuous Logical. Should continuous metrics be classified?
#' @param continuousBreaks Numeric vector of length 3 giving cut points for the
#'   absolute difference bins used for continuous metrics.
#'
#' @return A data frame like `x$pairData` with an added
#'   `disagreementClass` column.
#' @keywords internal
classifyWmfmScoreDisagreement = function(
    x,
    includeContinuous = FALSE,
    continuousBreaks = c(0.10, 0.35, 0.75)
) {
  if (!inherits(x, "wmfmScoreComparison")) {
    stop("`x` must inherit from `wmfmScoreComparison`.", call. = FALSE)
  }

  pairData = x$pairData

  if (!is.data.frame(pairData) || nrow(pairData) == 0) {
    return(data.frame())
  }

  registry = x$registry
  orderedLookup = stats::setNames(registry$orderedLevels, registry$metricName)

  classifyOne = function(metric, metricType, leftValue, rightValue) {
    if (is.na(leftValue) || is.na(rightValue)) {
      return(NA_character_)
    }

    if (identical(metricType, "binary")) {
      return(if (identical(as.logical(leftValue), as.logical(rightValue))) "exact" else "different")
    }

    if (identical(metricType, "ordinal")) {
      levels = orderedLookup[[metric]]
      left = match(as.character(leftValue), levels)
      right = match(as.character(rightValue), levels)

      if (is.na(left) || is.na(right)) {
        return(NA_character_)
      }

      absDiff = abs(right - left)

      if (absDiff == 0) {
        return("exact")
      }

      if (absDiff == 1) {
        return("adjacent")
      }

      if (absDiff == 2) {
        return("moderate")
      }

      return("large")
    }

    if (!isTRUE(includeContinuous)) {
      return(NA_character_)
    }

    absDiff = abs(as.numeric(rightValue) - as.numeric(leftValue))

    if (is.na(absDiff)) {
      return(NA_character_)
    }

    if (absDiff <= continuousBreaks[1]) {
      return("exact")
    }

    if (absDiff <= continuousBreaks[2]) {
      return("small")
    }

    if (absDiff <= continuousBreaks[3]) {
      return("moderate")
    }

    "large"
  }

  pairData$disagreementClass = vapply(
    seq_len(nrow(pairData)),
    FUN = function(i) {
      classifyOne(
        metric = pairData$metric[i],
        metricType = pairData$metricType[i],
        leftValue = pairData$leftValue[i],
        rightValue = pairData$rightValue[i]
      )
    },
    FUN.VALUE = character(1)
  )

  pairData
}


#' Extract run-level comparison data for a metric
#'
#' Returns run-level paired scores for a given metric from a
#' `wmfmScoreComparison` object.
#'
#' This accessor uses the metric registry stored on the comparison object as the
#' source of truth for valid metric names. Because the choices are dynamic,
#' `match.arg()` is used for validation and partial matching, but IDE tab
#' completion will not show the possible values automatically.
#'
#' @param x A `wmfmScoreComparison` object.
#' @param metric Character name of the metric. Must be one of the metric names
#'   stored in `x$registry$metricName`.
#'
#' @return An object of class `metricComparisonData` with one row per run.
#'   When one of the compared methods is deterministic, the returned data frame
#'   includes a `detValue` column and a method-specific value column such as
#'   `llmValue`. When the comparison is between deterministic and LLM scoring,
#'   the output also includes `llmMinusDet` and `detMinusLlm`.
#' @export
getMetricComparisonData = function(x, metric) {

  if (!inherits(x, "wmfmScoreComparison")) {
    stop("`x` must inherit from `wmfmScoreComparison`.", call. = FALSE)
  }

  metricChoices = unique(as.character(x$registry$metricName))
  metricChoices = metricChoices[!is.na(metricChoices) & nzchar(metricChoices)]

  if (length(metricChoices) == 0) {
    stop("`x$registry` does not contain any metric names.", call. = FALSE)
  }

  if (missing(metric)) {
    stop(
      "`metric` must be supplied. Use the comparison registry to see valid choices.",
      call. = FALSE
    )
  }

  metric = match.arg(metric, choices = metricChoices)

  df = x$pairData

  if (is.null(df) || nrow(df) == 0) {
    stop("`pairData` is empty.", call. = FALSE)
  }

  subDf = df[df$metric == metric, c(
    "runId",
    "leftValue",
    "rightValue"
  ), drop = FALSE]

  if (nrow(subDf) == 0) {
    stop("No data found for metric: ", metric, call. = FALSE)
  }

  leftMethod = as.character(x$leftMethod)[1]
  rightMethod = as.character(x$rightMethod)[1]

  if (identical(leftMethod, "deterministic")) {
    detVals = subDf$leftValue
    otherVals = subDf$rightValue
    otherName = rightMethod
  } else if (identical(rightMethod, "deterministic")) {
    detVals = subDf$rightValue
    otherVals = subDf$leftValue
    otherName = leftMethod
  } else {
    detVals = subDf$leftValue
    otherVals = subDf$rightValue
    otherName = rightMethod
  }

  out = data.frame(
    runId = subDf$runId,
    detValue = detVals,
    stringsAsFactors = FALSE
  )

  otherColName = paste0(otherName, "Value")
  out[[otherColName]] = otherVals

  if (identical(otherName, "llm")) {
    out$llmMinusDet = otherVals - detVals
    out$detMinusLlm = detVals - otherVals
    diffVals = out$llmMinusDet
  } else {
    out$difference = otherVals - detVals
    diffVals = out$difference
  }

  out$absDifference = abs(diffVals)

  out$agreementClass = ifelse(
    out$absDifference == 0,
    "exact",
    ifelse(out$absDifference == 1, "adjacent", "different")
  )

  rownames(out) = NULL

  structure(
    out,
    metric = metric,
    leftMethod = leftMethod,
    rightMethod = rightMethod,
    class = c("metricComparisonData", "data.frame")
  )
}


#' Derive paired metric comparison data directly from wmfmScores
#'
#' Internal helper that reconstructs run-level paired deterministic and LLM
#' scores for a single metric from a \code{wmfmScores} object.
#'
#' @param scores A \code{wmfmScores} object.
#' @param metric A single metric name.
#'
#' @return A data frame with columns \code{runId}, \code{detValue}, and
#'   \code{llmValue}.
#'
#' @keywords internal
deriveMetricComparisonDataFromScores = function(scores, metric) {
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

  if (!is.list(detList) || !is.list(llmList)) {
    stop("scores$scores$deterministic and scores$scores$llm must both be lists.")
  }

  if (length(detList) != length(llmList)) {
    stop("Deterministic and LLM score lists have different lengths.")
  }

  detDf = do.call(
    rbind,
    lapply(seq_along(detList), function(i) {
      run = detList[[i]]

      if (!is.list(run) || is.null(names(run))) {
        stop("Deterministic score records must be named lists.")
      }

      if (!metric %in% names(run)) {
        stop("Metric not found in deterministic score record: ", metric)
      }

      data.frame(
        runId = i,
        detValue = suppressWarnings(as.numeric(run[[metric]])),
        stringsAsFactors = FALSE
      )
    })
  )

  llmDf = do.call(
    rbind,
    lapply(seq_along(llmList), function(i) {
      run = llmList[[i]]

      if (!is.list(run) || is.null(names(run))) {
        stop("LLM score records must be named lists.")
      }

      if (!metric %in% names(run)) {
        stop("Metric not found in llm score record: ", metric)
      }

      data.frame(
        runId = i,
        llmValue = suppressWarnings(as.numeric(run[[metric]])),
        stringsAsFactors = FALSE
      )
    })
  )

  merge(detDf, llmDf, by = "runId", all = TRUE, sort = FALSE)
}


#' List metrics that can be compared in wmfmScores objects
#'
#' Internal helper used by comparison and diagnosis workflows.
#'
#' @param scores A \code{wmfmScores} object.
#'
#' @return A character vector of metric names present in both deterministic and
#'   LLM score records and having at least one non-missing numeric value on
#'   each side.
#'
#' @keywords internal
listMetricComparisonMetrics = function(scores) {
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
      "to compare metrics."
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
    "primaryScoringMethod",
    "interactionEvidenceAppropriate"
  )

  metricNames = setdiff(metricNames, excluded)
  metricNames = metricNames[!is.na(metricNames) & nzchar(metricNames)]

  extractMetricValues = function(runList, metricName) {
    vapply(
      runList,
      function(x) {
        if (!is.list(x) || is.null(names(x)) || !(metricName %in% names(x))) {
          return(NA_character_)
        }

        as.character(x[[metricName]])
      },
      FUN.VALUE = character(1)
    )
  }

  hasUsableNumericValues = function(metricName) {
    detVals = suppressWarnings(as.numeric(extractMetricValues(detList, metricName)))
    llmVals = suppressWarnings(as.numeric(extractMetricValues(llmList, metricName)))

    any(!is.na(detVals)) && any(!is.na(llmVals))
  }

  metricNames = metricNames[vapply(metricNames, hasUsableNumericValues, logical(1))]
  sort(metricNames)
}


#' Summarise metric-level comparison and deterministic ease
#'
#' Builds a metric-level summary combining cross-method disagreement
#' with deterministic stability/ease measures.
#'
#' @param scores A `wmfmScores` object.
#' @param comparison A `wmfmScoreComparison` object.
#' @param deterministicMethod Name of deterministic method.
#' @param orderBy Optional ordering: NULL, `"disagreement"`, or `"ease"`.
#'
#' @return An object of class `metricComparisonSummary`.
#' @export
summariseMetricComparison = function(
  scores,
  comparison,
  deterministicMethod = "deterministic",
  orderBy = NULL
) {

  if (!inherits(scores, "wmfmScores")) {
    stop("`scores` must inherit from `wmfmScores`.", call. = FALSE)
  }

  if (!inherits(comparison, "wmfmScoreComparison")) {
    stop("`comparison` must inherit from `wmfmScoreComparison`.", call. = FALSE)
  }

  registry = comparison$registry

  if (is.null(registry) || !is.data.frame(registry) || nrow(registry) == 0) {
    stop("`comparison$registry` must be a non-empty data.frame.", call. = FALSE)
  }

  longDf = as.data.frame(scores, format = "long")
  longDf = longDf[longDf$method == deterministicMethod, , drop = FALSE]

  if (nrow(longDf) == 0) {
    stop("No rows found for deterministic method `", deterministicMethod, "`.", call. = FALSE)
  }

  safeEntropy = function(x) {
    x = x[!is.na(x)]
    if (length(x) == 0) {
      return(NA_real_)
    }
    p = as.numeric(table(x)) / length(x)
    -sum(p * log(p))
  }

  modalProp = function(x) {
    x = x[!is.na(x)]
    if (length(x) == 0) {
      return(NA_real_)
    }
    max(table(x)) / length(x)
  }

  deterministicDf = do.call(rbind, lapply(seq_len(nrow(registry)), function(i) {

    metricName = registry$metricName[i]
    metricType = registry$metricType[i]

    if (!metricName %in% names(longDf)) {
      return(NULL)
    }

    vals = longDf[[metricName]]

    data.frame(
      metric = metricName,
      label = registry$label[i],
      group = registry$group[i],
      metricType = metricType,
      modalProportionDeterministic = modalProp(as.character(vals)),
      entropyDeterministic = safeEntropy(as.character(vals)),
      nUniqueDeterministic = length(unique(vals[!is.na(vals)])),
      stringsAsFactors = FALSE
    )
  }))

  if (is.null(deterministicDf) || nrow(deterministicDf) == 0) {
    stop("Could not derive deterministic metric summaries.", call. = FALSE)
  }

  cleanCols = function(df, cols) {
    if (is.null(df) || nrow(df) == 0) {
      return(NULL)
    }
    df[, intersect(cols, names(df)), drop = FALSE]
  }

  ordinal = cleanCols(comparison$ordinalAgreement, c(
    "metric",
    "meanAbsoluteDifference",
    "proportionEqual",
    "proportionAdjacent",
    "weightedKappa"
  ))

  binary = cleanCols(comparison$binaryAgreement, c(
    "metric",
    "meanAbsoluteDifference",
    "proportionEqual"
  ))

  continuous = cleanCols(comparison$continuousAgreement, c(
    "metric",
    "meanAbsoluteDifference",
    "correlation"
  ))

  allCols = c(
    "metric",
    "meanAbsoluteDifference",
    "proportionEqual",
    "proportionAdjacent",
    "weightedKappa",
    "correlation"
  )

  standardiseCols = function(df) {
    if (is.null(df) || nrow(df) == 0) {
      return(NULL)
    }

    for (nm in setdiff(allCols, names(df))) {
      df[[nm]] = NA_real_
    }

    df[, allCols, drop = FALSE]
  }

  disagreementList = Filter(
    Negate(is.null),
    list(
      standardiseCols(ordinal),
      standardiseCols(binary),
      standardiseCols(continuous)
    )
  )

  if (length(disagreementList) == 0) {
    disagreementDf = data.frame(
      metric = deterministicDf$metric,
      stringsAsFactors = FALSE
    )
  } else {
    disagreementDf = do.call(rbind, disagreementList)
    rownames(disagreementDf) = NULL
  }

  out = merge(
    deterministicDf,
    disagreementDf,
    by = "metric",
    all.x = TRUE,
    sort = FALSE
  )

  if (!is.null(orderBy)) {

    if (!orderBy %in% c("disagreement", "ease")) {
      stop("`orderBy` must be NULL, 'disagreement', or 'ease'.", call. = FALSE)
    }

    if (identical(orderBy, "disagreement") && "meanAbsoluteDifference" %in% names(out)) {
      out = out[order(-out$meanAbsoluteDifference, out$label), , drop = FALSE]
    }

    if (identical(orderBy, "ease")) {
      easeScore = -out$modalProportionDeterministic + out$entropyDeterministic
      out = out[order(easeScore, out$label), , drop = FALSE]
    }
  }

  rownames(out) = NULL

  structure(
    out,
    comparison = comparison,
    deterministicMethod = deterministicMethod,
    class = c("metricComparisonSummary", "data.frame")
  )
}


#' Build run-level paired comparison data for WMFM scores
#'
#' Creates run-level paired data for all metrics in the registry that are
#' present in two score data frames. The returned object is intended to
#' support downstream summaries and plotting.
#'
#' @param leftDf Long-form score data for the left method.
#' @param rightDf Long-form score data for the right method.
#' @param registry Metric registry.
#' @param leftMethod Name of the left method.
#' @param rightMethod Name of the right method.
#'
#' @return A data frame of run-level paired values.
#' @keywords internal
buildWmfmComparisonPairs = function(leftDf, rightDf, registry, leftMethod, rightMethod) {
  validateWmfmMetricRegistry(registry)

  merged = merge(
    leftDf,
    rightDf,
    by = "runId",
    suffixes = c(".x", ".y")
  )

  registry = registry[registry$includeInComparison, , drop = FALSE]
  out = vector("list", length = nrow(registry))
  nOut = 0

  for (i in seq_len(nrow(registry))) {
    metricName = registry$metricName[i]
    leftName = paste0(metricName, ".x")
    rightName = paste0(metricName, ".y")

    if (!(leftName %in% names(merged) && rightName %in% names(merged))) {
      next
    }

    nOut = nOut + 1
    out[[nOut]] = data.frame(
      runId = merged$runId,
      metric = metricName,
      label = registry$label[i],
      group = registry$group[i],
      metricType = registry$metricType[i],
      leftMethod = leftMethod,
      rightMethod = rightMethod,
      leftValue = merged[[leftName]],
      rightValue = merged[[rightName]],
      stringsAsFactors = FALSE
    )
  }

  out = out[seq_len(nOut)]

  if (length(out) == 0) {
    return(data.frame())
  }

  do.call(rbind, out)
}
