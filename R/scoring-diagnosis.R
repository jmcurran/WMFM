#' Classify the effect scale described in a WMFM explanation
#'
#' Uses simple text heuristics to classify the scale on which the explanation
#' describes the main effect. The intent is to identify the dominant scale used
#' for coefficient interpretation, rather than unrelated numeric summaries such
#' as model-fit percentages.
#'
#' The classifier is deliberately conservative about assigning
#' \code{"mixed_or_unclear"}. In particular, generic percentage language such as
#' an \eqn{R^2} statement should not by itself force a multiplicative label when
#' the coefficient interpretation is otherwise clearly additive.
#'
#' @param text Character scalar explanation text.
#'
#' @return One of \code{"additive"}, \code{"multiplicative"},
#'   \code{"probability_or_odds"}, \code{"mixed_or_unclear"}, or
#'   \code{"not_stated"}.
#' @export
classifyEffectScaleClaim = function(text) {

  detectPatternLocal = function(text, pattern) {
    if (is.na(text) || !nzchar(trimws(text))) {
      return(FALSE)
    }

    grepl(pattern, text, ignore.case = TRUE, perl = TRUE)
  }

  if (length(text) == 0) {
    return("not_stated")
  }

  text = as.character(text)[1]

  if (is.na(text) || !nzchar(trimws(text))) {
    return("not_stated")
  }

  additivePattern = paste(
    "\\bpoint(s)?\\b",
    "\\bunit(s)?\\b",
    "\\bhigher by\\b",
    "\\blower by\\b",
    "\\braises? .* by\\b",
    "\\bincrease(s|d)? .* by\\b",
    "\\bdecrease(s|d)? .* by\\b",
    "\\bboost(s|ed)? .* by\\b",
    "\\bassociated with .* points? higher\\b",
    "\\bassociated with .* points? lower\\b",
    "\\babout .* points? higher\\b",
    "\\babout .* points? lower\\b",
    "\\beach (extra|additional) point\\b",
    "\\bfor each (extra|additional|one|1)[ -]?unit\\b",
    "\\bfor each (extra|additional) point\\b",
    "\\bdifference of\\b",
    "\\bincrease of\\b",
    "\\bdecrease of\\b",
    sep = "|"
  )

  multiplicativePattern = paste(
    "\\bmultipl(?:y|ies|ied|ier|iers)?\\b",
    "\\bmultiplier\\b",
    "\\btimes\\b",
    "\\bfold\\b",
    "\\brate ratio\\b",
    "\\bodds ratio\\b",
    "\\brelative risk\\b",
    "\\brisk ratio\\b",
    "\\bincidence rate ratio\\b",
    "\\bchanges? by .*%\\b",
    "\\bincrease(s|d)? by .*%\\b",
    "\\bdecrease(s|d)? by .*%\\b",
    sep = "|"
  )

  probabilityPattern = paste(
    "\\bodds\\b",
    "\\bprobability\\b",
    "\\bchance\\b",
    "\\blikelihood\\b",
    sep = "|"
  )

  additiveDetected = detectPatternLocal(text, additivePattern)
  multiplicativeDetected = detectPatternLocal(text, multiplicativePattern)
  probabilityDetected = detectPatternLocal(text, probabilityPattern)

  nDetected = sum(c(additiveDetected, multiplicativeDetected, probabilityDetected))

  if (nDetected > 1) {
    return("mixed_or_unclear")
  }

  if (additiveDetected) {
    return("additive")
  }

  if (multiplicativeDetected) {
    return("multiplicative")
  }

  if (probabilityDetected) {
    return("probability_or_odds")
  }

  "not_stated"
}


#' Build metric-specific evidence summary for diagnosis
#'
#' Internal helper for \code{diagnose()} that creates a compact,
#' metric-aware evidence summary from the run-level diagnosis data.
#'
#' The initial implementation focuses on fields that are especially useful
#' for diagnosing disagreement on \code{numericExpressionAdequate}. For other
#' metrics, the function returns a lighter generic summary.
#'
#' @param metricDf Run-level metric diagnosis data.
#' @param metric Metric name.
#'
#' @return A one-row data frame containing metric-specific summary fields.
#'
#' @keywords internal
buildMetricEvidenceSummary = function(metricDf, metric) {
  nRuns = nrow(metricDf)

  if (nRuns < 1L) {
    return(data.frame(metric = metric, stringsAsFactors = FALSE))
  }

  countValue = function(x, value) {
    if (is.null(x)) {
      return(NA_integer_)
    }

    sum(as.character(x) == as.character(value), na.rm = TRUE)
  }

  propTrue = function(x) {
    if (is.null(x)) {
      return(NA_real_)
    }

    mean(as.logical(x), na.rm = TRUE)
  }

  firstPresent = function(df, candidates) {
    hits = intersect(candidates, names(df))
    if (length(hits) < 1L) {
      return(NULL)
    }

    df[[hits[1L]]]
  }

  nDisagree = sum(metricDf$disagrees, na.rm = TRUE)

  out = data.frame(
    metric = metric,
    nRuns = nRuns,
    nDisagree = nDisagree,
    stringsAsFactors = FALSE
  )

  if (identical(metric, "numericExpressionAdequate")) {
    effectScaleClaim = firstPresent(metricDf, "effectScaleClaim")
    percentLanguageMention = firstPresent(metricDf, "percentLanguageMention")
    comparisonLanguageMention = firstPresent(metricDf, "comparisonLanguageMention")
    conditionalLanguageMention = firstPresent(metricDf, "conditionalLanguageMention")

    out$effectScale_notStated_n = countValue(effectScaleClaim, "not_stated")
    out$effectScale_mixedOrUnclear_n = countValue(effectScaleClaim, "mixed_or_unclear")
    out$effectScale_additive_n = countValue(effectScaleClaim, "additive")
    out$effectScale_multiplicative_n = countValue(effectScaleClaim, "multiplicative")
    out$percentLanguageMention_rate = propTrue(percentLanguageMention)
    out$comparisonLanguageMention_rate = propTrue(comparisonLanguageMention)
    out$conditionalLanguageMention_rate = propTrue(conditionalLanguageMention)

    likelyIssue = NA_character_

    if (nDisagree > 0L) {
      likelyIssue = "insufficientContext"

      if (!is.null(effectScaleClaim)) {
        notStatedRate = out$effectScale_notStated_n / nRuns
        mixedRate = out$effectScale_mixedOrUnclear_n / nRuns
        additiveRate = out$effectScale_additive_n / nRuns
      } else {
        notStatedRate = NA_real_
        mixedRate = NA_real_
        additiveRate = NA_real_
      }

      percentRate = out$percentLanguageMention_rate
      comparisonRate = out$comparisonLanguageMention_rate
      conditionalRate = out$conditionalLanguageMention_rate

      if (!is.na(notStatedRate) && notStatedRate >= 0.6) {
        likelyIssue = "effectScaleExtractionOftenMissing"
      } else if (!is.na(mixedRate) && mixedRate >= 0.6) {
        likelyIssue = "effectScaleExtractionOftenUnclear"
      } else if (!is.na(additiveRate) &&
                 additiveRate >= 0.4 &&
                 !is.na(percentRate) &&
                 percentRate < 0.2) {
        likelyIssue = "ruleMayUndervalueAdditiveNumericLanguage"
      } else if (!is.na(comparisonRate) &&
                 !is.na(conditionalRate) &&
                 (comparisonRate >= 0.5 || conditionalRate >= 0.5)) {
        likelyIssue = "numericContentPresentButRuleInputsMayMissIt"
      }
    }

    out$likelyIssue = likelyIssue
    return(out)
  }

  out$likelyIssue =
    if (nDisagree > 0L) {
      "metricSpecificEvidenceNotYetImplemented"
    } else {
      NA_character_
    }

  out
}


#' Extract contextual run-level information for metric diagnosis
#'
#' Internal helper for \code{diagnose()} that attempts to recover
#' explanation text and diagnostic context. Preference is given to the
#' original runs object, because \code{wmfmScores} retains score fields
#' but not the full raw explanation record.
#'
#' @param scores A \code{wmfmScores} object.
#' @param metric A single metric name.
#' @param runs Optional \code{wmfmRuns} object or compatible list/data.frame.
#'
#' @return A data frame keyed by \code{runId}, or \code{NULL}.
#'
#' @keywords internal
extractMetricDiagnosisContext = function(scores, metric, runs = NULL) {
  asRunsDf = function(x) {
    if (is.null(x)) {
      return(NULL)
    }

    if (is.data.frame(x)) {
      out = x
      if (!"runId" %in% names(out)) {
        out$runId = seq_len(nrow(out))
      }
      return(out)
    }

    if (inherits(x, "wmfmRuns") && !is.null(x$runs)) {
      out = do.call(
        rbind,
        lapply(x$runs, function(run) {
          as.data.frame(run, stringsAsFactors = FALSE)
        })
      )
      rownames(out) = NULL
      out$runId = seq_len(nrow(out))
      return(out)
    }

    if (is.list(x) && !is.null(x$runsDf) && is.data.frame(x$runsDf)) {
      out = x$runsDf
      if (!"runId" %in% names(out)) {
        out$runId = seq_len(nrow(out))
      }
      return(out)
    }

    NULL
  }

  runsDf = asRunsDf(runs)

  if (is.null(runsDf)) {
    return(NULL)
  }

  keepCols = intersect(
    c(
      "runId",
      "explanationText",
      "explanation",
      "formula",
      "equationsText",
      "interactionTerms",
      "interactionMinPValue",
      "effectDirectionClaim",
      "effectScaleClaim",
      "interactionSubstantiveClaim",
      "inferentialRegister",
      "percentLanguageMention",
      "comparisonLanguageMention",
      "conditionalLanguageMention",
      "referenceGroupMention",
      "uncertaintyMention",
      "ciMention",
      "overclaimDetected",
      "underclaimDetected",
      "hasInteractionTerms",
      "expectedEffectScale",
      "expectedEffectDirection"
    ),
    names(runsDf)
  )

  if (length(keepCols) < 1L) {
    return(NULL)
  }

  runsDf[, keepCols, drop = FALSE]
}


#' List metrics that can be diagnosed from a wmfmScores object
#'
#' Internal helper for \code{diagnose()}.
#'
#' @param scores A \code{wmfmScores} object.
#'
#' @return A character vector of metric names present in both deterministic and
#'   LLM score records and having at least one non-missing numeric value on
#'   each side.
#'
#' @keywords internal
listDiagnosableMetrics = function(scores) {
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


#' Compute a priority score for metric diagnosis
#'
#' Internal helper that ranks metrics for follow-up. Higher values indicate
#' more systematic and potentially more important disagreement.
#'
#' @param summaryTable A data frame of per-metric diagnosis summaries.
#'
#' @return A numeric vector of priority scores.
#'
#' @keywords internal
computeDiagnosisPriorityScore = function(summaryTable) {
  if (!is.data.frame(summaryTable) || nrow(summaryTable) < 1L) {
    return(numeric(0))
  }

  disagreementTerm = 100 * summaryTable$disagreementRate
  biasTerm = 20 * abs(summaryTable$meanLlmMinusDet)
  consistencyTerm = 20 * summaryTable$directionConsistency
  constantGapBonus = ifelse(
    isTRUE(summaryTable$detConstant) & isTRUE(summaryTable$llmConstant),
    15,
    0
  )

  disagreementTerm + biasTerm + consistencyTerm + constantGapBonus
}


#' Resolve run-level data for metric diagnosis
#'
#' Internal helper for \code{diagnose()}.
#'
#' @param scores A \code{wmfmScores} object.
#' @param metric A single metric name.
#' @param cmp Optional \code{wmfmScoreComparison} object.
#' @param runs Optional \code{wmfmRuns} object or compatible run container.
#'
#' @return A data frame with one row per run.
#'
#' @keywords internal
resolveMetricDiagnosisData = function(scores, metric, cmp = NULL, runs = NULL) {
  if (!is.null(cmp)) {
    if (!exists("getMetricComparisonData", mode = "function")) {
      stop("getMetricComparisonData() is required when cmp is supplied.")
    }

    metricDf = getMetricComparisonData(cmp, metric)
  } else {
    metricDf = deriveMetricComparisonDataFromScores(scores = scores, metric = metric)
  }

  if (!is.data.frame(metricDf) || nrow(metricDf) < 1L) {
    stop("Could not resolve run-level comparison data for metric: ", metric)
  }

  requiredCols = c("runId", "detValue", "llmValue")
  missingCols = setdiff(requiredCols, names(metricDf))
  if (length(missingCols) > 0L) {
    stop(
      "Metric comparison data is missing required columns: ",
      paste(missingCols, collapse = ", ")
    )
  }

  metricDf$detMinusLlm = metricDf$detValue - metricDf$llmValue
  metricDf$llmMinusDet = metricDf$llmValue - metricDf$detValue
  metricDf$disagrees = metricDf$detValue != metricDf$llmValue
  metricDf$disagreementDirection = ifelse(
    metricDf$llmValue > metricDf$detValue,
    "llmHigher",
    ifelse(metricDf$llmValue < metricDf$detValue, "detHigher", "same")
  )

  extraDf = extractMetricDiagnosisContext(
    scores = scores,
    metric = metric,
    runs = runs
  )

  if (!is.null(extraDf) && is.data.frame(extraDf) && "runId" %in% names(extraDf)) {
    metricDf = merge(
      metricDf,
      extraDf,
      by = "runId",
      all.x = TRUE,
      sort = FALSE
    )
  }

  metricDf[order(metricDf$runId), , drop = FALSE]
}


#' Select flagged runs for metric diagnosis
#'
#' Internal helper for \code{diagnose()} that selects the most informative
#' disagreement cases for inspection.
#'
#' @param metricDf Run-level metric diagnosis data.
#' @param maxExamples Maximum number of examples to return.
#'
#' @return A data frame.
#'
#' @keywords internal
selectFlaggedMetricRuns = function(metricDf, maxExamples = 5) {
  flaggedDf = metricDf[metricDf$disagrees, , drop = FALSE]

  if (nrow(flaggedDf) < 1L) {
    return(flaggedDf)
  }

  flaggedDf$absDifference = abs(flaggedDf$llmMinusDet)
  flaggedDf = flaggedDf[order(-flaggedDf$absDifference, flaggedDf$runId), , drop = FALSE]

  utils::head(flaggedDf, maxExamples)
}


#' Build summary statistics for metric diagnosis
#'
#' Internal helper for \code{diagnose()}.
#'
#' @param metricDf Run-level metric diagnosis data.
#' @param metric Metric name.
#'
#' @return A one-row data frame.
#'
#' @keywords internal
buildMetricDiagnosisSummary = function(metricDf, metric) {
  nRuns = nrow(metricDf)
  nDisagree = sum(metricDf$disagrees, na.rm = TRUE)

  llmHigherCount = sum(metricDf$disagreementDirection == "llmHigher", na.rm = TRUE)
  detHigherCount = sum(metricDf$disagreementDirection == "detHigher", na.rm = TRUE)

  detNonMissing = metricDf$detValue[!is.na(metricDf$detValue)]
  llmNonMissing = metricDf$llmValue[!is.na(metricDf$llmValue)]

  detUnique = sort(unique(detNonMissing))
  llmUnique = sort(unique(llmNonMissing))

  safeMean = function(x) {
    if (length(x) < 1L || all(is.na(x))) {
      return(NA_real_)
    }

    mean(x, na.rm = TRUE)
  }

  safeMedian = function(x) {
    if (length(x) < 1L || all(is.na(x))) {
      return(NA_real_)
    }

    stats::median(x, na.rm = TRUE)
  }

  safeMin = function(x) {
    if (length(x) < 1L) {
      return(NA_real_)
    }

    min(x)
  }

  safeMax = function(x) {
    if (length(x) < 1L) {
      return(NA_real_)
    }

    max(x)
  }

  out = data.frame(
    metric = metric,
    nRuns = nRuns,
    nDisagree = nDisagree,
    disagreementRate = if (nRuns > 0L) nDisagree / nRuns else NA_real_,
    meanDet = safeMean(metricDf$detValue),
    meanLlm = safeMean(metricDf$llmValue),
    meanLlmMinusDet = safeMean(metricDf$llmMinusDet),
    medianLlmMinusDet = safeMedian(metricDf$llmMinusDet),
    llmHigherCount = llmHigherCount,
    detHigherCount = detHigherCount,
    directionConsistency = max(llmHigherCount, detHigherCount) / max(1, nDisagree),
    detConstant = length(detUnique) == 1L && length(detUnique) > 0L,
    llmConstant = length(llmUnique) == 1L && length(llmUnique) > 0L,
    detMin = safeMin(detNonMissing),
    detMax = safeMax(detNonMissing),
    llmMin = safeMin(llmNonMissing),
    llmMax = safeMax(llmNonMissing),
    stringsAsFactors = FALSE
  )

  out
}
