
#' Compare deterministic and LLM scoring results
#'
#' Compares two repeated-run objects, typically one scored deterministically and
#' one scored using the LLM, and summarises agreement and disagreement across
#' shared scoring fields.
#'
#' The two inputs are aligned by `runId` when that column is available in both
#' objects. Otherwise, rows are aligned by position.
#'
#' @param deterministicObj Repeated-run object containing deterministic scores.
#' @param llmObj Repeated-run object containing LLM scores.
#'
#' @return An object of class \code{"WmfmScoreComparison"} containing comparison
#'   summaries.
#' @export
compareScores = function(deterministicObj, llmObj) {

  if (!is.list(deterministicObj) || is.null(deterministicObj$runsDf) || !is.data.frame(deterministicObj$runsDf)) {
    stop("`deterministicObj` must contain a `runsDf` data frame.", call. = FALSE)
  }

  if (!is.list(llmObj) || is.null(llmObj$runsDf) || !is.data.frame(llmObj$runsDf)) {
    stop("`llmObj` must contain a `runsDf` data frame.", call. = FALSE)
  }

  detDf = deterministicObj$runsDf
  llmDf = llmObj$runsDf

  if ("runId" %in% names(detDf) && "runId" %in% names(llmDf)) {
    mergedDf = merge(
      detDf,
      llmDf,
      by = "runId",
      suffixes = c(".det", ".llm"),
      all = FALSE,
      sort = TRUE
    )
  } else {
    if (nrow(detDf) != nrow(llmDf)) {
      stop(
        "The two objects do not have the same number of runs and cannot be aligned by position.",
        call. = FALSE
      )
    }

    detDf$..rowIndexCompare = seq_len(nrow(detDf))
    llmDf$..rowIndexCompare = seq_len(nrow(llmDf))

    mergedDf = merge(
      detDf,
      llmDf,
      by = "..rowIndexCompare",
      suffixes = c(".det", ".llm"),
      all = FALSE,
      sort = TRUE
    )
  }

  if (nrow(mergedDf) == 0) {
    stop("No overlapping runs were found to compare.", call. = FALSE)
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

  getMergedFieldPair = function(field) {
    detName = paste0(field, ".det")
    llmName = paste0(field, ".llm")

    if (!(detName %in% names(mergedDf)) || !(llmName %in% names(mergedDf))) {
      return(NULL)
    }

    list(
      det = mergedDf[[detName]],
      llm = mergedDf[[llmName]]
    )
  }

  availableMetrics = Filter(
    function(field) {
      !is.null(getMergedFieldPair(field))
    },
    metricFields
  )

  if (length(availableMetrics) == 0) {
    stop(
      "No shared scoring columns were found between the two objects.",
      call. = FALSE
    )
  }

  agreementDf = data.frame(
    metric = character(0),
    nCompared = integer(0),
    nEqual = integer(0),
    proportionEqual = numeric(0),
    meanDifference = numeric(0),
    stringsAsFactors = FALSE
  )

  pairedOverallScores = NULL

  for (field in availableMetrics) {
    pair = getMergedFieldPair(field)
    det = pair$det
    llm = pair$llm

    ok = !(is.na(det) | is.na(llm))
    nCompared = sum(ok)

    if (nCompared == 0) {
      next
    }

    detOk = det[ok]
    llmOk = llm[ok]

    equalCount = sum(detOk == llmOk)

    meanDiff =
      if (is.numeric(detOk) && is.numeric(llmOk)) {
        mean(llmOk - detOk)
      } else if (is.logical(detOk) && is.logical(llmOk)) {
        mean(as.integer(llmOk) - as.integer(detOk))
      } else {
        NA_real_
      }

    agreementDf = rbind(
      agreementDf,
      data.frame(
        metric = field,
        nCompared = nCompared,
        nEqual = equalCount,
        proportionEqual = equalCount / nCompared,
        meanDifference = meanDiff,
        stringsAsFactors = FALSE
      )
    )

    if (identical(field, "overallScore")) {
      pairedOverallScores = data.frame(
        detOverallScore = as.numeric(detOk),
        llmOverallScore = as.numeric(llmOk),
        meanOverallScore = (as.numeric(detOk) + as.numeric(llmOk)) / 2,
        differenceOverallScore = as.numeric(llmOk) - as.numeric(detOk),
        stringsAsFactors = FALSE
      )
    }
  }

  overallSummary = NULL
  if (!is.null(pairedOverallScores) && nrow(pairedOverallScores) > 0) {
    diffVec = pairedOverallScores$differenceOverallScore
    overallSummary = list(
      meanDeterministicOverallScore = mean(pairedOverallScores$detOverallScore),
      meanLlmOverallScore = mean(pairedOverallScores$llmOverallScore),
      meanDifferenceLlmMinusDeterministic = mean(diffVec),
      sdDifferenceLlmMinusDeterministic = stats::sd(diffVec)
    )
  }

  out = list(
    metricAgreement = agreementDf,
    overallSummary = overallSummary,
    pairedOverallScores = pairedOverallScores,
    nRunsCompared = nrow(mergedDf),
    deterministicPrimaryMethod = deterministicObj$primaryScoringMethod %||% NA_character_,
    llmPrimaryMethod = llmObj$primaryScoringMethod %||% NA_character_
  )

  class(out) = c("WmfmScoreComparison", class(out))
  out
}
