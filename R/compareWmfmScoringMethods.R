#' Compare deterministic and LLM WMFM scoring results
#'
#' Summarises agreement and disagreement between deterministic and LLM scoring
#' columns stored in a repeated-run object.
#'
#' @param x Object produced by `runWMFMPackageExampleRepeated()` and optionally
#'   updated by `rescoreWmfmRepeatedRunsWithOpposingMethod()`.
#'
#' @return A named list containing comparison summaries.
#' @export
compareWmfmScoringMethods = function(x) {

  if (!is.list(x) || is.null(x$runsDf) || !is.data.frame(x$runsDf)) {
    stop("`x` must contain a `runsDf` data frame.", call. = FALSE)
  }

  runsDf = x$runsDf

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

  getMethodColumnName = function(method, field) {
    prefix = if (identical(method, "deterministic")) "det_" else "llm_"
    paste0(prefix, field)
  }

  getFieldVector = function(method, field) {
    prefixedName = getMethodColumnName(method, field)

    if (prefixedName %in% names(runsDf)) {
      return(runsDf[[prefixedName]])
    }

    if (field %in% names(runsDf) && identical(x$primaryScoringMethod, method)) {
      return(runsDf[[field]])
    }

    NULL
  }

  availableMetrics = Filter(
    function(field) {
      !is.null(getFieldVector("deterministic", field)) &&
        !is.null(getFieldVector("llm", field))
    },
    metricFields
  )

  if (length(availableMetrics) == 0) {
    stop(
      "No paired deterministic/LLM scoring columns were found.",
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

  for (field in availableMetrics) {
    det = getFieldVector("deterministic", field)
    llm = getFieldVector("llm", field)

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
  }

  overallDet = getFieldVector("deterministic", "overallScore")
  overallLlm = getFieldVector("llm", "overallScore")

  overallSummary = NULL
  if (!is.null(overallDet) && !is.null(overallLlm)) {
    ok = !(is.na(overallDet) | is.na(overallLlm))
    if (any(ok)) {
      overallSummary = list(
        meanDeterministicOverallScore = mean(overallDet[ok]),
        meanLlmOverallScore = mean(overallLlm[ok]),
        meanDifferenceLlmMinusDeterministic = mean(overallLlm[ok] - overallDet[ok])
      )
    }
  }

  list(
    primaryScoringMethod = x$primaryScoringMethod %||% NA_character_,
    opposingScoringMethod = x$opposingScoringMethod %||% NA_character_,
    metricAgreement = agreementDf,
    overallSummary = overallSummary
  )
}
