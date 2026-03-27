#' Summarise repeated WMFM explanation runs
#'
#' Produces a set-level summary of repeated WMFM explanation runs using the
#' revised run-record schema. The summary is designed to cover two distinct
#' questions:
#'
#' \enumerate{
#'   \item How variable were the explanations across repeated runs?
#'   \item How stable and how strong was explanation quality across runs?
#' }
#'
#' The function accepts either:
#' \itemize{
#'   \item a data.frame of run records, or
#'   \item a list containing a `runsDf` element.
#' }
#'
#' Exact duplication is assessed using `normalizedExplanation` when available,
#' otherwise using trimmed explanation text.
#'
#' ## Summary structure
#'
#' The returned list contains five top-level sections:
#'
#' \describe{
#'   \item{runCounts}{Counts of runs, valid explanations, unique explanations,
#'   and error rate.}
#'   \item{textVariability}{Duplicate rate, variability flag, and word/sentence
#'   summaries.}
#'   \item{claimSummaries}{Rates and distributions for extracted claim fields
#'   such as interaction mention, uncertainty mention, and inferential
#'   register.}
#'   \item{qualitySummaries}{Summaries of judged quality fields and aggregate
#'   dimension scores such as factual, inference, completeness, clarity,
#'   calibration, and overall score.}
#'   \item{qualityStability}{Cross-run stability indicators for key judged
#'   fields and score variability measures.}
#' }
#'
#' The function is written to work primarily with the revised schema introduced
#' in `buildWmfmRunRecord()`, but it also recognises several older field names
#' where doing so is easy and low-risk.
#'
#' @param runsDf A data.frame of run records, or a list containing a `runsDf`
#'   element.
#'
#' @return A named list containing repeated-run variability and quality
#'   summaries.
#' @export
summariseWmfmRepeatedRuns = function(runsDf) {

  extractRunsDf = function(x) {
    if (is.data.frame(x)) {
      return(x)
    }

    if (is.list(x) && "runsDf" %in% names(x) && is.data.frame(x$runsDf)) {
      return(x$runsDf)
    }

    stop(
      "`runsDf` must be a data.frame or a list containing a data.frame named `runsDf`.",
      call. = FALSE
    )
  }

  normaliseText = function(x) {
    x = as.character(x)
    x[is.na(x)] = ""
    trimws(x)
  }

  countWordsLocal = function(x) {
    x = trimws(x)

    if (!nzchar(x)) {
      return(0L)
    }

    length(strsplit(x, "[[:space:]]+")[[1]])
  }

  getCharacterColumn = function(df, primaryName, fallbackNames = character(0), default = NA_character_) {
    candidateNames = c(primaryName, fallbackNames)
    candidateNames = candidateNames[candidateNames %in% names(df)]

    if (length(candidateNames) == 0) {
      return(rep(default, nrow(df)))
    }

    out = as.character(df[[candidateNames[1]]])
    out[is.na(out)] = default
    out
  }

  getLogicalColumn = function(df, primaryName, fallbackNames = character(0), default = FALSE) {
    candidateNames = c(primaryName, fallbackNames)
    candidateNames = candidateNames[candidateNames %in% names(df)]

    if (length(candidateNames) == 0) {
      return(rep(default, nrow(df)))
    }

    out = as.logical(df[[candidateNames[1]]])
    out[is.na(out)] = default
    out
  }

  getNumericColumn = function(df, primaryName, fallbackNames = character(0), default = NA_real_) {
    candidateNames = c(primaryName, fallbackNames)
    candidateNames = candidateNames[candidateNames %in% names(df)]

    if (length(candidateNames) == 0) {
      return(rep(default, nrow(df)))
    }

    suppressWarnings(as.numeric(df[[candidateNames[1]]]))
  }

  getIntegerColumn = function(df, primaryName, fallbackNames = character(0), default = NA_integer_) {
    out = getNumericColumn(df, primaryName, fallbackNames, default = default)
    suppressWarnings(as.integer(out))
  }

  summarizeNumeric = function(x) {
    x = as.numeric(x)
    valid = x[!is.na(x)]

    if (length(valid) == 0) {
      return(list(
        n = 0L,
        mean = NA_real_,
        median = NA_real_,
        sd = NA_real_,
        min = NA_real_,
        max = NA_real_
      ))
    }

    list(
      n = length(valid),
      mean = mean(valid),
      median = stats::median(valid),
      sd = stats::sd(valid),
      min = min(valid),
      max = max(valid)
    )
  }

  summarizeLogical = function(x) {
    x = as.logical(x)
    x = x[!is.na(x)]

    if (length(x) == 0) {
      return(list(
        n = 0L,
        trueRate = NA_real_,
        variesAcrossRuns = NA
      ))
    }

    list(
      n = length(x),
      trueRate = mean(x),
      variesAcrossRuns = length(unique(x)) > 1
    )
  }

  summarizeCategory = function(x) {
    x = as.character(x)
    x[is.na(x) | !nzchar(trimws(x))] = "missing"

    list(
      counts = sort(table(x), decreasing = TRUE),
      variesAcrossRuns = length(unique(x)) > 1
    )
  }

  scaledStability = function(x) {
    x = x[!is.na(x)]

    if (length(x) == 0) {
      return(NA_real_)
    }

    tab = table(x)
    as.numeric(max(tab)) / length(x)
  }

  classifyVariability = function(propUniqueExplanations, nUniqueExplanations) {
    if (is.na(propUniqueExplanations)) {
      return("no valid explanations")
    }

    if (nUniqueExplanations == 1) {
      return("all explanations identical")
    }

    if (propUniqueExplanations <= 0.25) {
      return("low variability")
    }

    if (propUniqueExplanations <= 0.75) {
      return("moderate variability")
    }

    "high variability"
  }

  runsDf = extractRunsDf(runsDf)

  explanationText = getCharacterColumn(
    runsDf,
    "explanationText",
    fallbackNames = c("explanation"),
    default = ""
  )
  explanationPresent = nzchar(normaliseText(explanationText))

  normalizedExplanation = normaliseText(
    getCharacterColumn(runsDf, "normalizedExplanation", default = "")
  )
  explanationKey = normalizedExplanation
  emptyKeyIdx = which(!nzchar(explanationKey))

  if (length(emptyKeyIdx) > 0) {
    explanationKey[emptyKeyIdx] = normaliseText(explanationText[emptyKeyIdx])
  }

  wordCount = getIntegerColumn(runsDf, "wordCount")
  missingWordCountIdx = which(is.na(wordCount))

  if (length(missingWordCountIdx) > 0) {
    wordCount[missingWordCountIdx] = vapply(
      explanationText[missingWordCountIdx],
      countWordsLocal,
      integer(1)
    )
  }

  sentenceCount = getIntegerColumn(runsDf, "sentenceCount")
  explanationKeysValid = explanationKey[explanationPresent]

  hasError = getLogicalColumn(runsDf, "hasError", default = FALSE)

  if (!"hasError" %in% names(runsDf) && "errorMessage" %in% names(runsDf)) {
    errorMessageText = normaliseText(getCharacterColumn(runsDf, "errorMessage", default = ""))
    hasError = nzchar(errorMessageText)
  }

  if (length(explanationKeysValid) == 0) {
    return(list(
      runCounts = list(
        nRuns = nrow(runsDf),
        nValidExplanations = 0L,
        nUniqueExplanations = 0L,
        errorRate = mean(hasError)
      ),
      textVariability = list(
        propUniqueExplanations = NA_real_,
        mostCommonExplanationCount = NA_integer_,
        mostCommonExplanationProportion = NA_real_,
        exactDuplicateRate = NA_real_,
        variabilityFlag = "no valid explanations",
        wordCount = summarizeNumeric(numeric(0)),
        sentenceCount = summarizeNumeric(numeric(0))
      ),
      claimSummaries = list(),
      qualitySummaries = list(),
      qualityStability = list()
    ))
  }

  explanationTable = sort(table(explanationKeysValid), decreasing = TRUE)

  nValidExplanations = length(explanationKeysValid)
  nUniqueExplanations = length(explanationTable)
  propUniqueExplanations = nUniqueExplanations / nValidExplanations
  mostCommonExplanationCount = as.integer(explanationTable[1])
  mostCommonExplanationProportion = as.numeric(explanationTable[1]) / nValidExplanations
  exactDuplicateRate = 1 - propUniqueExplanations
  variabilityFlag = classifyVariability(propUniqueExplanations, nUniqueExplanations)

  out = list(
    runCounts = list(
      nRuns = nrow(runsDf),
      nValidExplanations = nValidExplanations,
      nUniqueExplanations = nUniqueExplanations,
      errorRate = mean(hasError)
    ),
    textVariability = list(
      propUniqueExplanations = propUniqueExplanations,
      mostCommonExplanationCount = mostCommonExplanationCount,
      mostCommonExplanationProportion = mostCommonExplanationProportion,
      exactDuplicateRate = exactDuplicateRate,
      variabilityFlag = variabilityFlag,
      wordCount = summarizeNumeric(wordCount[explanationPresent]),
      sentenceCount = summarizeNumeric(sentenceCount[explanationPresent])
    ),
    claimSummaries = list(),
    qualitySummaries = list(),
    qualityStability = list()
  )

  claimLogicalSpecs = list(
    ciMention = c("mentionsConfidenceInterval"),
    percentLanguageMention = c("usesPercentLanguage"),
    referenceGroupMention = c("mentionsReferenceGroup"),
    interactionMention = c("mentionsInteraction"),
    uncertaintyMention = c("uncertaintyMentioned"),
    usesInferentialLanguage = character(0),
    usesDescriptiveOnlyLanguage = character(0),
    overclaimDetected = character(0),
    underclaimDetected = character(0),
    conditionalLanguageMention = character(0),
    comparisonLanguageMention = character(0),
    outcomeMention = character(0),
    predictorMention = character(0)
  )

  for (name in names(claimLogicalSpecs)) {
    fallbackNames = claimLogicalSpecs[[name]]

    if (name %in% names(runsDf) || any(fallbackNames %in% names(runsDf))) {
      out$claimSummaries[[name]] = summarizeLogical(
        getLogicalColumn(runsDf, name, fallbackNames = fallbackNames)[explanationPresent]
      )
    }
  }

  claimCategorySpecs = list(
    effectDirectionClaim = c("effectDirection"),
    effectScaleClaim = c("effectScale"),
    interactionSubstantiveClaim = c("interactionClaim"),
    inferentialRegister = c("inferentialStyle"),
    uncertaintyTypeClaim = character(0)
  )

  for (name in names(claimCategorySpecs)) {
    fallbackNames = claimCategorySpecs[[name]]

    if (name %in% names(runsDf) || any(fallbackNames %in% names(runsDf))) {
      values = getCharacterColumn(runsDf, name, fallbackNames = fallbackNames)
      out$claimSummaries[[name]] = summarizeCategory(values[explanationPresent])
    }
  }

  qualityNumericFields = c(
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
    "overallScore"
  )

  for (fieldName in qualityNumericFields) {
    if (fieldName %in% names(runsDf)) {
      values = getNumericColumn(runsDf, fieldName)
      out$qualitySummaries[[fieldName]] = summarizeNumeric(values[explanationPresent])
    }
  }

  qualityLogicalFields = c(
    "fatalFlawDetected",
    "overallPass"
  )

  for (fieldName in qualityLogicalFields) {
    if (fieldName %in% names(runsDf)) {
      values = getLogicalColumn(runsDf, fieldName)
      out$qualitySummaries[[fieldName]] = summarizeLogical(values[explanationPresent])
    }
  }

  if ("interactionEvidenceAppropriate" %in% names(runsDf) || "interactionInference" %in% names(runsDf)) {
    interactionEvidenceValues = getCharacterColumn(
      runsDf,
      "interactionEvidenceAppropriate",
      fallbackNames = c("interactionInference")
    )
    out$qualitySummaries$interactionEvidenceAppropriate = summarizeCategory(
      interactionEvidenceValues[explanationPresent]
    )
  }

  stabilityCategorySpecs = list(
    effectDirectionClaim = c("effectDirection"),
    interactionSubstantiveClaim = c("interactionClaim"),
    interactionEvidenceAppropriate = c("interactionInference"),
    inferentialRegister = c("inferentialStyle")
  )

  for (name in names(stabilityCategorySpecs)) {
    fallbackNames = stabilityCategorySpecs[[name]]

    if (name %in% names(runsDf) || any(fallbackNames %in% names(runsDf))) {
      values = getCharacterColumn(runsDf, name, fallbackNames = fallbackNames)
      values = values[explanationPresent]
      values[is.na(values) | !nzchar(trimws(values))] = "missing"

      out$qualityStability[[name]] = list(
        stability = scaledStability(values),
        variesAcrossRuns = length(unique(values)) > 1,
        modalValue = names(sort(table(values), decreasing = TRUE))[1]
      )
    }
  }

  stabilityScoreFields = c(
    "factualScore",
    "inferenceScore",
    "completenessScore",
    "clarityScore",
    "calibrationScore",
    "overallScore"
  )

  for (fieldName in stabilityScoreFields) {
    if (fieldName %in% names(runsDf)) {
      values = getNumericColumn(runsDf, fieldName)[explanationPresent]
      valid = values[!is.na(values)]

      if (length(valid) > 0) {
        out$qualityStability[[fieldName]] = list(
          sd = stats::sd(valid),
          min = min(valid),
          max = max(valid),
          range = max(valid) - min(valid)
        )
      } else {
        out$qualityStability[[fieldName]] = list(
          sd = NA_real_,
          min = NA_real_,
          max = NA_real_,
          range = NA_real_
        )
      }
    }
  }

  out
}
