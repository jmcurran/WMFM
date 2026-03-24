#' Summarise repeated WMFM explanation runs
#'
#' Produces a set-level summary of repeated explanation runs, with emphasis on
#' variability in explanation text rather than equation generation.
#'
#' This function is tolerant of different input shapes. `runsDf` may be either:
#' \itemize{
#'   \item a data.frame of run records, or
#'   \item a list returned by `runWMFMPackageExampleRepeated()` containing a
#'   `runsDf` element.
#' }
#'
#' Exact duplication is assessed using `normalizedExplanation` when available,
#' otherwise using trimmed explanation text.
#'
#' @param runsDf A data.frame of run records, or a list containing a `runsDf`
#'   element.
#'
#' @return A named list summary.
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

  runsDf = extractRunsDf(runsDf)

  getExplanationText = function(df) {
    if ("explanationText" %in% names(df)) {
      x = df$explanationText
    } else if ("explanation" %in% names(df)) {
      x = df$explanation
    } else {
      stop(
        "Run records must contain either `explanationText` or `explanation`.",
        call. = FALSE
      )
    }

    x = as.character(x)
    x[is.na(x)] = ""
    x
  }

  countWords = function(x) {
    x = trimws(x)

    if (!nzchar(x)) {
      return(0L)
    }

    parts = strsplit(x, "[[:space:]]+")[[1]]
    length(parts)
  }

  getWordCount = function(df, explanationText) {
    if ("wordCount" %in% names(df)) {
      out = suppressWarnings(as.integer(df$wordCount))
      missingIdx = which(is.na(out))

      if (length(missingIdx) > 0) {
        out[missingIdx] = vapply(explanationText[missingIdx], countWords, integer(1))
      }

      return(out)
    }

    vapply(explanationText, countWords, integer(1))
  }

  getExplanationKey = function(df, explanationText) {
    if ("normalizedExplanation" %in% names(df)) {
      key = as.character(df$normalizedExplanation)
      key[is.na(key)] = ""
      return(trimws(key))
    }

    trimws(explanationText)
  }

  explanationText = getExplanationText(runsDf)
  explanationPresent = nzchar(trimws(explanationText))
  wordCount = getWordCount(runsDf, explanationText)
  explanationKey = getExplanationKey(runsDf, explanationText)

  validKeys = explanationKey[explanationPresent]

  if (length(validKeys) == 0) {
    return(list(
      nRuns = nrow(runsDf),
      nValidExplanations = 0L,
      nUniqueExplanations = 0L,
      propUniqueExplanations = NA_real_,
      mostCommonExplanationCount = NA_integer_,
      mostCommonExplanationProportion = NA_real_,
      exactDuplicateRate = NA_real_,
      meanWords = NA_real_,
      sdWords = NA_real_,
      minWords = NA_real_,
      maxWords = NA_real_,
      variabilityFlag = "no valid explanations"
    ))
  }

  tbl = sort(table(validKeys), decreasing = TRUE)

  nValidExplanations = length(validKeys)
  nUniqueExplanations = length(tbl)
  propUniqueExplanations = nUniqueExplanations / nValidExplanations
  mostCommonExplanationCount = as.integer(tbl[1])
  mostCommonExplanationProportion = as.numeric(tbl[1]) / nValidExplanations
  exactDuplicateRate = 1 - propUniqueExplanations

  variabilityFlag = if (nUniqueExplanations == 1) {
    "all explanations identical"
  } else if (propUniqueExplanations <= 0.25) {
    "low variability"
  } else if (propUniqueExplanations <= 0.75) {
    "moderate variability"
  } else {
    "high variability"
  }

  out = list(
    nRuns = nrow(runsDf),
    nValidExplanations = nValidExplanations,
    nUniqueExplanations = nUniqueExplanations,
    propUniqueExplanations = propUniqueExplanations,
    mostCommonExplanationCount = mostCommonExplanationCount,
    mostCommonExplanationProportion = mostCommonExplanationProportion,
    exactDuplicateRate = exactDuplicateRate,
    variabilityFlag = variabilityFlag,
    meanWords = mean(wordCount[explanationPresent], na.rm = TRUE),
    sdWords = stats::sd(wordCount[explanationPresent], na.rm = TRUE),
    minWords = min(wordCount[explanationPresent], na.rm = TRUE),
    maxWords = max(wordCount[explanationPresent], na.rm = TRUE)
  )

  if ("sentenceCount" %in% names(runsDf)) {
    out$meanSentences = mean(runsDf$sentenceCount[explanationPresent], na.rm = TRUE)
    out$sdSentences = stats::sd(runsDf$sentenceCount[explanationPresent], na.rm = TRUE)
    out$minSentences = min(runsDf$sentenceCount[explanationPresent], na.rm = TRUE)
    out$maxSentences = max(runsDf$sentenceCount[explanationPresent], na.rm = TRUE)
  }

  featureNames = c(
    "mentionsCi",
    "usesPercentLanguage",
    "mentionsReferenceGroup",
    "mentionsInteraction"
  )

  for (featureName in featureNames) {
    if (featureName %in% names(runsDf)) {
      featureValues = as.logical(runsDf[[featureName]][explanationPresent])
      featureValues[is.na(featureValues)] = FALSE

      out[[paste0(featureName, "Rate")]] = mean(featureValues)
      out[[paste0(featureName, "VariesAcrossRuns")]] = length(unique(featureValues)) > 1
    }
  }

  if ("hasError" %in% names(runsDf)) {
    hasError = as.logical(runsDf$hasError)
    hasError[is.na(hasError)] = FALSE
    out$errorRate = mean(hasError)
  } else if ("errorMessage" %in% names(runsDf)) {
    msg = as.character(runsDf$errorMessage)
    msg[is.na(msg)] = ""
    out$errorRate = mean(nzchar(trimws(msg)))
  }

  out
}
