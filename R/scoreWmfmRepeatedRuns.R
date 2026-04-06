#' Score repeated WMFM explanation runs using a multidimensional rubric
#'
#' Applies a rubric-based scoring framework to repeated WMFM explanation runs.
#' The scoring structure is designed to align with the revised
#' `buildWmfmRunRecord()` schema by separating:
#'
#' \enumerate{
#'   \item descriptive metadata about the run,
#'   \item extracted claim variables describing what the explanation said,
#'   \item judged quality variables describing whether those claims were
#'     appropriate, and
#'   \item aggregate dimension scores.
#' }
#'
#' The function accepts either:
#' \itemize{
#'   \item a data.frame of run records, or
#'   \item a list containing a `runsDf` element.
#' }
#'
#' ## Rubric dimensions
#'
#' The function scores explanations across five dimensions:
#'
#' \describe{
#'   \item{Factual score}{How well the explanation states the main effect,
#'   effect scale, reference-group structure, and interaction substance.}
#'   \item{Inference score}{How appropriate the explanation's inferential
#'   language is, especially for uncertainty and interaction evidence.}
#'   \item{Completeness score}{Whether the explanation covers the important
#'   ingredients that the model structure suggests should be present.}
#'   \item{Clarity score}{Whether the explanation is reasonably clear, sensibly
#'   expressed, and not excessively short or long.}
#'   \item{Calibration score}{Whether the explanation avoids overclaiming and
#'   severe underclaiming.}
#' }
#'
#' ## Judged fields created here
#'
#' This function fills or overwrites the following judged fields when they are
#' missing or `NA`:
#'
#' \describe{
#'   \item{effectDirectionCorrect}{Integer in `0`, `1`, `2`.}
#'   \item{effectScaleAppropriate}{Integer in `0`, `1`, `2`.}
#'   \item{referenceGroupHandledCorrectly}{Integer in `0`, `1`, `2`.}
#'   \item{interactionCoverageAdequate}{Integer in `0`, `1`, `2`.}
#'   \item{interactionSubstantiveCorrect}{Integer in `0`, `1`, `2`.}
#'   \item{uncertaintyHandlingAppropriate}{Integer in `0`, `1`, `2`.}
#'   \item{inferentialRegisterAppropriate}{Integer in `0`, `1`, `2`.}
#'   \item{mainEffectCoverageAdequate}{Integer in `0`, `1`, `2`.}
#'   \item{referenceGroupCoverageAdequate}{Integer in `0`, `1`, `2`.}
#'   \item{clarityAdequate}{Integer in `0`, `1`, `2`.}
#'   \item{numericExpressionAdequate}{Integer in `0`, `1`, `2`.}
#'   \item{comparisonStructureClear}{Integer in `0`, `1`, `2`.}
#'   \item{fatalFlawDetected}{Logical.}
#' }
#'
#' ## Aggregate fields created here
#'
#' \describe{
#'   \item{factualScore}{Numeric score on a `0` to `2` scale.}
#'   \item{inferenceScore}{Numeric score on a `0` to `2` scale.}
#'   \item{completenessScore}{Numeric score on a `0` to `2` scale.}
#'   \item{clarityScore}{Numeric score on a `0` to `2` scale.}
#'   \item{calibrationScore}{Numeric score on a `0` to `2` scale.}
#'   \item{overallScore}{Numeric weighted score on a `0` to `100` scale.}
#'   \item{overallPass}{Logical.}
#' }
#'
#' Duplicate detection is based on `normalizedExplanation` when available,
#' otherwise on trimmed explanation text.
#'
#' Fatal flaws do not force the score to zero, but they cap the final overall
#' score using `fatalFlawCap`.
#'
#' @param runsDf A data.frame of repeated-run outputs, or a list containing a
#'   `runsDf` element.
#' @param preferredMinWords Integer. Lower bound for a preferred explanation
#'   length band.
#' @param preferredMaxWords Integer. Upper bound for a preferred explanation
#'   length band.
#' @param penaliseDuplicates Logical. Should exact duplicate explanations be
#'   penalised?
#' @param duplicatePenalty Numeric penalty subtracted from the final
#'   `overallScore` for exact duplicates.
#' @param fatalFlawCap Numeric in `0` to `100`. Maximum allowed overall score
#'   when `fatalFlawDetected` is `TRUE`.
#' @param passThreshold Numeric in `0` to `100`. Threshold used to create
#'   `overallPass`.
#' @param factualWeight Numeric weight for the factual dimension.
#' @param inferenceWeight Numeric weight for the inference dimension.
#' @param completenessWeight Numeric weight for the completeness dimension.
#' @param clarityWeight Numeric weight for the clarity dimension.
#' @param calibrationWeight Numeric weight for the calibration dimension.
#'
#' @return A data.frame with judged quality columns and dimension scores added.
#' @export
scoreWmfmRepeatedRuns = function(
    runsDf,
    preferredMinWords = 80L,
    preferredMaxWords = 220L,
    penaliseDuplicates = TRUE,
    duplicatePenalty = 5,
    fatalFlawCap = 40,
    passThreshold = 65,
    factualWeight = 0.30,
    inferenceWeight = 0.25,
    completenessWeight = 0.20,
    clarityWeight = 0.15,
    calibrationWeight = 0.10
) {

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

  coalesceCharacter = function(...) {
    args = list(...)
    out = args[[1]]

    for (i in seq_along(args)) {
      current = args[[i]]
      missingIdx = is.na(out) | out == ""
      out[missingIdx] = current[missingIdx]
    }

    out
  }

  overwriteIfMissing = function(existing, computed) {
    if (length(existing) == 0) {
      return(computed)
    }

    out = existing
    replaceIdx = is.na(out)
    out[replaceIdx] = computed[replaceIdx]
    out
  }

  scoreFromKnownState = function(value, goodValues = character(0), partialValues = character(0), badValues = character(0), default = 1L) {
    out = rep(as.integer(default), length(value))
    out[value %in% badValues] = 0L
    out[value %in% partialValues] = 1L
    out[value %in% goodValues] = 2L
    out
  }

  meanIgnoringNa = function(...) {
    pieces = list(...)
    matrixIn = do.call(cbind, lapply(pieces, as.numeric))
    out = rowMeans(matrixIn, na.rm = TRUE)
    out[is.nan(out)] = NA_real_
    out
  }

  clamp01 = function(x) {
    x[x < 0] = 0
    x[x > 1] = 1
    x
  }

  runsDf = extractRunsDf(runsDf)

  explanationText = getCharacterColumn(runsDf, "explanationText", fallbackNames = c("explanation"), default = "")
  explanationPresent = nzchar(normaliseText(explanationText))

  wordCount = getIntegerColumn(runsDf, "wordCount")
  missingWordCountIdx = which(is.na(wordCount))

  if (length(missingWordCountIdx) > 0) {
    wordCount[missingWordCountIdx] = vapply(explanationText[missingWordCountIdx], countWordsLocal, integer(1))
  }

  hasError = getLogicalColumn(runsDf, "hasError")

  if (!"hasError" %in% names(runsDf) && "errorMessage" %in% names(runsDf)) {
    errorMessageText = normaliseText(getCharacterColumn(runsDf, "errorMessage", default = ""))
    hasError = nzchar(errorMessageText)
  }

  normalizedExplanation = normaliseText(getCharacterColumn(runsDf, "normalizedExplanation", default = ""))
  explanationKey = normalizedExplanation
  emptyKeyIdx = which(!nzchar(explanationKey))

  if (length(emptyKeyIdx) > 0) {
    explanationKey[emptyKeyIdx] = normaliseText(explanationText[emptyKeyIdx])
  }

  keyTable = table(explanationKey)
  duplicateCount = as.integer(keyTable[match(explanationKey, names(keyTable))])
  duplicateCount[!explanationPresent] = 0L
  isExactDuplicate = duplicateCount > 1L

  hasInteractionTerms = getLogicalColumn(runsDf, "hasInteractionTerms")

  if (!"hasInteractionTerms" %in% names(runsDf)) {
    nInteractionTerms = getIntegerColumn(runsDf, "nInteractionTerms", default = NA_integer_)
    interactionTermsText = normaliseText(getCharacterColumn(runsDf, "interactionTerms", default = ""))
    hasInteractionTerms = (!is.na(nInteractionTerms) & nInteractionTerms > 0L) | nzchar(interactionTermsText)
  }

  interactionMinPValue = getNumericColumn(runsDf, "interactionMinPValue")
  interactionAlpha = getNumericColumn(runsDf, "interactionAlpha", default = 0.05)
  interactionAlpha[is.na(interactionAlpha)] = 0.05

  ciMention = getLogicalColumn(runsDf, "ciMention", fallbackNames = c("mentionsConfidenceInterval"))
  percentLanguageMention = getLogicalColumn(runsDf, "percentLanguageMention", fallbackNames = c("usesPercentLanguage"))
  referenceGroupMention = getLogicalColumn(runsDf, "referenceGroupMention", fallbackNames = c("mentionsReferenceGroup"))
  interactionMention = getLogicalColumn(runsDf, "interactionMention", fallbackNames = c("mentionsInteraction"))
  uncertaintyMention = getLogicalColumn(runsDf, "uncertaintyMention", fallbackNames = c("uncertaintyMentioned"))
  usesInferentialLanguage = getLogicalColumn(runsDf, "usesInferentialLanguage")
  usesDescriptiveOnlyLanguage = getLogicalColumn(runsDf, "usesDescriptiveOnlyLanguage")
  overclaimDetected = getLogicalColumn(runsDf, "overclaimDetected")
  underclaimDetected = getLogicalColumn(runsDf, "underclaimDetected")
  conditionalLanguageMention = getLogicalColumn(runsDf, "conditionalLanguageMention")
  comparisonLanguageMention = getLogicalColumn(runsDf, "comparisonLanguageMention")
  outcomeMention = getLogicalColumn(runsDf, "outcomeMention")
  predictorMention = getLogicalColumn(runsDf, "predictorMention")

  effectDirectionClaim = getCharacterColumn(runsDf, "effectDirectionClaim", fallbackNames = c("effectDirection"))
  effectScaleClaim = getCharacterColumn(runsDf, "effectScaleClaim", fallbackNames = c("effectScale"))
  interactionSubstantiveClaim = getCharacterColumn(runsDf, "interactionSubstantiveClaim", fallbackNames = c("interactionClaim"))
  inferentialRegister = getCharacterColumn(runsDf, "inferentialRegister", fallbackNames = c("inferentialStyle"))
  uncertaintyTypeClaim = getCharacterColumn(runsDf, "uncertaintyTypeClaim")
  interactionEvidenceAppropriate = getCharacterColumn(runsDf, "interactionEvidenceAppropriate", fallbackNames = c("interactionInference"))

  expectedEffectDirection = getCharacterColumn(runsDf, "expectedEffectDirection")
  expectedEffectScale = getCharacterColumn(runsDf, "expectedEffectScale")

  effectDirectionCorrectExisting = getIntegerColumn(runsDf, "effectDirectionCorrect")
  effectScaleAppropriateExisting = getIntegerColumn(runsDf, "effectScaleAppropriate")
  referenceGroupHandledCorrectlyExisting = getIntegerColumn(runsDf, "referenceGroupHandledCorrectly")
  interactionCoverageAdequateExisting = getIntegerColumn(runsDf, "interactionCoverageAdequate")
  interactionSubstantiveCorrectExisting = getIntegerColumn(runsDf, "interactionSubstantiveCorrect")
  uncertaintyHandlingAppropriateExisting = getIntegerColumn(runsDf, "uncertaintyHandlingAppropriate")
  inferentialRegisterAppropriateExisting = getIntegerColumn(runsDf, "inferentialRegisterAppropriate")
  mainEffectCoverageAdequateExisting = getIntegerColumn(runsDf, "mainEffectCoverageAdequate")
  referenceGroupCoverageAdequateExisting = getIntegerColumn(runsDf, "referenceGroupCoverageAdequate")
  clarityAdequateExisting = getIntegerColumn(runsDf, "clarityAdequate")
  numericExpressionAdequateExisting = getIntegerColumn(runsDf, "numericExpressionAdequate")
  comparisonStructureClearExisting = getIntegerColumn(runsDf, "comparisonStructureClear")
  fatalFlawDetectedExisting = getLogicalColumn(runsDf, "fatalFlawDetected", default = NA)

  effectDirectionCorrectComputed = rep(NA_integer_, nrow(runsDf))
  hasExpectedDirection = !is.na(expectedEffectDirection) & expectedEffectDirection != ""
  matchesExpectedDirection = effectDirectionClaim == expectedEffectDirection
  effectDirectionCorrectComputed[hasExpectedDirection & matchesExpectedDirection] = 2L
  effectDirectionCorrectComputed[hasExpectedDirection & !matchesExpectedDirection & effectDirectionClaim == "not_stated"] = 0L
  effectDirectionCorrectComputed[hasExpectedDirection & !matchesExpectedDirection & effectDirectionClaim != "not_stated"] = 0L
  effectDirectionCorrectComputed[!hasExpectedDirection & effectDirectionClaim %in% c("increase", "decrease")] = 2L
  effectDirectionCorrectComputed[!hasExpectedDirection & effectDirectionClaim == "mixed_or_both"] = 1L
  effectDirectionCorrectComputed[!hasExpectedDirection & effectDirectionClaim == "not_stated"] = 0L
  effectDirectionCorrect = overwriteIfMissing(effectDirectionCorrectExisting, effectDirectionCorrectComputed)

  effectScaleAppropriateComputed = rep(NA_integer_, nrow(runsDf))
  hasExpectedScale = !is.na(expectedEffectScale) & expectedEffectScale != ""
  effectScaleAppropriateComputed[hasExpectedScale & effectScaleClaim == expectedEffectScale] = 2L
  effectScaleAppropriateComputed[hasExpectedScale & effectScaleClaim %in% c("mixed_or_unclear", "not_stated")] = 0L
  effectScaleAppropriateComputed[hasExpectedScale & !(effectScaleClaim %in% c(expectedEffectScale, "mixed_or_unclear", "not_stated"))] = 0L
  effectScaleAppropriateComputed[!hasExpectedScale & effectScaleClaim %in% c("additive", "multiplicative", "probability_or_odds")] = 2L
  effectScaleAppropriateComputed[!hasExpectedScale & effectScaleClaim == "mixed_or_unclear"] = 1L
  effectScaleAppropriateComputed[!hasExpectedScale & effectScaleClaim == "not_stated"] = 0L
  effectScaleAppropriate = overwriteIfMissing(effectScaleAppropriateExisting, effectScaleAppropriateComputed)

  referenceGroupHandledCorrectlyComputed = rep(1L, nrow(runsDf))
  referenceGroupHandledCorrectlyComputed[referenceGroupMention] = 2L
  referenceGroupHandledCorrectlyComputed[!referenceGroupMention & hasInteractionTerms] = 0L
  referenceGroupHandledCorrectlyComputed[!referenceGroupMention & !hasInteractionTerms] = 1L
  referenceGroupHandledCorrectly = overwriteIfMissing(referenceGroupHandledCorrectlyExisting, referenceGroupHandledCorrectlyComputed)

  interactionCoverageAdequateComputed = rep(2L, nrow(runsDf))
  interactionCoverageAdequateComputed[hasInteractionTerms & !interactionMention & interactionSubstantiveClaim %in% c("not_mentioned", "", NA)] = 0L
  interactionCoverageAdequateComputed[hasInteractionTerms & (interactionMention | interactionSubstantiveClaim != "not_mentioned") & !(comparisonLanguageMention | conditionalLanguageMention)] = 1L
  interactionCoverageAdequateComputed[hasInteractionTerms & (interactionMention | interactionSubstantiveClaim != "not_mentioned") & (comparisonLanguageMention | conditionalLanguageMention)] = 2L
  interactionCoverageAdequateComputed[!hasInteractionTerms & interactionMention] = 1L
  interactionCoverageAdequate = overwriteIfMissing(interactionCoverageAdequateExisting, interactionCoverageAdequateComputed)

  interactionSubstantiveCorrectComputed = rep(2L, nrow(runsDf))
  interactionSubstantiveCorrectComputed[hasInteractionTerms & interactionSubstantiveClaim == "not_mentioned"] = 0L
  interactionSubstantiveCorrectComputed[hasInteractionTerms & interactionSubstantiveClaim == "unclear"] = 1L
  interactionSubstantiveCorrectComputed[hasInteractionTerms & interactionSubstantiveClaim == "no_clear_difference"] = 1L
  interactionSubstantiveCorrectComputed[hasInteractionTerms & interactionSubstantiveClaim %in% c("difference_claimed_cautiously", "difference_claimed_strongly")] = 2L
  interactionSubstantiveCorrectComputed[!hasInteractionTerms & interactionSubstantiveClaim %in% c("difference_claimed_cautiously", "difference_claimed_strongly")] = 0L
  interactionSubstantiveCorrectComputed[!hasInteractionTerms & interactionSubstantiveClaim %in% c("no_clear_difference", "not_mentioned", "not_applicable", "unclear", "")] = 2L
  interactionSubstantiveCorrect = overwriteIfMissing(interactionSubstantiveCorrectExisting, interactionSubstantiveCorrectComputed)

  interactionEvidenceAppropriateComputed = rep("unclear", nrow(runsDf))
  interactionEvidenceAppropriateComputed[!hasInteractionTerms] = "not_applicable"
  significantInteraction = hasInteractionTerms & !is.na(interactionMinPValue) & interactionMinPValue <= interactionAlpha
  weakInteraction = hasInteractionTerms & !is.na(interactionMinPValue) & interactionMinPValue > interactionAlpha

  interactionEvidenceAppropriateComputed[significantInteraction & interactionSubstantiveClaim %in% c("difference_claimed_cautiously", "difference_claimed_strongly")] = "appropriate"
  interactionEvidenceAppropriateComputed[significantInteraction & interactionSubstantiveClaim == "not_mentioned"] = "too_weak"
  interactionEvidenceAppropriateComputed[significantInteraction & interactionSubstantiveClaim == "no_clear_difference"] = "too_weak"
  interactionEvidenceAppropriateComputed[weakInteraction & interactionSubstantiveClaim == "difference_claimed_strongly"] = "too_strong"
  interactionEvidenceAppropriateComputed[weakInteraction & interactionSubstantiveClaim == "difference_claimed_cautiously"] = "appropriate"
  interactionEvidenceAppropriateComputed[weakInteraction & interactionSubstantiveClaim %in% c("no_clear_difference", "not_mentioned", "unclear")] = "appropriate"
  interactionEvidenceAppropriateComputed[hasInteractionTerms & is.na(interactionMinPValue) & interactionSubstantiveClaim %in% c("difference_claimed_cautiously", "difference_claimed_strongly", "no_clear_difference")] = "unclear"

  needsInteractionEvidenceFill = is.na(interactionEvidenceAppropriate) | interactionEvidenceAppropriate == ""
  interactionEvidenceAppropriate[needsInteractionEvidenceFill] = interactionEvidenceAppropriateComputed[needsInteractionEvidenceFill]

  uncertaintyHandlingAppropriateComputed = rep(1L, nrow(runsDf))
  uncertaintyHandlingAppropriateComputed[ciMention] = 2L
  uncertaintyHandlingAppropriateComputed[uncertaintyMention & !ciMention] = 2L
  uncertaintyHandlingAppropriateComputed[overclaimDetected & !uncertaintyMention] = 0L
  uncertaintyHandlingAppropriateComputed[!uncertaintyMention & inferentialRegister == "inferential"] = 1L
  uncertaintyHandlingAppropriateComputed[!uncertaintyMention & inferentialRegister == "overclaiming"] = 0L
  uncertaintyHandlingAppropriate = overwriteIfMissing(uncertaintyHandlingAppropriateExisting, uncertaintyHandlingAppropriateComputed)

  inferentialRegisterAppropriateComputed = rep(1L, nrow(runsDf))
  inferentialRegisterAppropriateComputed[inferentialRegister == "inferential" & (uncertaintyMention | ciMention | usesInferentialLanguage)] = 2L
  inferentialRegisterAppropriateComputed[inferentialRegister == "descriptive_only" & hasInteractionTerms & !is.na(interactionMinPValue) & interactionMinPValue <= interactionAlpha] = 1L
  inferentialRegisterAppropriateComputed[inferentialRegister == "descriptive_only" & !hasInteractionTerms] = 2L
  inferentialRegisterAppropriateComputed[inferentialRegister == "overclaiming"] = 0L
  inferentialRegisterAppropriateComputed[overclaimDetected] = 0L
  inferentialRegisterAppropriate = overwriteIfMissing(inferentialRegisterAppropriateExisting, inferentialRegisterAppropriateComputed)

  mainEffectCoverageAdequateComputed = rep(0L, nrow(runsDf))
  mainEffectCoverageAdequateComputed[effectDirectionClaim != "not_stated"] = 1L
  mainEffectCoverageAdequateComputed[effectDirectionClaim != "not_stated" & effectScaleClaim != "not_stated"] = 2L
  mainEffectCoverageAdequateComputed[!explanationPresent] = 0L
  mainEffectCoverageAdequate = overwriteIfMissing(mainEffectCoverageAdequateExisting, mainEffectCoverageAdequateComputed)

  referenceGroupCoverageAdequateComputed = rep(1L, nrow(runsDf))
  referenceGroupCoverageAdequateComputed[referenceGroupMention] = 2L
  referenceGroupCoverageAdequateComputed[hasInteractionTerms & !referenceGroupMention] = 0L
  referenceGroupCoverageAdequate = overwriteIfMissing(referenceGroupCoverageAdequateExisting, referenceGroupCoverageAdequateComputed)

  hasNumericDigits = grepl("\\d", explanationText, perl = TRUE)
  hasNumberWords = grepl(
    paste(
      "\\bone\\b",
      "\\btwo\\b",
      "\\bthree\\b",
      "\\bfour\\b",
      "\\bfive\\b",
      "\\bsix\\b",
      "\\bseven\\b",
      "\\beight\\b",
      "\\bnine\\b",
      "\\bten\\b",
      "\\beleven\\b",
      "\\btwelve\\b",
      "\\bhalf\\b",
      sep = "|"
    ),
    explanationText,
    ignore.case = TRUE,
    perl = TRUE
  )
  hasNumericMagnitude = hasNumericDigits | hasNumberWords

  additiveNumericPattern = paste(
    "\\bpoint(s)?\\b",
    "\\bhigher\\b",
    "\\blower\\b",
    "\\braises?\\b",
    "\\bboosts?\\b",
    "\\bincrease(s|d)?\\b",
    "\\bdecrease(s|d)?\\b",
    sep = "|"
  )
  additiveNumericMention = grepl(
    additiveNumericPattern,
    explanationText,
    ignore.case = TRUE,
    perl = TRUE
  )

  numericExpressionAdequateComputed = rep(0L, nrow(runsDf))
  numericExpressionAdequateComputed[hasNumericMagnitude] = 1L
  numericExpressionAdequateComputed[
    effectScaleClaim %in% c("additive", "probability_or_odds", "multiplicative") &
      hasNumericMagnitude
  ] = 2L
  numericExpressionAdequateComputed[
    effectScaleClaim == "mixed_or_unclear" &
      hasNumericMagnitude
  ] = 1L
  numericExpressionAdequateComputed[
    effectScaleClaim == "mixed_or_unclear" &
      hasNumericMagnitude &
      additiveNumericMention &
      (comparisonLanguageMention | conditionalLanguageMention | percentLanguageMention)
  ] = 2L
  numericExpressionAdequateComputed[
    effectScaleClaim == "not_stated" &
      hasNumericMagnitude &
      additiveNumericMention
  ] = 1L
  numericExpressionAdequate = overwriteIfMissing(numericExpressionAdequateExisting, numericExpressionAdequateComputed)

  comparisonStructureClearComputed = rep(1L, nrow(runsDf))
  comparisonStructureClearComputed[comparisonLanguageMention | conditionalLanguageMention] = 2L
  comparisonStructureClearComputed[hasInteractionTerms & !(comparisonLanguageMention | conditionalLanguageMention)] = 0L
  comparisonStructureClear = overwriteIfMissing(comparisonStructureClearExisting, comparisonStructureClearComputed)

  clarityAdequateComputed = rep(1L, nrow(runsDf))
  preferredLength = wordCount >= as.integer(preferredMinWords) & wordCount <= as.integer(preferredMaxWords)
  clarityAdequateComputed[preferredLength] = 2L
  clarityAdequateComputed[wordCount < max(20L, as.integer(preferredMinWords %/% 2))] = 0L
  clarityAdequateComputed[wordCount > as.integer(preferredMaxWords * 1.5)] = 0L
  clarityAdequateComputed[comparisonStructureClear == 2L & numericExpressionAdequate == 2L & preferredLength] = 2L
  clarityAdequateComputed[!explanationPresent] = 0L
  clarityAdequate = overwriteIfMissing(clarityAdequateExisting, clarityAdequateComputed)

  fatalFlawDetectedComputed = rep(FALSE, nrow(runsDf))
  fatalFlawDetectedComputed[hasError] = TRUE
  fatalFlawDetectedComputed[!explanationPresent] = TRUE
  fatalFlawDetectedComputed[overclaimDetected] = TRUE
  fatalFlawDetectedComputed[interactionEvidenceAppropriate == "too_strong"] = TRUE
  fatalFlawDetectedComputed[hasInteractionTerms & interactionCoverageAdequate == 0L] = TRUE
  fatalFlawDetectedComputed[mainEffectCoverageAdequate == 0L] = TRUE
  fatalFlawDetected = fatalFlawDetectedExisting
  fillFatalIdx = is.na(fatalFlawDetected)
  fatalFlawDetected[fillFatalIdx] = fatalFlawDetectedComputed[fillFatalIdx]

  factualScore = meanIgnoringNa(
    effectDirectionCorrect,
    effectScaleAppropriate,
    referenceGroupHandledCorrectly,
    interactionSubstantiveCorrect
  )

  interactionEvidenceNumeric = scoreFromKnownState(
    interactionEvidenceAppropriate,
    goodValues = c("appropriate", "not_applicable"),
    partialValues = c("unclear"),
    badValues = c("too_strong", "too_weak"),
    default = 1L
  )

  inferenceScore = meanIgnoringNa(
    uncertaintyHandlingAppropriate,
    inferentialRegisterAppropriate,
    interactionEvidenceNumeric
  )

  completenessScore = meanIgnoringNa(
    mainEffectCoverageAdequate,
    interactionCoverageAdequate,
    referenceGroupCoverageAdequate,
    ifelse(uncertaintyMention | ciMention, 2, 1)
  )

  clarityScore = meanIgnoringNa(
    clarityAdequate,
    numericExpressionAdequate,
    comparisonStructureClear,
    ifelse(outcomeMention | predictorMention, 2, 1)
  )

  calibrationScore = rep(2, nrow(runsDf))
  calibrationScore[underclaimDetected] = 1
  calibrationScore[overclaimDetected] = 0

  totalWeight = factualWeight + inferenceWeight + completenessWeight + clarityWeight + calibrationWeight

  if (totalWeight <= 0) {
    stop("Rubric weights must sum to a positive value.", call. = FALSE)
  }

  weightedScore01 = (
    factualWeight * (factualScore / 2) +
      inferenceWeight * (inferenceScore / 2) +
      completenessWeight * (completenessScore / 2) +
      clarityWeight * (clarityScore / 2) +
      calibrationWeight * (calibrationScore / 2)
  ) / totalWeight

  weightedScore01[is.na(weightedScore01)] = 0
  overallScore = round(100 * clamp01(weightedScore01), 1)

  if (isTRUE(penaliseDuplicates)) {
    overallScore[isExactDuplicate] = overallScore[isExactDuplicate] - as.numeric(duplicatePenalty)
  }

  overallScore = pmax(0, overallScore)
  overallScore[fatalFlawDetected] = pmin(overallScore[fatalFlawDetected], as.numeric(fatalFlawCap))
  overallPass = overallScore >= as.numeric(passThreshold) & !fatalFlawDetected

  scoredDf = runsDf
  scoredDf$explanationTextDerived = explanationText
  scoredDf$wordCountDerived = wordCount
  scoredDf$hasErrorDerived = hasError
  scoredDf$explanationPresent = explanationPresent
  scoredDf$duplicateCount = duplicateCount
  scoredDf$isExactDuplicate = isExactDuplicate
  scoredDf$duplicatePenaltyApplied = ifelse(isExactDuplicate & isTRUE(penaliseDuplicates), as.numeric(duplicatePenalty), 0)

  scoredDf$interactionEvidenceAppropriate = interactionEvidenceAppropriate
  scoredDf$effectDirectionCorrect = effectDirectionCorrect
  scoredDf$effectScaleAppropriate = effectScaleAppropriate
  scoredDf$referenceGroupHandledCorrectly = referenceGroupHandledCorrectly
  scoredDf$interactionCoverageAdequate = interactionCoverageAdequate
  scoredDf$interactionSubstantiveCorrect = interactionSubstantiveCorrect
  scoredDf$uncertaintyHandlingAppropriate = uncertaintyHandlingAppropriate
  scoredDf$inferentialRegisterAppropriate = inferentialRegisterAppropriate
  scoredDf$mainEffectCoverageAdequate = mainEffectCoverageAdequate
  scoredDf$referenceGroupCoverageAdequate = referenceGroupCoverageAdequate
  scoredDf$clarityAdequate = clarityAdequate
  scoredDf$numericExpressionAdequate = numericExpressionAdequate
  scoredDf$comparisonStructureClear = comparisonStructureClear
  scoredDf$fatalFlawDetected = fatalFlawDetected

  scoredDf$factualScore = round(factualScore, 3)
  scoredDf$inferenceScore = round(inferenceScore, 3)
  scoredDf$completenessScore = round(completenessScore, 3)
  scoredDf$clarityScore = round(clarityScore, 3)
  scoredDf$calibrationScore = round(calibrationScore, 3)
  scoredDf$overallScore = overallScore
  scoredDf$overallPass = overallPass

  scoredDf
}
