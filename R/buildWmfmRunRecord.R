#' Build a single run record
#'
#' Creates a structured record for one repeated WMFM run, including raw text,
#' simple text summaries, heuristic semantic-claim fields, and interaction
#' evidence fields derived from the fitted model.
#'
#' The interaction fields separate:
#' \itemize{
#'   \item what the explanation claims about the interaction, and
#'   \item whether that claim is appropriate relative to the interaction-term
#'   p-value.
#' }
#'
#' @param runId Integer.
#' @param exampleName Character.
#' @param package Character.
#' @param modelType Character.
#' @param formula Character.
#' @param equationsText Character.
#' @param explanationText Character.
#' @param errorMessage Character.
#' @param interactionTerms Character vector of interaction-term names from the
#'   fitted model.
#' @param interactionMinPValue Numeric. Minimum p-value across fitted
#'   interaction terms, or `NA` if no interaction terms are present or no
#'   p-value could be extracted.
#' @param interactionAlpha Numeric. Threshold used to judge whether the
#'   explanation's interaction interpretation is appropriate.
#'
#' @return Named list.
#' @export
buildWmfmRunRecord = function(
    runId,
    exampleName,
    package,
    modelType,
    formula,
    equationsText,
    explanationText,
    errorMessage = NA_character_,
    interactionTerms = character(0),
    interactionMinPValue = NA_real_,
    interactionAlpha = 0.05
) {

  detectPatternLocal = function(text, pattern) {
    if (is.na(text) || !nzchar(trimws(text))) {
      return(FALSE)
    }

    grepl(pattern, text, ignore.case = TRUE, perl = TRUE)
  }

  classifyEffectDirection = function(text) {
    if (is.na(text) || !nzchar(trimws(text))) {
      return("not_stated")
    }

    decreasePattern = paste(
      "\\bdecrease(s|d)?\\b",
      "\\bdecline(s|d)?\\b",
      "\\bdeclines?\\b",
      "\\breduce(s|d)?\\b",
      "\\breduction\\b",
      "\\bdrop(s|ped)?\\b",
      "\\bfall(s|en)?\\b",
      "\\blower\\b",
      "\\bsmaller\\b",
      "\\brare(r)?\\b",
      "\\bfalls to\\b",
      "\\bcut(s|ting)?\\b",
      sep = "|"
    )

    increasePattern = paste(
      "\\bincrease(s|d)?\\b",
      "\\bincreasing\\b",
      "\\brise(s|n)?\\b",
      "\\bgrow(s|n|th)?\\b",
      "\\bhigher\\b",
      "\\blarger\\b",
      "\\bgreater\\b",
      "\\bmore likely\\b",
      "\\bmore frequent\\b",
      "\\bmore abundant\\b",
      sep = "|"
    )

    decreaseDetected = detectPatternLocal(text, decreasePattern)
    increaseDetected = detectPatternLocal(text, increasePattern)

    if (decreaseDetected && increaseDetected) {
      return("mixed_or_both")
    }

    if (decreaseDetected) {
      return("decrease")
    }

    if (increaseDetected) {
      return("increase")
    }

    "not_stated"
  }

  classifyEffectScale = function(text) {
    if (is.na(text) || !nzchar(trimws(text))) {
      return("not_stated")
    }

    multiplicativePattern = paste(
      "\\bmultipl(?:y|ies|ied|ier|iers)?\\b",
      "\\bmultiplier\\b",
      "\\btimes\\b",
      "\\bfold\\b",
      "\\bpercent\\b",
      "%",
      "\\brate ratio\\b",
      "\\bodds ratio\\b",
      "\\bof its previous\\b",
      "\\bfalls to about\\b",
      "\\bexpected count\\b",
      "\\bexpected number\\b",
      sep = "|"
    )

    probabilityPattern = paste(
      "\\bodds\\b",
      "\\bprobability\\b",
      "\\bchance\\b",
      "\\blikelihood\\b",
      sep = "|"
    )

    additivePattern = paste(
      "\\bunit(s)?\\b",
      "\\bpoint(s)?\\b",
      "\\bhigher by\\b",
      "\\blower by\\b",
      "\\bincrease of\\b",
      "\\bdecrease of\\b",
      "\\bdifference of\\b",
      sep = "|"
    )

    multiplicativeDetected = detectPatternLocal(text, multiplicativePattern)
    probabilityDetected = detectPatternLocal(text, probabilityPattern)
    additiveDetected = detectPatternLocal(text, additivePattern)

    nDetected = sum(c(multiplicativeDetected, probabilityDetected, additiveDetected))

    if (nDetected > 1) {
      return("mixed_or_unclear")
    }

    if (multiplicativeDetected) {
      return("multiplicative")
    }

    if (probabilityDetected) {
      return("probability_or_odds")
    }

    if (additiveDetected) {
      return("additive")
    }

    "not_stated"
  }

  classifyInteractionClaim = function(text, hasInteractionTerms) {
    if (!isTRUE(hasInteractionTerms)) {
      return("not_applicable")
    }

    if (is.na(text) || !nzchar(trimws(text))) {
      return("not_mentioned")
    }

    strongDifferencePattern = paste(
      "\\bsteeper\\b",
      "\\bstronger\\b",
      "\\bmore pronounced\\b",
      "\\bespecially pronounced\\b",
      "\\bclearly different\\b",
      "\\bdrops? more rapidly\\b",
      "\\bdiffers? markedly\\b",
      sep = "|"
    )

    cautiousDifferencePattern = paste(
      "\\bevidence.*differ",
      "\\bsuggest(s|ed)?.*differ",
      "\\bindicate(s|d)?.*differ",
      "\\beffect differs by\\b",
      "\\bdepends on\\b",
      "\\bvaries by\\b",
      "\\binteraction\\b",
      "\\bdifferent slope\\b",
      sep = "|"
    )

    noClearDifferencePattern = paste(
      "\\blittle evidence\\b",
      "\\bno clear evidence\\b",
      "\\bno strong evidence\\b",
      "\\bdoes not clearly show\\b",
      "\\bnot clearly different\\b",
      "\\bno clear interaction\\b",
      "\\binteraction.*weak\\b",
      sep = "|"
    )

    strongDetected = detectPatternLocal(text, strongDifferencePattern)
    cautiousDetected = detectPatternLocal(text, cautiousDifferencePattern)
    noClearDetected = detectPatternLocal(text, noClearDifferencePattern)

    if ((strongDetected || cautiousDetected) && noClearDetected) {
      return("unclear")
    }

    if (strongDetected) {
      return("difference_claimed_strongly")
    }

    if (cautiousDetected) {
      return("difference_claimed_cautiously")
    }

    if (noClearDetected) {
      return("no_clear_difference")
    }

    "not_mentioned"
  }

  classifyInteractionInference = function(
      interactionClaim,
      interactionMinPValue,
      hasInteractionTerms,
      interactionAlpha
  ) {
    if (!isTRUE(hasInteractionTerms)) {
      return("not_applicable")
    }

    if (is.na(interactionMinPValue)) {
      return("unclear")
    }

    if (interactionClaim == "unclear") {
      return("unclear")
    }

    if (interactionMinPValue <= interactionAlpha) {
      if (interactionClaim %in% c("difference_claimed_strongly", "difference_claimed_cautiously")) {
        return("appropriate")
      }

      if (interactionClaim %in% c("no_clear_difference", "not_mentioned")) {
        return("too_weak")
      }

      return("unclear")
    }

    if (interactionClaim %in% c("difference_claimed_strongly", "difference_claimed_cautiously")) {
      return("too_strong")
    }

    if (interactionClaim %in% c("no_clear_difference", "not_mentioned")) {
      return("appropriate")
    }

    "unclear"
  }

  classifyInferentialStyle = function(
      usesInferentialLanguage,
      usesDescriptiveOnlyLanguage,
      overclaimDetected
  ) {
    if (isTRUE(overclaimDetected)) {
      return("overclaiming")
    }

    if (isTRUE(usesInferentialLanguage)) {
      return("inferential")
    }

    if (isTRUE(usesDescriptiveOnlyLanguage)) {
      return("descriptive_only")
    }

    "unclear"
  }

  explanationText = as.character(explanationText)
  equationsText = as.character(equationsText)
  errorMessage = as.character(errorMessage)
  interactionTerms = as.character(interactionTerms)

  if (length(explanationText) == 0 || is.na(explanationText)) {
    explanationText = NA_character_
  }

  if (length(equationsText) == 0 || is.na(equationsText)) {
    equationsText = NA_character_
  }

  if (length(errorMessage) == 0 || identical(errorMessage, "NA")) {
    errorMessage = NA_character_
  }

  if (length(interactionTerms) == 1 && is.na(interactionTerms)) {
    interactionTerms = character(0)
  }

  normalizedExplanation = normalizeWmfmText(explanationText)
  wordCount = countWmfmWords(explanationText)
  sentenceCount = countWmfmSentences(explanationText)

  mentionsConfidenceInterval = detectPatternLocal(
    explanationText,
    "confidence interval|95%|95 %|credible interval|interval estimate"
  )

  usesPercentLanguage = detectPatternLocal(
    explanationText,
    "percent|%"
  )

  mentionsReferenceGroup = detectPatternLocal(
    explanationText,
    paste(
      "reference group",
      "reference category",
      "reference level",
      "baseline",
      "compared with the reference",
      "relative to the reference",
      sep = "|"
    )
  )

  mentionsInteraction = detectPatternLocal(
    explanationText,
    paste(
      "interaction",
      "effect differs by",
      "depends on",
      "varies by",
      "different slope",
      "steeper",
      "shallower",
      sep = "|"
    )
  )

  uncertaintyMentioned = detectPatternLocal(
    explanationText,
    paste(
      "confidence interval",
      "95%",
      "95 %",
      "\\buncertain",
      "\\buncertainty",
      "\\bplausible",
      "\\bconsistent with the data",
      "\\bevidence",
      "\\bsuggest(s|ed)?\\b",
      "\\bestimate(d)?\\b",
      sep = "|"
    )
  )

  usesInferentialLanguage = detectPatternLocal(
    explanationText,
    paste(
      "\\bevidence\\b",
      "\\bsuggest(s|ed)?\\b",
      "\\bindicate(s|d)?\\b",
      "\\bconsistent with the data\\b",
      "\\bestimate(d)?\\b",
      "\\bconfidence interval\\b",
      sep = "|"
    )
  )

  overclaimDetected = detectPatternLocal(
    explanationText,
    paste(
      "\\bprove(s|d)?\\b",
      "\\bdefinitely\\b",
      "\\bguarantee(s|d)?\\b",
      "\\bcauses?\\b",
      "\\bleads to\\b",
      "\\bresults in\\b",
      sep = "|"
    )
  )

  usesDescriptiveOnlyLanguage =
    !usesInferentialLanguage &&
    detectPatternLocal(
      explanationText,
      paste(
        "\\bthe data show\\b",
        "\\bthe study examined\\b",
        "\\bon average\\b",
        "\\bthere were\\b",
        "\\bthere are\\b",
        sep = "|"
      )
    )

  effectDirection = classifyEffectDirection(explanationText)
  effectScale = classifyEffectScale(explanationText)
  hasInteractionTerms = length(interactionTerms) > 0
  interactionClaim = classifyInteractionClaim(explanationText, hasInteractionTerms)
  interactionInference = classifyInteractionInference(
    interactionClaim = interactionClaim,
    interactionMinPValue = interactionMinPValue,
    hasInteractionTerms = hasInteractionTerms,
    interactionAlpha = interactionAlpha
  )

  inferentialStyle = classifyInferentialStyle(
    usesInferentialLanguage = usesInferentialLanguage,
    usesDescriptiveOnlyLanguage = usesDescriptiveOnlyLanguage,
    overclaimDetected = overclaimDetected
  )

  list(
    runId = runId,
    timestamp = as.character(Sys.time()),
    exampleName = exampleName,
    package = package,
    modelType = modelType,
    formula = formula,
    equationsText = equationsText,
    explanationText = explanationText,
    normalizedExplanation = normalizedExplanation,
    wordCount = wordCount,
    sentenceCount = sentenceCount,
    mentionsConfidenceInterval = mentionsConfidenceInterval,
    usesPercentLanguage = usesPercentLanguage,
    mentionsReferenceGroup = mentionsReferenceGroup,
    mentionsInteraction = mentionsInteraction,
    uncertaintyMentioned = uncertaintyMentioned,
    usesInferentialLanguage = usesInferentialLanguage,
    usesDescriptiveOnlyLanguage = usesDescriptiveOnlyLanguage,
    overclaimDetected = overclaimDetected,
    effectDirection = effectDirection,
    effectScale = effectScale,
    interactionClaim = interactionClaim,
    interactionInference = interactionInference,
    interactionMinPValue = interactionMinPValue,
    interactionTerms = paste(interactionTerms, collapse = " | "),
    interactionAlpha = interactionAlpha,
    inferentialStyle = inferentialStyle,
    hasError = !is.na(errorMessage),
    errorMessage = errorMessage
  )
}
