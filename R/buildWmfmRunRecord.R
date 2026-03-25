#' Build a single run record
#'
#' Creates a structured record for one repeated WMFM run, including raw text,
#' simple text summaries, heuristic semantic-claim fields, and a model-aware
#' interaction-inference assessment.
#'
#' The interaction-related fields are intended to distinguish between:
#' \itemize{
#'   \item what the explanation says about the interaction
#'   \item whether that claim is appropriate given the interaction-term
#'   p-value(s)
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
#' @param interactionTerms Character vector of fitted interaction-term names.
#' @param interactionPValues Numeric vector of p-values for fitted interaction
#'   terms.
#' @param interactionAlpha Numeric. Significance threshold used when assessing
#'   whether the interaction claim is too strong or too weak. Defaults to 0.05.
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
    interactionTerms = character(),
    interactionPValues = numeric(),
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
      "\\bdecreased\\b",
      "\\bdecline(s|d)?\\b",
      "\\bdeclined\\b",
      "\\bfall(s|en)?\\b",
      "\\bfell\\b",
      "\\bdrop(s|ped)?\\b",
      "\\bdropped\\b",
      "\\breduce(s|d)?\\b",
      "\\breduced\\b",
      "\\blower\\b",
      "\\brarer\\b",
      "\\bsmaller\\b",
      "\\bsharply lower\\b",
      "\\bmarkedly lower\\b",
      sep = "|"
    )

    increasePattern = paste(
      "\\bincrease(s|d)?\\b",
      "\\bincreased\\b",
      "\\brise(s|n)?\\b",
      "\\brose\\b",
      "\\bgrow(s|n)?\\b",
      "\\bgrew\\b",
      "\\bhigher\\b",
      "\\blarger\\b",
      "\\bgreater\\b",
      "\\bmore frequent\\b",
      "\\bmore abundant\\b",
      sep = "|"
    )

    decreaseDetected = detectPatternLocal(text, decreasePattern)
    increaseDetected = detectPatternLocal(text, increasePattern)

    if (decreaseDetected && increaseDetected) {
      if (detectPatternLocal(text, "\\bfalls? to\\b|\\bdrops? to\\b|\\breduces? to\\b|\\bdeclines? to\\b")) {
        return("decrease")
      }

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
      "\\bpercent\\b",
      "%",
      "\\btimes\\b",
      "\\bfold\\b",
      "\\bmultipl(y|ies|ied)?\\b",
      "\\bmultiplier\\b",
      "\\brate ratio\\b",
      "\\brelative risk\\b",
      "\\bexpected count\\b",
      "\\bfalls? to\\b",
      "\\bdrops? to\\b",
      "\\breduces? to\\b",
      "\\bof (its|their|the) previous\\b",
      sep = "|"
    )

    probabilityOrOddsPattern = paste(
      "\\bodds\\b",
      "\\bodds ratio\\b",
      "\\bprobability\\b",
      "\\bprobabilities\\b",
      "\\bchance\\b",
      "\\blikelihood\\b",
      sep = "|"
    )

    additivePattern = paste(
      "\\bby [0-9]+(\\.[0-9]+)? units?\\b",
      "\\bby [0-9]+(\\.[0-9]+)? points?\\b",
      "\\bincrease of [0-9]+(\\.[0-9]+)?\\b",
      "\\bdecrease of [0-9]+(\\.[0-9]+)?\\b",
      "\\bhigher by [0-9]+(\\.[0-9]+)?\\b",
      "\\blower by [0-9]+(\\.[0-9]+)?\\b",
      "\\badditive\\b",
      sep = "|"
    )

    multiplicativeDetected = detectPatternLocal(text, multiplicativePattern)
    probabilityOrOddsDetected = detectPatternLocal(text, probabilityOrOddsPattern)
    additiveDetected = detectPatternLocal(text, additivePattern)

    nDetected = sum(c(multiplicativeDetected, probabilityOrOddsDetected, additiveDetected))

    if (nDetected > 1) {
      if (multiplicativeDetected && probabilityOrOddsDetected && !additiveDetected) {
        return("probability_or_odds")
      }

      return("mixed_or_unclear")
    }

    if (probabilityOrOddsDetected) {
      return("probability_or_odds")
    }

    if (multiplicativeDetected) {
      return("multiplicative")
    }

    if (additiveDetected) {
      return("additive")
    }

    "not_stated"
  }

  classifyInteractionClaim = function(text, mentionsInteraction) {
    if (!isTRUE(mentionsInteraction)) {
      return("not_mentioned")
    }

    if (is.na(text) || !nzchar(trimws(text))) {
      return("not_mentioned")
    }

    noDifferencePattern = paste(
      "\\blittle evidence\\b",
      "\\bno evidence\\b",
      "\\bnot clear\\b",
      "\\bdoes not clearly\\b",
      "\\bdo not clearly\\b",
      "\\bno clear interaction\\b",
      "\\bweak evidence\\b",
      "\\buncertain interaction\\b",
      "\\binteraction .* not .* clear\\b",
      sep = "|"
    )

    strongDifferencePattern = paste(
      "\\binteraction\\b",
      "\\beffect differs by\\b",
      "\\bdiffers by group\\b",
      "\\bdepends on\\b",
      "\\bvaries by\\b",
      "\\bdifferent slope\\b",
      "\\bdifferent slopes\\b",
      "\\bsteeper\\b",
      "\\bshallower\\b",
      "\\bstronger\\b",
      "\\bweaker\\b",
      "\\bmore pronounced\\b",
      "\\bless pronounced\\b",
      "\\bespecially pronounced\\b",
      sep = "|"
    )

    noDifferenceDetected = detectPatternLocal(text, noDifferencePattern)
    strongDifferenceDetected = detectPatternLocal(text, strongDifferencePattern)

    if (noDifferenceDetected && strongDifferenceDetected) {
      return("mixed_or_unclear")
    }

    if (strongDifferenceDetected) {
      return("difference_claimed")
    }

    if (noDifferenceDetected) {
      return("no_clear_difference")
    }

    "mentioned_but_unclear"
  }

  classifyInteractionInference = function(
      interactionClaim,
      interactionPValues,
      interactionAlpha
  ) {
    interactionPValues = as.numeric(interactionPValues)
    interactionPValues = interactionPValues[!is.na(interactionPValues)]

    if (length(interactionPValues) == 0) {
      return("not_applicable")
    }

    if (interactionClaim %in% c("mixed_or_unclear", "mentioned_but_unclear")) {
      return("unclear")
    }

    strongestEvidence = min(interactionPValues)

    if (interactionClaim == "difference_claimed") {
      if (strongestEvidence < interactionAlpha) {
        return("appropriate")
      }

      return("too_strong")
    }

    if (interactionClaim %in% c("no_clear_difference", "not_mentioned")) {
      if (strongestEvidence < interactionAlpha) {
        return("too_weak")
      }

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
  interactionTerms = interactionTerms[!is.na(interactionTerms) & nzchar(trimws(interactionTerms))]
  interactionPValues = as.numeric(interactionPValues)
  interactionPValues = interactionPValues[!is.na(interactionPValues)]

  if (length(explanationText) == 0 || is.na(explanationText)) {
    explanationText = NA_character_
  }

  if (length(equationsText) == 0 || is.na(equationsText)) {
    equationsText = NA_character_
  }

  if (length(errorMessage) == 0 || identical(errorMessage, "NA")) {
    errorMessage = NA_character_
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
    "reference group|reference category|reference level|baseline|compared with the reference|relative to the reference"
  )

  mentionsInteraction = detectPatternLocal(
    explanationText,
    "interaction|effect differs by|depends on|varies by|different slope|different slopes|steeper|shallower|more pronounced|less pronounced"
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
      "\\bproves that\\b",
      "\\bdefinitely\\b",
      "\\bclearly proves\\b",
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
        "\\bhas\\b",
        "\\bhave\\b",
        sep = "|"
      )
    )

  effectDirection = classifyEffectDirection(explanationText)
  effectScale = classifyEffectScale(explanationText)
  interactionClaim = classifyInteractionClaim(explanationText, mentionsInteraction)
  interactionInference = classifyInteractionInference(
    interactionClaim = interactionClaim,
    interactionPValues = interactionPValues,
    interactionAlpha = interactionAlpha
  )

  inferentialStyle = classifyInferentialStyle(
    usesInferentialLanguage = usesInferentialLanguage,
    usesDescriptiveOnlyLanguage = usesDescriptiveOnlyLanguage,
    overclaimDetected = overclaimDetected
  )

  interactionMinPValue = if (length(interactionPValues) > 0) {
    min(interactionPValues)
  } else {
    NA_real_
  }

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
    interactionTerms = if (length(interactionTerms) > 0) paste(interactionTerms, collapse = " | ") else NA_character_,
    interactionMinPValue = interactionMinPValue,
    interactionClaim = interactionClaim,
    interactionInference = interactionInference,
    inferentialStyle = inferentialStyle,
    hasError = !is.na(errorMessage),
    errorMessage = errorMessage
  )
}
