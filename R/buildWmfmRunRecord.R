#' Build a single run record
#'
#' Creates a structured record for one repeated WMFM run, including raw text,
#' simple text summaries, and heuristic semantic-claim fields intended to help
#' assess whether repeated explanations differ in substantive meaning rather
#' than only wording.
#'
#' @param runId Integer.
#' @param exampleName Character.
#' @param package Character.
#' @param modelType Character.
#' @param formula Character.
#' @param equationsText Character.
#' @param explanationText Character.
#' @param errorMessage Character.
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
    errorMessage = NA_character_
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

    increaseDetected = detectPatternLocal(
      text,
      paste(
        "\\bincrease(s|d)?\\b",
        "\\bhigher\\b",
        "\\brise(s|n)?\\b",
        "\\bgrow(th|s|n)?\\b",
        "\\bmore\\b",
        sep = "|"
      )
    )

    decreaseDetected = detectPatternLocal(
      text,
      paste(
        "\\bdecrease(s|d)?\\b",
        "\\blower\\b",
        "\\bfall(s|en)?\\b",
        "\\bdrop(s|ped)?\\b",
        "\\breduce(s|d)?\\b",
        "\\bless\\b",
        "\\bdecline(s|d)?\\b",
        sep = "|"
      )
    )

    if (increaseDetected && decreaseDetected) {
      return("mixed_or_both")
    }

    if (increaseDetected) {
      return("increase")
    }

    if (decreaseDetected) {
      return("decrease")
    }

    "not_stated"
  }

  classifyEffectScale = function(text) {
    if (is.na(text) || !nzchar(trimws(text))) {
      return("not_stated")
    }

    multiplicativeDetected = detectPatternLocal(
      text,
      paste(
        "\\bpercent\\b",
        "%",
        "\\btimes\\b",
        "\\bfold\\b",
        "\\bmultipl",
        "\\bmultiplier\\b",
        "\\brate ratio\\b",
        "\\bof its previous\\b",
        "\\bexpected count\\b",
        "\\bfalls to about\\b",
        sep = "|"
      )
    )

    probabilityDetected = detectPatternLocal(
      text,
      paste(
        "\\bodds\\b",
        "\\bodds ratio\\b",
        "\\bprobability\\b",
        "\\bchance\\b",
        "\\blikelihood\\b",
        sep = "|"
      )
    )

    additiveDetected = detectPatternLocal(
      text,
      paste(
        "\\bpoint(s)?\\b",
        "\\bunit(s)?\\b",
        "\\bhigher by\\b",
        "\\blower by\\b",
        "\\bincrease of\\b",
        "\\bdecrease of\\b",
        sep = "|"
      )
    )

    nDetected = sum(c(multiplicativeDetected, probabilityDetected, additiveDetected))

    if (nDetected > 1) {
      return("mixed_or_unclear")
    }

    if (probabilityDetected) {
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

  classifyInteractionDirection = function(text, mentionsInteraction) {
    if (!isTRUE(mentionsInteraction)) {
      return("not_stated")
    }

    strongerDetected = detectPatternLocal(
      text,
      paste(
        "\\bsteeper\\b",
        "\\bstronger\\b",
        "\\blarger\\b",
        "\\bmore pronounced\\b",
        "\\bgreater\\b",
        sep = "|"
      )
    )

    weakerDetected = detectPatternLocal(
      text,
      paste(
        "\\bweaker\\b",
        "\\bsmaller\\b",
        "\\bless steep\\b",
        "\\bless pronounced\\b",
        sep = "|"
      )
    )

    if (strongerDetected && weakerDetected) {
      return("mixed_or_unclear")
    }

    if (strongerDetected) {
      return("stronger_effect")
    }

    if (weakerDetected) {
      return("weaker_effect")
    }

    "effect_differs_by_group"
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
    "interaction|effect differs by|depends on|varies by|different slope|steeper|shallower"
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
  interactionDirection = classifyInteractionDirection(
    explanationText,
    mentionsInteraction
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
    interactionDirection = interactionDirection,
    inferentialStyle = inferentialStyle,
    hasError = !is.na(errorMessage),
    errorMessage = errorMessage
  )
}
