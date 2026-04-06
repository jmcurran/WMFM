#' Build a single WMFM repeated-run record
#'
#' Creates a structured record for one repeated WMFM run using a schema that
#' separates:
#' \enumerate{
#'   \item metadata and model context,
#'   \item raw explanation text and text summaries,
#'   \item extracted semantic claims, and
#'   \item judged quality fields and aggregate score placeholders.
#' }
#'
#' This design is intended to keep a clear distinction between:
#' \itemize{
#'   \item what the explanation said, and
#'   \item whether those claims were appropriate or correct.
#' }
#'
#' In this file, most claim fields are extracted directly from the explanation
#' text using simple heuristics. Only one judged field is computed here:
#' `interactionEvidenceAppropriate`, because that judgment can be made from the
#' extracted interaction claim together with the fitted model's interaction-term
#' evidence (`interactionMinPValue`, `interactionAlpha`, and whether interaction
#' terms are present).
#'
#' The remaining judged fields and aggregate scores are initialised as `NA` and
#' are intended to be populated later by downstream scoring functions such as
#' `scoreWmfmRepeatedRuns()`.
#'
#' ## Output schema
#'
#' The returned named list contains the following groups of fields.
#'
#' ### Metadata and model context
#' \describe{
#'   \item{runId}{Integer run identifier.}
#'   \item{timestamp}{Character timestamp created at record build time.}
#'   \item{exampleName}{Example identifier.}
#'   \item{package}{Package name.}
#'   \item{modelType}{High-level model class.}
#'   \item{modelFamily}{Optional model family. Initialised to `NA`.}
#'   \item{linkFunction}{Optional link function. Initialised to `NA`.}
#'   \item{formula}{Model formula as character.}
#'   \item{equationsText}{Fitted-equation text.}
#'   \item{interactionTerms}{Pipe-separated interaction-term names.}
#'   \item{hasInteractionTerms}{Logical. Whether the fitted model includes interactions.}
#'   \item{nInteractionTerms}{Integer count of interaction terms.}
#'   \item{interactionMinPValue}{Minimum interaction-term p-value, or `NA`.}
#'   \item{interactionAlpha}{Threshold used when judging interaction evidence.}
#'   \item{hasError}{Logical. Whether an error was recorded for the run.}
#'   \item{errorMessage}{Run error message, or `NA`.}
#' }
#'
#' ### Raw explanation text and summaries
#' \describe{
#'   \item{explanationText}{Raw explanation text.}
#'   \item{normalizedExplanation}{Normalised explanation used for duplicate detection.}
#'   \item{explanationPresent}{Logical. Whether a non-empty explanation is present.}
#'   \item{wordCount}{Approximate word count.}
#'   \item{sentenceCount}{Approximate sentence count.}
#' }
#'
#' ### Extracted claim variables
#' \describe{
#'   \item{ciMention}{Logical. Whether a confidence or credible interval is mentioned.}
#'   \item{percentLanguageMention}{Logical. Whether percent language is used.}
#'   \item{referenceGroupMention}{Logical. Whether reference/baseline framing is mentioned.}
#'   \item{interactionMention}{Logical. Whether interaction or differential-pattern language is mentioned.}
#'   \item{uncertaintyMention}{Logical. Whether uncertainty or evidential caution is mentioned.}
#'   \item{usesInferentialLanguage}{Logical. Whether inferential wording is present.}
#'   \item{usesDescriptiveOnlyLanguage}{Logical. Whether wording is descriptive only.}
#'   \item{overclaimDetected}{Logical. Whether overstrong language is detected.}
#'   \item{underclaimDetected}{Logical. Whether unusually weak language is detected.}
#'   \item{conditionalLanguageMention}{Logical. Whether conditional wording such as "depends on" or subgroup-conditioned interpretation is present.}
#'   \item{comparisonLanguageMention}{Logical. Whether explicit or implicit comparison wording is present.}
#'   \item{outcomeMention}{Logical. Placeholder for whether the response is clearly named. Initialised conservatively.}
#'   \item{predictorMention}{Logical. Placeholder for whether the focal predictor is clearly named. Initialised conservatively.}
#'   \item{effectDirectionClaim}{Character. One of `increase`, `decrease`, `mixed_or_both`, `not_stated`.}
#'   \item{effectScaleClaim}{Character. One of `additive`, `multiplicative`, `probability_or_odds`, `mixed_or_unclear`, `not_stated`.}
#'   \item{interactionSubstantiveClaim}{Character. One of `difference_claimed_strongly`, `difference_claimed_cautiously`, `no_clear_difference`, `not_mentioned`, `unclear`, `not_applicable`.}
#'   \item{inferentialRegister}{Character. One of `inferential`, `descriptive_only`, `overclaiming`, `unclear`.}
#'   \item{uncertaintyTypeClaim}{Character. One of `none`, `generic_uncertainty`, `confidence_interval`, `mixed`, `unclear`.}
#' }
#'
#' ### Judged quality fields created or reserved here
#' \describe{
#'   \item{interactionEvidenceAppropriate}{Character. One of `appropriate`, `too_strong`, `too_weak`, `unclear`, `not_applicable`.}
#'   \item{effectDirectionCorrect}{Integer score placeholder (`0`, `1`, `2`, or `NA`).}
#'   \item{effectScaleAppropriate}{Integer score placeholder (`0`, `1`, `2`, or `NA`).}
#'   \item{referenceGroupHandledCorrectly}{Integer score placeholder (`0`, `1`, `2`, or `NA`).}
#'   \item{interactionCoverageAdequate}{Integer score placeholder (`0`, `1`, `2`, or `NA`).}
#'   \item{interactionSubstantiveCorrect}{Integer score placeholder (`0`, `1`, `2`, or `NA`).}
#'   \item{uncertaintyHandlingAppropriate}{Integer score placeholder (`0`, `1`, `2`, or `NA`).}
#'   \item{inferentialRegisterAppropriate}{Integer score placeholder (`0`, `1`, `2`, or `NA`).}
#'   \item{mainEffectCoverageAdequate}{Integer score placeholder (`0`, `1`, `2`, or `NA`).}
#'   \item{referenceGroupCoverageAdequate}{Integer score placeholder (`0`, `1`, `2`, or `NA`).}
#'   \item{clarityAdequate}{Integer score placeholder (`0`, `1`, `2`, or `NA`).}
#'   \item{numericExpressionAdequate}{Integer score placeholder (`0`, `1`, `2`, or `NA`).}
#'   \item{comparisonStructureClear}{Integer score placeholder (`0`, `1`, `2`, or `NA`).}
#'   \item{fatalFlawDetected}{Logical placeholder.}
#' }
#'
#' ### Aggregate score placeholders
#' \describe{
#'   \item{factualScore}{Numeric placeholder.}
#'   \item{inferenceScore}{Numeric placeholder.}
#'   \item{completenessScore}{Numeric placeholder.}
#'   \item{clarityScore}{Numeric placeholder.}
#'   \item{calibrationScore}{Numeric placeholder.}
#'   \item{overallScore}{Numeric placeholder.}
#'   \item{overallPass}{Logical placeholder.}
#' }
#'
#' @param runId Integer run identifier.
#' @param exampleName Character example identifier.
#' @param package Character package name.
#' @param modelType Character model class.
#' @param formula Character model formula.
#' @param equationsText Character fitted-equation text.
#' @param explanationText Character explanation text.
#' @param errorMessage Character error message, or `NA`.
#' @param interactionTerms Character vector of interaction-term names from the
#'   fitted model.
#' @param interactionMinPValue Numeric minimum p-value across fitted interaction
#'   terms, or `NA` if no interaction terms are present or no p-value could be
#'   extracted.
#' @param interactionAlpha Numeric threshold used to judge whether the
#'   explanation's interaction-evidence wording is appropriate.
#'
#' @return Named list containing one structured run record.
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

  normaliseScalarText = function(x) {
    if (length(x) == 0) {
      return(NA_character_)
    }

    x = as.character(x)[1]

    if (is.na(x) || identical(x, "NA") || !nzchar(trimws(x))) {
      return(NA_character_)
    }

    x
  }

  classifyEffectDirection = function(text) {
    if (is.na(text) || !nzchar(trimws(text))) {
      return("not_stated")
    }

    decreasePattern = paste(
      "\\bdecrease(s|d)?\\b",
      "\\bdecline(s|d)?\\b",
      "\\bdeclines?\\b",
      "\\breduce(s|d|r)?\\b",
      "\\breduction\\b",
      "\\bdrop(s|ped|ping)?\\b",
      "\\bfall(s|en|ing)?\\b",
      "\\bsmaller\\b",
      "\\brare(r)?\\b",
      "\\bfalls? to\\b",
      "\\bcut(s|ting)?\\b",
      "\\bdiminish(es|ed|ing)?\\b",
      "\\bsteeper decline\\b",
      sep = "|"
    )

    increasePattern = paste(
      "\\bincrease(s|d|ing)?\\b",
      "\\brise(s|n)?\\b",
      "\\bgrow(s|n|th|ing)?\\b",
      "\\bhigher\\b",
      "\\blarger\\b",
      "\\bgreater\\b",
      "\\bmore likely\\b",
      "\\bmore frequent\\b",
      "\\bmore abundant\\b",
      "\\bsteeper rise\\b",
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

  classifyInteractionSubstantiveClaim = function(text, hasInteractionTerms) {
    if (!isTRUE(hasInteractionTerms)) {
      return("not_applicable")
    }

    if (is.na(text) || !nzchar(trimws(text))) {
      return("not_mentioned")
    }

    strongDifferencePattern = paste(
      "\\bsteeper\\b",
      "\\bshallower\\b",
      "\\bstronger\\b",
      "\\bweaker\\b",
      "\\bmore pronounced\\b",
      "\\bless pronounced\\b",
      "\\bespecially pronounced\\b",
      "\\bclearly different\\b",
      "\\bdrops? more rapidly\\b",
      "\\bfalls? more rapidly\\b",
      "\\bdrops? faster\\b",
      "\\bfalls? faster\\b",
      "\\bdiminish(es)? more steeply\\b",
      "\\bdiffers? markedly\\b",
      "\\bpattern differs?\\b",
      "\\brate differs?\\b",
      sep = "|"
    )

    cautiousDifferencePattern = paste(
      "\\bevidence.*differ",
      "\\bsuggest(s|ed)?.*differ",
      "\\bindicate(s|d)?.*differ",
      "\\beffect differs by\\b",
      "\\beffect varies by\\b",
      "\\bdepends on\\b",
      "\\bvaries by\\b",
      "\\binteraction\\b",
      "\\bdifferent slope\\b",
      "\\bcompared with\\b",
      "\\brelative to\\b",
      "\\bin contrast\\b",
      "\\bwhereas\\b",
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
      "\\bsimilar pattern\\b",
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

  classifyInteractionEvidenceAppropriate = function(
      interactionSubstantiveClaim,
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

    if (interactionSubstantiveClaim == "unclear") {
      return("unclear")
    }

    if (interactionMinPValue <= interactionAlpha) {
      if (interactionSubstantiveClaim %in% c(
        "difference_claimed_strongly",
        "difference_claimed_cautiously"
      )) {
        return("appropriate")
      }

      if (interactionSubstantiveClaim %in% c("no_clear_difference", "not_mentioned")) {
        return("too_weak")
      }

      return("unclear")
    }

    if (interactionSubstantiveClaim %in% c(
      "difference_claimed_strongly",
      "difference_claimed_cautiously"
    )) {
      return("too_strong")
    }

    if (interactionSubstantiveClaim %in% c("no_clear_difference", "not_mentioned")) {
      return("appropriate")
    }

    "unclear"
  }

  classifyInferentialRegister = function(
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

  classifyUncertaintyTypeClaim = function(text) {
    if (is.na(text) || !nzchar(trimws(text))) {
      return("none")
    }

    ciDetected = detectPatternLocal(
      text,
      "confidence interval|95%|95 %|credible interval|interval estimate"
    )

    genericDetected = detectPatternLocal(
      text,
      paste(
        "\\buncertain",
        "\\buncertainty",
        "\\bplausible",
        "\\bconsistent with the data",
        "\\bevidence",
        "\\bsuggest(s|ed)?\\b",
        "\\bindicate(s|d)?\\b",
        sep = "|"
      )
    )

    if (ciDetected && genericDetected) {
      return("mixed")
    }

    if (ciDetected) {
      return("confidence_interval")
    }

    if (genericDetected) {
      return("generic_uncertainty")
    }

    "none"
  }

  explanationText = normaliseScalarText(explanationText)
  equationsText = normaliseScalarText(equationsText)
  errorMessage = normaliseScalarText(errorMessage)
  interactionTerms = as.character(interactionTerms)

  if (length(interactionTerms) == 1 && is.na(interactionTerms)) {
    interactionTerms = character(0)
  }

  interactionTerms = interactionTerms[nzchar(trimws(interactionTerms))]
  hasInteractionTerms = length(interactionTerms) > 0
  nInteractionTerms = length(interactionTerms)

  explanationPresent = !is.na(explanationText) && nzchar(trimws(explanationText))
  normalizedExplanation = normalizeWmfmText(explanationText)
  wordCount = countWmfmWords(explanationText)
  sentenceCount = countWmfmSentences(explanationText)

  ciMention = detectPatternLocal(
    explanationText,
    "confidence interval|95%|95 %|credible interval|interval estimate"
  )

  percentLanguageMention = detectPatternLocal(
    explanationText,
    "percent|%"
  )

  referenceGroupMention = detectPatternLocal(
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

  interactionMention = detectPatternLocal(
    explanationText,
    paste(
      "interaction",
      "effect differs by",
      "effect varies by",
      "depends on",
      "varies by",
      "different slope",
      "steeper",
      "shallower",
      "pattern differs",
      "drops faster",
      "falls faster",
      "more gradual",
      "compared with",
      "whereas",
      sep = "|"
    )
  )

  uncertaintyMention = detectPatternLocal(
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
      "\\bindicate(s|d)?\\b",
      "\\bestimate(d)?\\b",
      sep = "|"
    )
  )

  usesInferentialLanguage = detectPatternLocal(
    explanationText,
    paste(
      "\\bevidence\\b",
      "\\bsuggest(ing|s|ed)?\\b",
      "\\bindicate(s|d|ing)?\\b",
      "\\b(the data (are )?)?consistent with\\b",
      "\\bestimate(d|s|ing)?\\b",
      "\\bconfidence interval\\b",
      "\\bassociated with\\b",
      "\\blinked to\\b",
      "\\brelated to\\b",
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
      "(?<!\\b(final\\s)?exam(ination)?\\s)(?<!\\btest\\s)(?<!\\bscore\\s)\\bresults in\\b",
      sep = "|"
    )
  )

  weakMatches = gregexpr(
    paste(
      "\\bmay\\b",
      "\\bmight\\b",
      "\\bcould\\b",
      "\\bpossibly\\b",
      "\\bperhaps\\b",
      "\\bappears? to\\b",
      sep = "|"
    ),
    explanationText,
    ignore.case = TRUE,
    perl = TRUE
  )[[1]]

  weakCount = if (length(weakMatches) == 0 || weakMatches[1] == -1) {
    0L
  } else {
    length(weakMatches)
  }

  underclaimDetected = weakCount >= 2L && !usesInferentialLanguage

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

  conditionalLanguageMention = detectPatternLocal(
    explanationText,
    paste(
      "depends on",
      "varies by",
      "for .* compared with",
      "in .* each",
      "when .* then",
      "whereas",
      sep = "|"
    )
  )

  explicitComparisonLanguageMention = detectPatternLocal(
    explanationText,
    paste(
      "compared with",
      "relative to",
      "higher than",
      "lower than",
      "more than",
      "less than",
      "whereas",
      "in contrast",
      sep = "|"
    )
  )

  implicitComparisonLanguageMention = detectImplicitComparison(explanationText)

  comparisonLanguageMention =
    explicitComparisonLanguageMention ||
    implicitComparisonLanguageMention

  outcomeMention = explanationPresent
  predictorMention = explanationPresent

  effectDirectionClaim = classifyEffectDirection(explanationText)
  effectScaleClaim = classifyEffectScaleClaim(explanationText)
  interactionSubstantiveClaim = classifyInteractionSubstantiveClaim(
    text = explanationText,
    hasInteractionTerms = hasInteractionTerms
  )

  inferentialRegister = classifyInferentialRegister(
    usesInferentialLanguage = usesInferentialLanguage,
    usesDescriptiveOnlyLanguage = usesDescriptiveOnlyLanguage,
    overclaimDetected = overclaimDetected
  )

  uncertaintyTypeClaim = classifyUncertaintyTypeClaim(explanationText)

  interactionEvidenceAppropriate = classifyInteractionEvidenceAppropriate(
    interactionSubstantiveClaim = interactionSubstantiveClaim,
    interactionMinPValue = interactionMinPValue,
    hasInteractionTerms = hasInteractionTerms,
    interactionAlpha = interactionAlpha
  )

  list(
    runId = runId,
    timestamp = as.character(Sys.time()),
    exampleName = exampleName,
    package = package,
    modelType = modelType,
    modelFamily = NA_character_,
    linkFunction = NA_character_,
    formula = formula,
    equationsText = equationsText,
    interactionTerms = paste(interactionTerms, collapse = " | "),
    hasInteractionTerms = hasInteractionTerms,
    nInteractionTerms = nInteractionTerms,
    interactionMinPValue = interactionMinPValue,
    interactionAlpha = interactionAlpha,
    hasError = !is.na(errorMessage),
    errorMessage = errorMessage,
    explanationText = explanationText,
    normalizedExplanation = normalizedExplanation,
    explanationPresent = explanationPresent,
    wordCount = wordCount,
    sentenceCount = sentenceCount,
    ciMention = ciMention,
    percentLanguageMention = percentLanguageMention,
    referenceGroupMention = referenceGroupMention,
    interactionMention = interactionMention,
    uncertaintyMention = uncertaintyMention,
    usesInferentialLanguage = usesInferentialLanguage,
    usesDescriptiveOnlyLanguage = usesDescriptiveOnlyLanguage,
    overclaimDetected = overclaimDetected,
    underclaimDetected = underclaimDetected,
    conditionalLanguageMention = conditionalLanguageMention,
    comparisonLanguageMention = comparisonLanguageMention,
    outcomeMention = outcomeMention,
    predictorMention = predictorMention,
    effectDirectionClaim = effectDirectionClaim,
    effectScaleClaim = effectScaleClaim,
    interactionSubstantiveClaim = interactionSubstantiveClaim,
    inferentialRegister = inferentialRegister,
    uncertaintyTypeClaim = uncertaintyTypeClaim,
    interactionEvidenceAppropriate = interactionEvidenceAppropriate,
    effectDirectionCorrect = NA_integer_,
    effectScaleAppropriate = NA_integer_,
    referenceGroupHandledCorrectly = NA_integer_,
    interactionCoverageAdequate = NA_integer_,
    interactionSubstantiveCorrect = NA_integer_,
    uncertaintyHandlingAppropriate = NA_integer_,
    inferentialRegisterAppropriate = NA_integer_,
    mainEffectCoverageAdequate = NA_integer_,
    referenceGroupCoverageAdequate = NA_integer_,
    clarityAdequate = NA_integer_,
    numericExpressionAdequate = NA_integer_,
    comparisonStructureClear = NA_integer_,
    fatalFlawDetected = NA,
    factualScore = NA_real_,
    inferenceScore = NA_real_,
    completenessScore = NA_real_,
    clarityScore = NA_real_,
    calibrationScore = NA_real_,
    overallScore = NA_real_,
    overallPass = NA
  )
}
