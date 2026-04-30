#' Build a single WMFM repeated-run record
#'
#' Creates a structured record for one repeated WMFM run using a schema that
#' separates:
#' \enumerate{
#'   \item metadata and model context,
#'   \item raw explanation text and text summaries, and
#'   \item extracted semantic claims.
#' }
#'
#' This function is intentionally limited to describing what happened during a
#' run and what the explanation appeared to say. It does \emph{not} create any
#' judged scoring fields or aggregate score placeholders. Scoring is handled
#' later by downstream functions such as `score()` and
#' `scoreWmfmRepeatedRuns()`, which should operate on the raw run record rather
#' than being partially embedded in it.
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
#'   \item{interactionAlpha}{Stored interaction threshold metadata from the run.}
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
#' @param interactionAlpha Numeric interaction threshold metadata stored with
#'   the run record.
#'
#' @return Named list containing one structured raw run record.
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
      return(NA_character_)
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
  normalizedExplanation = normaliseWmfmText(explanationText)
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

  rangeMention = detectRangeExpression(explanationText)

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
  ) || rangeMention

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
    uncertaintyTypeClaim = uncertaintyTypeClaim
  )
}

#' Extract the expected scoring effect scale from an explanation audit
#'
#' Converts the human-readable effect-scale description stored in a
#' `wmfmExplanationAudit` object into the compact claim categories used by the
#' deterministic scoring core.
#'
#' @param audit A `wmfmExplanationAudit` object, or `NULL`.
#'
#' @return A character scalar: `"additive"`, `"multiplicative"`,
#'   `"probability_or_odds"`, or `NA_character_` when the audit does not supply
#'   a recognised scale.
#'
#' @keywords internal
#' @noRd
extractWmfmAuditExpectedEffectScale = function(audit) {
  if (!inherits(audit, "wmfmExplanationAudit")) {
    return(NA_character_)
  }

  effectScale = audit$interpretationScale$effectScale %||% NA_character_

  if (!is.character(effectScale) || length(effectScale) != 1 || is.na(effectScale)) {
    return(NA_character_)
  }

  effectScaleLower = tolower(effectScale)

  if (grepl("odds", effectScaleLower, fixed = TRUE)) {
    return("probability_or_odds")
  }

  if (grepl("multiplier", effectScaleLower, fixed = TRUE) ||
      grepl("multiplicative", effectScaleLower, fixed = TRUE) ||
      grepl("expected-count", effectScaleLower, fixed = TRUE) ||
      grepl("rate ratio", effectScaleLower, fixed = TRUE)) {
    return("multiplicative")
  }

  if (grepl("additive", effectScaleLower, fixed = TRUE) ||
      grepl("difference", effectScaleLower, fixed = TRUE)) {
    return("additive")
  }

  NA_character_
}


#' Resolve model-family metadata for scoring run records
#'
#' @param x A `wmfmModel` object.
#'
#' @return A named list with `modelFamily` and `linkFunction`.
#'
#' @keywords internal
#' @noRd
resolveWmfmModelFamilyMetadata = function(x) {
  modelFamily = NA_character_
  linkFunction = NA_character_

  if (inherits(x$model, "glm")) {
    modelFamily = x$model$family$family %||% NA_character_
    linkFunction = x$model$family$link %||% NA_character_
  } else if (inherits(x$model, "lm")) {
    modelFamily = "gaussian"
    linkFunction = "identity"
  }

  audit = x$explanationAudit

  if (inherits(audit, "wmfmExplanationAudit")) {
    modelFamily = audit$overview$modelFamily %||% modelFamily
    linkFunction = audit$overview$link %||% linkFunction
  }

  list(
    modelFamily = modelFamily,
    linkFunction = linkFunction
  )
}


#' Build a single run record for WMFM grading
#'
#' Internal helper that converts a `wmfmModel` object and a supplied
#' explanation into the same raw run-record structure used elsewhere in WMFM.
#'
#' @param x A `wmfmModel` object.
#' @param explanation Character scalar.
#' @param runId Integer run identifier.
#' @param answerRole Character scalar describing the answer role.
#'
#' @return A named list containing one raw run record.
#'
#' @keywords internal
#' @noRd
buildWmfmGradeRunRecord = function(
    x,
    explanation,
    runId = 1L,
    answerRole = c("student", "modelAnswer")
) {
  answerRole = match.arg(answerRole)

  if (!inherits(x, "wmfmModel")) {
    stop("`x` must inherit from `wmfmModel`.", call. = FALSE)
  }

  if (!is.character(explanation) || length(explanation) != 1 || is.na(explanation)) {
    stop("`explanation` must be a single non-missing character string.", call. = FALSE)
  }

  exampleName = x$meta$exampleName %||% NA_character_
  packageName = x$meta$package %||% NA_character_
  audit = x$explanationAudit
  modelMetadata = resolveWmfmModelFamilyMetadata(x)

  out = buildWmfmRunRecord(
    runId = as.integer(runId),
    exampleName = exampleName,
    package = packageName,
    modelType = x$modelType,
    formula = paste(deparse(x$formula), collapse = " "),
    equationsText = extractWmfmText(x$equations),
    explanationText = explanation,
    errorMessage = NA_character_,
    interactionTerms = x$interactionTerms %||% character(0),
    interactionMinPValue = x$interactionMinPValue %||% NA_real_,
    interactionAlpha = x$meta$interactionAlpha %||% 0.05
  )

  claimEvidenceMap = x$explanationClaimEvidenceMap
  hasClaimEvidenceMap = inherits(
    claimEvidenceMap,
    "wmfmExplanationClaimEvidenceMap"
  )

  claimEvidenceQualityFlags = NA_character_
  claimEvidenceSentenceCount = NA_integer_

  if (isTRUE(hasClaimEvidenceMap)) {
    if (is.data.frame(claimEvidenceMap$claims)) {
      claimEvidenceSentenceCount = nrow(claimEvidenceMap$claims)
    }

    qualityFlags = attr(claimEvidenceMap, "qualityFlags", exact = TRUE)

    if (length(qualityFlags) > 0) {
      qualityText = names(qualityFlags)[unlist(qualityFlags, use.names = FALSE)]

      if (length(qualityText) == 0) {
        qualityText = as.character(qualityFlags)
      }

      qualityText = qualityText[!is.na(qualityText) & nzchar(qualityText)]

      if (length(qualityText) > 0) {
        claimEvidenceQualityFlags = paste(unique(qualityText), collapse = "|")
      }
    }
  }

  out$answerRole = answerRole
  out$hasExplanationAudit = inherits(audit, "wmfmExplanationAudit")
  out$hasExplanationClaimEvidenceMap = hasClaimEvidenceMap
  out$claimEvidenceSentenceCount = claimEvidenceSentenceCount
  out$claimEvidenceQualityFlags = claimEvidenceQualityFlags
  out$modelFamily = modelMetadata$modelFamily
  out$linkFunction = modelMetadata$linkFunction
  out$auditEffectScale = if (inherits(audit, "wmfmExplanationAudit")) {
    audit$interpretationScale$effectScale %||% NA_character_
  } else {
    NA_character_
  }
  out$expectedEffectScale = extractWmfmAuditExpectedEffectScale(audit)
  out$scoringContext = if (isTRUE(out$hasExplanationAudit) && isTRUE(hasClaimEvidenceMap)) {
    "final_text_plus_explanation_audit_and_claim_evidence"
  } else if (isTRUE(out$hasExplanationAudit)) {
    "final_text_plus_explanation_audit"
  } else {
    "final_text_only"
  }

  out
}


#' Rebuild raw WMFM run records without rerunning the LLM
#'
#' Recomputes raw extracted fields for an existing `wmfmRuns` object by
#' re-running `buildWmfmRunRecord()` on the stored run metadata and generated
#' text. This is useful when extraction rules change and you want to refresh the
#' raw run records without generating new LLM outputs.
#'
#' This function is intentionally limited to rebuilding raw run records. It does
#' not rescore runs and does not compute summaries. If scoring is needed after
#' rebuilding, call `score()` on the returned object.
#'
#' @param x A `wmfmRuns` object.
#' @param preserveClass Logical. Should the class of `x` be preserved on the
#'   returned object? Defaults to `TRUE`.
#'
#' @return A rebuilt `wmfmRuns` object with refreshed `runs` records.
#' @export
rebuildWmfmRunRecords = function(
    x,
    preserveClass = TRUE
) {
  splitInteractionTerms = function(x) {
    if (length(x) == 0 || is.na(x) || !nzchar(trimws(x))) {
      return(character(0))
    }

    parts = unlist(strsplit(as.character(x), "\\|", fixed = FALSE))
    trimws(parts[nzchar(trimws(parts))])
  }

  getScalarField = function(runRecord, fieldName, default = NA) {
    if (!(fieldName %in% names(runRecord))) {
      return(default)
    }

    value = runRecord[[fieldName]]

    if (length(value) == 0) {
      return(default)
    }

    value[[1]]
  }

  rebuildOne = function(runRecord) {
    buildWmfmRunRecord(
      runId = getScalarField(runRecord, "runId"),
      exampleName = getScalarField(runRecord, "exampleName"),
      package = getScalarField(runRecord, "package"),
      modelType = getScalarField(runRecord, "modelType"),
      formula = getScalarField(runRecord, "formula"),
      equationsText = getScalarField(runRecord, "equationsText"),
      explanationText = getScalarField(runRecord, "explanationText"),
      errorMessage = getScalarField(runRecord, "errorMessage", NA_character_),
      interactionTerms = splitInteractionTerms(
        getScalarField(runRecord, "interactionTerms", NA_character_)
      ),
      interactionMinPValue = getScalarField(
        runRecord,
        "interactionMinPValue",
        NA_real_
      ),
      interactionAlpha = getScalarField(
        runRecord,
        "interactionAlpha",
        0.05
      )
    )
  }

  if (!inherits(x, "wmfmRuns")) {
    stop("`x` must inherit from `wmfmRuns`.", call. = FALSE)
  }

  if (!is.list(x$runs) || length(x$runs) == 0) {
    stop("`x$runs` must be a non-empty list of run records.", call. = FALSE)
  }

  rebuiltRuns = lapply(
    x$runs,
    function(runRecord) {
      if (!is.list(runRecord) || is.null(names(runRecord))) {
        stop(
          "Each element of `x$runs` must be a named run-record list.",
          call. = FALSE
        )
      }

      rebuildOne(runRecord)
    }
  )

  out = x
  out$runs = rebuiltRuns

  if ("summary" %in% names(out)) {
    out$summary = NULL
  }

  if (isTRUE(preserveClass)) {
    class(out) = class(x)
  }

  out
}
