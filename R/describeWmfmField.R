#' Describe a WMFM repeated-run evaluation field
#'
#' Prints a human-readable description of a field used in the WMFM repeated-run
#' explanation evaluation workflow. This is intended to help interpret the
#' claim, judged-quality, and score columns that appear in objects created or
#' updated by `buildWmfmRunRecord()`, `scoreWmfmRepeatedRuns()`,
#' `summary()` for `wmfmRuns` objects, and `plotWmfmExplanationClaimHeatmap()`.
#'
#' The function accepts:
#' \itemize{
#'   \item current schema names such as `interactionEvidenceAppropriate`,
#'   \item older aliases such as `interactionInference`, and
#'   \item the pretty x-axis labels used by `plotWmfmExplanationClaimHeatmap()`,
#'   such as `"Direction claim"`, `"Scale claim"`, or `"CI mention"`.
#' }
#'
#' By default the function prints a formatted explanation to the console using
#' `cat()` and returns the underlying character vector invisibly. This makes it
#' convenient for interactive use while still allowing the text to be captured
#' programmatically when needed.
#'
#' For judged fields scored on a `0`/`1`/`2` scale, the usual interpretation is:
#' \describe{
#'   \item{0}{Poor, missing, inappropriate, or clearly problematic.}
#'   \item{1}{Partly adequate, weak, borderline, or unclear.}
#'   \item{2}{Appropriate, clear, or well handled.}
#' }
#'
#' @param field Character scalar naming the field to describe. This may be a
#'   canonical field name, a recognised alias, or one of the pretty labels shown
#'   on the heatmap x-axis.
#' @param format Character. One of `"text"`, `"list"`, or `"data.frame"`.
#'   The default `"text"` prints a formatted character vector with `cat()` and
#'   returns that vector invisibly. `"list"` returns the underlying structured
#'   description. `"data.frame"` returns a one-row data frame.
#' @param includeExamples Logical. Should examples be included in the returned
#'   output?
#' @param includeAliases Logical. Should recognised aliases be included in the
#'   returned output?
#'
#' @return If `format = "text"`, a formatted character vector returned
#'   invisibly after being printed to the console. Otherwise a named list or a
#'   one-row data frame describing the requested field.
#' @export
#'
#' @examples
#' describeWmfmField("interactionEvidenceAppropriate")
#' describeWmfmField("Direction claim")
#' describeWmfmField("CI mention")
#' describeWmfmField("overallScore", format = "list")
describeWmfmField = function(
    field,
    format = c("text", "list", "data.frame"),
    includeExamples = TRUE,
    includeAliases = TRUE
) {

  format = match.arg(format)

  if (length(field) != 1 || is.na(field) || !nzchar(trimws(field))) {
    stop("`field` must be a single non-empty character string.", call. = FALSE)
  }

  field = trimws(as.character(field))

  normaliseLookupKey = function(x) {
    x = tolower(trimws(as.character(x)))
    x = gsub("[_[:space:]-]+", " ", x)
    x = gsub("[^[:alnum:]% ]+", "", x)
    x = gsub("[[:space:]]+", " ", x)
    trimws(x)
  }

  makeEntry = function(
      canonicalName,
      group,
      type,
      title,
      description,
      values = NA_character_,
      scoring = NA_character_,
      examplesHigh = NA_character_,
      examplesLow = NA_character_,
      aliases = character(0),
      prettyLabels = character(0)
  ) {
    list(
      canonicalName = canonicalName,
      group = group,
      type = type,
      title = title,
      description = description,
      values = values,
      scoring = scoring,
      examplesHigh = examplesHigh,
      examplesLow = examplesLow,
      aliases = aliases,
      prettyLabels = prettyLabels
    )
  }

  fieldDb = list(
    effectDirectionClaim = makeEntry(
      canonicalName = "effectDirectionClaim",
      group = "claim",
      type = "categorical",
      title = "Effect direction claim",
      description = paste(
        "What direction of effect the explanation appears to claim for the main",
        "relationship. It is extracted from the wording of the explanation, not",
        "judged against the model truth in this field itself."
      ),
      values = "increase, decrease, mixed_or_both, not_stated",
      scoring = paste(
        "This is a claim field rather than a score. It records what the model",
        "response said. Downstream scoring uses this when computing",
        "`effectDirectionCorrect`."
      ),
      examplesHigh = paste(
        "Example of an `increase` claim: 'the expected outcome increases as X",
        "rises'. Example of a `decrease` claim: 'the expected count falls as X",
        "increases'."
      ),
      examplesLow = paste(
        "`mixed_or_both` is used when both increase and decrease language appear,",
        "and `not_stated` is used when no direction is clearly given."
      ),
      aliases = c("effectDirection"),
      prettyLabels = c("Direction claim")
    ),
    effectScaleClaim = makeEntry(
      canonicalName = "effectScaleClaim",
      group = "claim",
      type = "categorical",
      title = "Effect scale claim",
      description = paste(
        "What scale the explanation uses when describing the effect. The field",
        "tries to distinguish additive interpretations from multiplicative or",
        "probability/odds interpretations."
      ),
      values = "additive, multiplicative, probability_or_odds, mixed_or_unclear, not_stated",
      scoring = paste(
        "This is a claim field. Downstream scoring uses it when computing",
        "`effectScaleAppropriate`."
      ),
      examplesHigh = paste(
        "Additive wording: 'three points higher'. Multiplicative wording: 'about",
        "twice as high' or '20% lower'. Probability/odds wording: 'higher odds' or",
        "'greater probability'."
      ),
      examplesLow = paste(
        "`mixed_or_unclear` is used when the explanation mixes scales or uses a",
        "scale that cannot be classified confidently. `not_stated` means no clear",
        "scale language was found."
      ),
      aliases = c("effectScale"),
      prettyLabels = c("Scale claim")
    ),
    referenceGroupMention = makeEntry(
      canonicalName = "referenceGroupMention",
      group = "claim",
      type = "logical",
      title = "Reference-group mention",
      description = paste(
        "Whether the explanation explicitly mentions the reference or baseline",
        "group. This matters especially for factor predictors and interaction",
        "models where comparisons depend on a baseline category."
      ),
      values = "TRUE or FALSE",
      scoring = paste(
        "This is a claim field. Downstream scoring uses it in fields such as",
        "`referenceGroupHandledCorrectly` and `referenceGroupCoverageAdequate`."
      ),
      examplesHigh = paste(
        "TRUE example: 'relative to the reference group', 'compared with the",
        "baseline level', or explicit naming of the baseline category."
      ),
      examplesLow = paste(
        "FALSE when the explanation discusses contrasts but never indicates what",
        "the comparison is relative to."
      ),
      aliases = c("mentionsReferenceGroup"),
      prettyLabels = c("Reference mention")
    ),
    interactionMention = makeEntry(
      canonicalName = "interactionMention",
      group = "claim",
      type = "logical",
      title = "Interaction mention",
      description = paste(
        "Whether the explanation appears to mention an interaction or a difference",
        "in pattern across groups, levels, or conditions."
      ),
      values = "TRUE or FALSE",
      scoring = paste(
        "This is a claim field. It helps determine whether interaction structure",
        "was covered at all."
      ),
      examplesHigh = paste(
        "TRUE example: 'the effect differs by group', 'the decline is steeper in",
        "Washington', or 'this relationship depends on attendance'."
      ),
      examplesLow = paste(
        "FALSE when the explanation treats all groups as if the same pattern",
        "applied and does not mention any differential effect."
      ),
      aliases = c("mentionsInteraction"),
      prettyLabels = c("Interaction mention")
    ),
    interactionSubstantiveClaim = makeEntry(
      canonicalName = "interactionSubstantiveClaim",
      group = "claim",
      type = "categorical",
      title = "Interaction substantive claim",
      description = paste(
        "What substantive claim the explanation makes about the interaction, if",
        "an interaction is present. This captures whether the explanation claims a",
        "real difference, no clear difference, or fails to mention the issue."
      ),
      values = paste(
        "difference_claimed_strongly, difference_claimed_cautiously,",
        "no_clear_difference, not_mentioned, unclear, not_applicable"
      ),
      scoring = paste(
        "This is a claim field. It feeds into `interactionSubstantiveCorrect` and",
        "`interactionEvidenceAppropriate`."
      ),
      examplesHigh = paste(
        "Cautious difference claim: 'the pattern appears to differ by group'.",
        "Strong difference claim: 'Washington shows a clearly steeper decline'."
      ),
      examplesLow = paste(
        "`not_mentioned` when an interaction exists but the explanation never",
        "addresses it. `no_clear_difference` when the explanation says the data do",
        "not clearly support a difference."
      ),
      aliases = c("interactionClaim"),
      prettyLabels = c("Interaction claim")
    ),
    inferentialRegister = makeEntry(
      canonicalName = "inferentialRegister",
      group = "claim",
      type = "categorical",
      title = "Inferential register",
      description = paste(
        "The overall evidential style of the explanation. It tries to distinguish",
        "careful inferential language from purely descriptive wording or",
        "overclaiming language."
      ),
      values = "inferential, descriptive_only, overclaiming, unclear",
      scoring = paste(
        "This is a claim field. It is later judged through",
        "`inferentialRegisterAppropriate`."
      ),
      examplesHigh = paste(
        "Inferential example: 'the data provide evidence that...' or 'the",
        "interval is consistent with...'."
      ),
      examplesLow = paste(
        "Overclaiming example: 'this proves that...' or 'X causes Y'.",
        "Descriptive-only example: 'the data show...' with no evidential framing."
      ),
      aliases = c("inferentialStyle"),
      prettyLabels = c("Register")
    ),
    uncertaintyTypeClaim = makeEntry(
      canonicalName = "uncertaintyTypeClaim",
      group = "claim",
      type = "categorical",
      title = "Uncertainty type claim",
      description = paste(
        "What sort of uncertainty language the explanation uses, if any. It",
        "distinguishes specific interval-based uncertainty from more generic",
        "cautious wording."
      ),
      values = "none, generic_uncertainty, confidence_interval, mixed, unclear",
      scoring = paste(
        "This is a claim field. It helps interpret how the explanation handles",
        "uncertainty and evidence."
      ),
      examplesHigh = paste(
        "`confidence_interval`: explicit mention of a 95% confidence interval or",
        "credible interval. `generic_uncertainty`: wording such as 'plausible',",
        "'evidence suggests', or 'consistent with the data'."
      ),
      examplesLow = "`none` means no clear uncertainty language was found.",
      aliases = character(0),
      prettyLabels = c("Uncertainty type")
    ),
    ciMention = makeEntry(
      canonicalName = "ciMention",
      group = "claim",
      type = "logical",
      title = "Confidence-interval mention",
      description = "Whether a confidence interval, credible interval, or similar interval estimate is explicitly mentioned.",
      values = "TRUE or FALSE",
      scoring = "Claim field used as evidence that uncertainty was handled explicitly.",
      examplesHigh = "TRUE example: 'the 95% confidence interval ranges from ... to ...'.",
      examplesLow = "FALSE when no interval language appears.",
      aliases = c("mentionsConfidenceInterval"),
      prettyLabels = c("CI mention")
    ),
    percentLanguageMention = makeEntry(
      canonicalName = "percentLanguageMention",
      group = "claim",
      type = "logical",
      title = "Percent-language mention",
      description = "Whether the explanation uses percent wording such as '20%' or '20 percent'.",
      values = "TRUE or FALSE",
      scoring = "Claim field often used as evidence of good multiplicative expression.",
      examplesHigh = "TRUE example: 'about 20% lower'.",
      examplesLow = "FALSE when multiplicative effects are discussed without percentages.",
      aliases = c("usesPercentLanguage"),
      prettyLabels = c("% language")
    ),
    uncertaintyMention = makeEntry(
      canonicalName = "uncertaintyMention",
      group = "claim",
      type = "logical",
      title = "Uncertainty mention",
      description = "Whether any uncertainty or evidential caution is present, including interval language or generic evidence wording.",
      values = "TRUE or FALSE",
      scoring = "Claim field used in several later judgments about calibration and inference.",
      examplesHigh = "TRUE example: 'the interval suggests', 'plausible values', or 'evidence indicates'.",
      examplesLow = "FALSE when the explanation states effects as certain facts without caution.",
      aliases = c("uncertaintyMentioned"),
      prettyLabels = c("Uncertainty mention")
    ),
    usesInferentialLanguage = makeEntry(
      canonicalName = "usesInferentialLanguage",
      group = "claim",
      type = "logical",
      title = "Uses inferential language",
      description = "Whether inferential wording is present at all.",
      values = "TRUE or FALSE",
      scoring = "Claim field used to classify inferential register.",
      examplesHigh = "TRUE example: 'evidence suggests', 'estimated', 'consistent with the data'.",
      examplesLow = "FALSE when the explanation remains purely descriptive.",
      aliases = character(0),
      prettyLabels = c("Inferential words")
    ),
    usesDescriptiveOnlyLanguage = makeEntry(
      canonicalName = "usesDescriptiveOnlyLanguage",
      group = "claim",
      type = "logical",
      title = "Uses descriptive-only language",
      description = "Whether the explanation appears to stay descriptive rather than inferential.",
      values = "TRUE or FALSE",
      scoring = "Claim field that helps separate descriptive register from inferential register.",
      examplesHigh = "TRUE example: 'the data show' or 'on average there were ...' without inferential framing.",
      examplesLow = "FALSE when the explanation also uses evidence or uncertainty language.",
      aliases = character(0),
      prettyLabels = c("Descriptive-only words")
    ),
    overclaimDetected = makeEntry(
      canonicalName = "overclaimDetected",
      group = "claim",
      type = "logical",
      title = "Overclaim detected",
      description = "Whether the explanation uses language that is too strong for ordinary statistical interpretation, such as proof or causation claims.",
      values = "TRUE or FALSE",
      scoring = "Claim flag that strongly affects calibration and may trigger a fatal flaw.",
      examplesHigh = "TRUE example: 'this proves', 'guarantees', or 'X causes Y'.",
      examplesLow = "FALSE when the explanation stays appropriately cautious.",
      aliases = character(0),
      prettyLabels = c("Overclaim")
    ),
    underclaimDetected = makeEntry(
      canonicalName = "underclaimDetected",
      group = "claim",
      type = "logical",
      title = "Underclaim detected",
      description = "Whether the explanation appears unusually weak or hesitant without using proper inferential wording.",
      values = "TRUE or FALSE",
      scoring = "Claim flag that reduces calibration but is less severe than overclaiming.",
      examplesHigh = "TRUE example: repeated use of 'might', 'perhaps', or 'possibly' with no firmer evidence language.",
      examplesLow = "FALSE when the explanation is proportionately confident.",
      aliases = character(0),
      prettyLabels = c("Underclaim")
    ),
    conditionalLanguageMention = makeEntry(
      canonicalName = "conditionalLanguageMention",
      group = "claim",
      type = "logical",
      title = "Conditional-language mention",
      description = "Whether the explanation explicitly conditions an effect on a subgroup, level, or other variable.",
      values = "TRUE or FALSE",
      scoring = "Helpful evidence that interaction structure is being explained clearly.",
      examplesHigh = "TRUE example: 'when Attend = Yes ...', 'the effect depends on location'.",
      examplesLow = "FALSE when subgroup-specific interpretation is absent.",
      aliases = character(0),
      prettyLabels = c("Conditional language")
    ),
    comparisonLanguageMention = makeEntry(
      canonicalName = "comparisonLanguageMention",
      group = "claim",
      type = "logical",
      title = "Comparison-language mention",
      description = "Whether explicit comparative wording appears, such as relative contrasts between groups or conditions.",
      values = "TRUE or FALSE",
      scoring = "Helpful evidence that comparisons are structurally clear.",
      examplesHigh = "TRUE example: 'compared with', 'relative to', 'higher than', or 'whereas'.",
      examplesLow = "FALSE when contrasts are implied but not expressed.",
      aliases = character(0),
      prettyLabels = c("Comparison language")
    ),
    outcomeMention = makeEntry(
      canonicalName = "outcomeMention",
      group = "claim",
      type = "logical",
      title = "Outcome mention",
      description = "Whether the explanation clearly names the response or outcome being modelled.",
      values = "TRUE or FALSE",
      scoring = "Used as a light clarity/completeness signal.",
      examplesHigh = "TRUE when the outcome is clearly named, such as 'expected earthquake count' or 'exam mark'.",
      examplesLow = "FALSE when the explanation talks about change without saying what changes.",
      aliases = character(0),
      prettyLabels = c("Outcome mention")
    ),
    predictorMention = makeEntry(
      canonicalName = "predictorMention",
      group = "claim",
      type = "logical",
      title = "Predictor mention",
      description = "Whether the explanation clearly names the focal predictor or explanatory variable.",
      values = "TRUE or FALSE",
      scoring = "Used as a light clarity/completeness signal.",
      examplesHigh = "TRUE when the explanation clearly says which predictor changes.",
      examplesLow = "FALSE when it describes an effect without naming the driver.",
      aliases = character(0),
      prettyLabels = c("Predictor mention")
    ),
    interactionEvidenceAppropriate = makeEntry(
      canonicalName = "interactionEvidenceAppropriate",
      group = "judged",
      type = "categorical",
      title = "Interaction evidence appropriate",
      description = paste(
        "Whether the strength of the explanation's interaction wording matches",
        "the fitted model's interaction evidence, usually using the minimum",
        "interaction p-value and the interaction alpha threshold."
      ),
      values = "appropriate, too_strong, too_weak, unclear, not_applicable",
      scoring = paste(
        "`appropriate` means the explanation's interaction claim is suitably",
        "matched to the evidence. `too_strong` means it claims a difference too",
        "strongly for the evidence. `too_weak` means it fails to claim a",
        "difference when the interaction evidence is strong."
      ),
      examplesHigh = paste(
        "If the interaction is statistically strong and the explanation says the",
        "pattern differs by group, that is typically `appropriate`. If there is no",
        "interaction term, the value is usually `not_applicable`."
      ),
      examplesLow = paste(
        "If the interaction p-value is weak but the explanation says the groups are",
        "clearly different, that is `too_strong`. If the interaction is strong but",
        "the explanation says there is no clear difference, that is `too_weak`."
      ),
      aliases = c("interactionInference"),
      prettyLabels = c("Interaction evidence")
    ),
    effectDirectionCorrect = makeEntry(
      canonicalName = "effectDirectionCorrect",
      group = "judged",
      type = "0/1/2 score",
      title = "Effect direction correct",
      description = "How well the claimed direction matches the expected or model-implied direction.",
      values = "0, 1, or 2",
      scoring = "2 = direction correct, 1 = partly acceptable or mixed when no expected direction is supplied, 0 = wrong or not stated.",
      examplesHigh = "Score 2 when the explanation says the outcome decreases and that matches the fitted effect.",
      examplesLow = "Score 0 when it claims an increase instead of a decrease, or never states a direction.",
      aliases = character(0),
      prettyLabels = c("Direction correct")
    ),
    effectScaleAppropriate = makeEntry(
      canonicalName = "effectScaleAppropriate",
      group = "judged",
      type = "0/1/2 score",
      title = "Effect scale appropriate",
      description = "Whether the explanation uses the appropriate interpretive scale for the fitted model or expected answer.",
      values = "0, 1, or 2",
      scoring = "2 = appropriate scale, 1 = partly acceptable or mixed when no expected scale is available, 0 = wrong, unclear, or absent.",
      examplesHigh = "Score 2 when a log-link count model is explained multiplicatively, for example with ratios or percentages.",
      examplesLow = "Score 0 when an additive interpretation is used where a multiplicative one is needed, or when the scale is not stated.",
      aliases = character(0),
      prettyLabels = c("Scale appropriate")
    ),
    referenceGroupHandledCorrectly = makeEntry(
      canonicalName = "referenceGroupHandledCorrectly",
      group = "judged",
      type = "0/1/2 score",
      title = "Reference group handled correctly",
      description = "Whether the explanation correctly handles the reference or baseline group structure.",
      values = "0, 1, or 2",
      scoring = "2 = reference framing handled well, 1 = acceptable but limited, 0 = missing or problematic when reference framing is important.",
      examplesHigh = "Score 2 when the explanation clearly states which group is the baseline and interprets contrasts relative to it.",
      examplesLow = "Score 0 when interactions or factor contrasts are discussed without identifying the baseline.",
      aliases = character(0),
      prettyLabels = c("Reference correct")
    ),
    interactionCoverageAdequate = makeEntry(
      canonicalName = "interactionCoverageAdequate",
      group = "judged",
      type = "0/1/2 score",
      title = "Interaction coverage adequate",
      description = "Whether the explanation gives enough coverage to the interaction structure when an interaction is present.",
      values = "0, 1, or 2",
      scoring = "2 = interaction clearly covered, 1 = mentioned but weakly structured, 0 = omitted when needed.",
      examplesHigh = "Score 2 when the explanation explicitly compares how the effect differs across groups or conditions.",
      examplesLow = "Score 0 when a real interaction is present but the explanation ignores it.",
      aliases = character(0),
      prettyLabels = c("Interaction coverage")
    ),
    interactionSubstantiveCorrect = makeEntry(
      canonicalName = "interactionSubstantiveCorrect",
      group = "judged",
      type = "0/1/2 score",
      title = "Interaction substantive correct",
      description = "Whether the substantive claim about the interaction is acceptable, separate from whether it is over- or under-stated statistically.",
      values = "0, 1, or 2",
      scoring = "2 = substantively appropriate, 1 = unclear or only partly satisfactory, 0 = clearly wrong or missing when required.",
      examplesHigh = "Score 2 when the explanation appropriately says the slope differs by group.",
      examplesLow = "Score 0 when it invents a group difference where no interaction exists, or says nothing when an important interaction exists.",
      aliases = character(0),
      prettyLabels = c("Interaction correct")
    ),
    uncertaintyHandlingAppropriate = makeEntry(
      canonicalName = "uncertaintyHandlingAppropriate",
      group = "judged",
      type = "0/1/2 score",
      title = "Uncertainty handling appropriate",
      description = "How well the explanation handles uncertainty and evidential caution.",
      values = "0, 1, or 2",
      scoring = "2 = uncertainty handled well, 1 = acceptable but limited, 0 = overconfident or clearly poor.",
      examplesHigh = "Score 2 when the explanation uses interval language or careful evidence wording appropriately.",
      examplesLow = "Score 0 when it makes strong claims with no uncertainty language, especially if it overclaims.",
      aliases = character(0),
      prettyLabels = c("Uncertainty handling")
    ),
    inferentialRegisterAppropriate = makeEntry(
      canonicalName = "inferentialRegisterAppropriate",
      group = "judged",
      type = "0/1/2 score",
      title = "Inferential register appropriate",
      description = "Whether the overall evidential style is suitable for the model output and the strength of evidence.",
      values = "0, 1, or 2",
      scoring = "2 = well calibrated inferential style, 1 = borderline or only partly appropriate, 0 = overclaiming.",
      examplesHigh = "Score 2 when the explanation uses evidence language without overstating certainty.",
      examplesLow = "Score 0 when it says the model proves or guarantees something.",
      aliases = character(0),
      prettyLabels = c("Register appropriate")
    ),
    mainEffectCoverageAdequate = makeEntry(
      canonicalName = "mainEffectCoverageAdequate",
      group = "judged",
      type = "0/1/2 score",
      title = "Main-effect coverage adequate",
      description = "Whether the explanation covers the main effect direction and scale sufficiently.",
      values = "0, 1, or 2",
      scoring = "2 = both direction and scale covered, 1 = one of these covered, 0 = main effect not meaningfully explained.",
      examplesHigh = "Score 2 when the explanation states both what changes and by how it is measured.",
      examplesLow = "Score 0 when it gives no clear description of the main relationship.",
      aliases = character(0),
      prettyLabels = c("Main-effect coverage")
    ),
    referenceGroupCoverageAdequate = makeEntry(
      canonicalName = "referenceGroupCoverageAdequate",
      group = "judged",
      type = "0/1/2 score",
      title = "Reference-group coverage adequate",
      description = "Whether the explanation gives enough attention to the reference-group structure.",
      values = "0, 1, or 2",
      scoring = "2 = clear reference coverage, 1 = acceptable limited coverage, 0 = insufficient coverage when reference structure matters.",
      examplesHigh = "Score 2 when the baseline category is clearly named and used in comparisons.",
      examplesLow = "Score 0 when interaction or factor comparisons are presented without a reference point.",
      aliases = character(0),
      prettyLabels = c("Reference coverage")
    ),
    clarityAdequate = makeEntry(
      canonicalName = "clarityAdequate",
      group = "judged",
      type = "0/1/2 score",
      title = "Clarity adequate",
      description = "Whether the explanation length and overall expression are reasonably clear.",
      values = "0, 1, or 2",
      scoring = "2 = clear and in the preferred length band, 1 = acceptable, 0 = too short, too long, absent, or otherwise poor.",
      examplesHigh = "Score 2 when the explanation is concise but complete and easy to follow.",
      examplesLow = "Score 0 when it is extremely short, very long, or missing altogether.",
      aliases = character(0),
      prettyLabels = c("Clarity")
    ),
    numericExpressionAdequate = makeEntry(
      canonicalName = "numericExpressionAdequate",
      group = "judged",
      type = "0/1/2 score",
      title = "Numeric expression adequate",
      description = "Whether numeric quantities are expressed in a useful and interpretable way.",
      values = "0, 1, or 2",
      scoring = "2 = numerically expressed well, 1 = acceptable but could be better, 0 = unclear, absent, or poorly scaled.",
      examplesHigh = "Score 2 when multiplicative effects are translated into readable percentages or ratios.",
      examplesLow = "Score 0 when the explanation gives no clear numeric expression or uses a confusing scale.",
      aliases = character(0),
      prettyLabels = c("Numbers")
    ),
    comparisonStructureClear = makeEntry(
      canonicalName = "comparisonStructureClear",
      group = "judged",
      type = "0/1/2 score",
      title = "Comparison structure clear",
      description = "Whether the explanation makes comparison relationships explicit and easy to follow.",
      values = "0, 1, or 2",
      scoring = "2 = comparisons clearly signposted, 1 = acceptable, 0 = unclear especially when interaction structure requires it.",
      examplesHigh = "Score 2 when wording such as 'compared with', 'relative to', and subgroup-specific statements are used clearly.",
      examplesLow = "Score 0 when an interaction exists but the comparison structure is left implicit or confusing.",
      aliases = character(0),
      prettyLabels = c("Comparison structure")
    ),
    fatalFlawDetected = makeEntry(
      canonicalName = "fatalFlawDetected",
      group = "judged",
      type = "logical",
      title = "Fatal flaw detected",
      description = "Whether the run contains a severe enough problem to cap the final score, such as a run error, no explanation, major overclaiming, or a seriously mishandled interaction.",
      values = "TRUE or FALSE",
      scoring = "TRUE does not force the score to zero, but it does cap the maximum overall score using `fatalFlawCap`.",
      examplesHigh = "FALSE is the desirable state: no major fatal issue present.",
      examplesLow = "TRUE when, for example, the run errors, the explanation is missing, or interaction evidence is stated far too strongly.",
      aliases = character(0),
      prettyLabels = c("Fatal flaw")
    ),
    factualScore = makeEntry(
      canonicalName = "factualScore",
      group = "aggregate score",
      type = "continuous score",
      title = "Factual score",
      description = "Average factual quality across direction, scale, reference-group handling, and interaction substance.",
      values = "Numeric from 0 to 1",
      scoring = "Higher is better. Values near 1 mean the explanation is factually well aligned with the model structure.",
      examplesHigh = "A high score usually means the explanation gets direction, scale, baseline structure, and interaction content mostly right.",
      examplesLow = "A low score usually means several of those core factual ingredients are wrong or missing.",
      aliases = character(0),
      prettyLabels = c("Factual score")
    ),
    inferenceScore = makeEntry(
      canonicalName = "inferenceScore",
      group = "aggregate score",
      type = "continuous score",
      title = "Inference score",
      description = "Average quality of uncertainty handling, inferential register, and interaction-evidence calibration.",
      values = "Numeric from 0 to 1",
      scoring = "Higher is better. It reflects whether the explanation sounds statistically appropriate rather than over- or under-stated.",
      examplesHigh = "High when the explanation uses careful evidence language and matches the strength of interaction evidence well.",
      examplesLow = "Low when it overclaims, mishandles uncertainty, or mismatches interaction evidence.",
      aliases = character(0),
      prettyLabels = c("Inference score")
    ),
    completenessScore = makeEntry(
      canonicalName = "completenessScore",
      group = "aggregate score",
      type = "continuous score",
      title = "Completeness score",
      description = "How fully the explanation covers the ingredients that ought to be discussed, such as main effects, interactions, reference structure, and uncertainty.",
      values = "Numeric from 0 to 1",
      scoring = "Higher is better. A low score usually indicates omission of important model features.",
      examplesHigh = "High when the explanation covers main effects, relevant interactions, baseline structure, and some uncertainty.",
      examplesLow = "Low when it leaves out one or more major ingredients.",
      aliases = character(0),
      prettyLabels = c("Completeness score")
    ),
    clarityScore = makeEntry(
      canonicalName = "clarityScore",
      group = "aggregate score",
      type = "continuous score",
      title = "Clarity score",
      description = "How clearly the explanation is expressed, considering length, numeric expression, comparison structure, and whether the outcome or predictor is named.",
      values = "Numeric from 0 to 1",
      scoring = "Higher is better. It is about communicative quality rather than factual correctness alone.",
      examplesHigh = "High when the explanation is readable, appropriately sized, and clearly structured.",
      examplesLow = "Low when it is vague, confusing, missing key labels, or badly sized.",
      aliases = character(0),
      prettyLabels = c("Clarity score")
    ),
    calibrationScore = makeEntry(
      canonicalName = "calibrationScore",
      group = "aggregate score",
      type = "continuous score",
      title = "Calibration score",
      description = "How well calibrated the explanation is in tone, especially with respect to overclaiming and underclaiming.",
      values = "Numeric from 0 to 1",
      scoring = "Usually 1 when well calibrated, 0.5 for underclaiming, and 0 for overclaiming.",
      examplesHigh = "High when the explanation is proportionately confident.",
      examplesLow = "Low when it says too much certainty or, less severely, hedges too weakly without proper inferential language.",
      aliases = character(0),
      prettyLabels = c("Calibration score")
    ),
    overallScore = makeEntry(
      canonicalName = "overallScore",
      group = "aggregate score",
      type = "continuous score",
      title = "Overall score",
      description = "Weighted overall performance score combining factual, inference, completeness, clarity, and calibration dimensions.",
      values = "Numeric from 0 to 100",
      scoring = paste(
        "Higher is better. The score is weighted, may be reduced for exact",
        "duplicates, and may be capped if `fatalFlawDetected` is TRUE."
      ),
      examplesHigh = "A high score means the explanation is generally strong across most dimensions.",
      examplesLow = "A low score means several dimensions are weak, or a fatal flaw or duplicate penalty has pulled the score down.",
      aliases = character(0),
      prettyLabels = c("Overall score")
    ),
    overallPass = makeEntry(
      canonicalName = "overallPass",
      group = "judged",
      type = "logical",
      title = "Overall pass",
      description = "Whether the run's final score exceeds the chosen pass threshold and does not have a fatal flaw.",
      values = "TRUE or FALSE",
      scoring = "TRUE means the run passed by the current rubric settings. FALSE means it did not pass, either because the score was too low or because a fatal flaw was present.",
      examplesHigh = "TRUE when the overall score is above the threshold and no fatal flaw is detected.",
      examplesLow = "FALSE when the score is below threshold or a fatal flaw forces failure.",
      aliases = character(0),
      prettyLabels = c("Overall pass")
    )
  )

  aliasLookup = list()

  for (nm in names(fieldDb)) {
    aliasLookup[[normaliseLookupKey(nm)]] = nm

    aliases = fieldDb[[nm]]$aliases
    if (length(aliases) > 0) {
      for (al in aliases) {
        aliasLookup[[normaliseLookupKey(al)]] = nm
      }
    }

    prettyLabels = fieldDb[[nm]]$prettyLabels
    if (length(prettyLabels) > 0) {
      for (lbl in prettyLabels) {
        aliasLookup[[normaliseLookupKey(lbl)]] = nm
      }
    }
  }

  resolvedName = aliasLookup[[normaliseLookupKey(field)]]

  if (is.null(resolvedName)) {
    availableNames = sort(unique(c(
      names(fieldDb),
      unlist(lapply(fieldDb, function(x) x$aliases)),
      unlist(lapply(fieldDb, function(x) x$prettyLabels))
    )))

    stop(
      paste0(
        "Unknown WMFM field: `", field, "`. Known fields include: ",
        paste(availableNames, collapse = ", "), "."
      ),
      call. = FALSE
    )
  }

  entry = fieldDb[[resolvedName]]

  if (!isTRUE(includeExamples)) {
    entry$examplesHigh = NA_character_
    entry$examplesLow = NA_character_
  }

  if (!isTRUE(includeAliases)) {
    entry$aliases = character(0)
    entry$prettyLabels = character(0)
  }

  if (format == "list") {
    return(entry)
  }

  if (format == "data.frame") {
    return(data.frame(
      canonicalName = entry$canonicalName,
      group = entry$group,
      type = entry$type,
      title = entry$title,
      description = entry$description,
      values = entry$values,
      scoring = entry$scoring,
      examplesHigh = entry$examplesHigh,
      examplesLow = entry$examplesLow,
      aliases = paste(entry$aliases, collapse = ", "),
      prettyLabels = paste(entry$prettyLabels, collapse = ", "),
      stringsAsFactors = FALSE
    ))
  }

  lines = c(
    paste0("Field: ", entry$canonicalName),
    paste0("Title: ", entry$title),
    paste0("Group: ", entry$group),
    paste0("Type: ", entry$type),
    "",
    "What it measures:",
    paste0("  ", entry$description),
    "",
    "Values / range:",
    paste0("  ", entry$values),
    "",
    "How to interpret it:",
    paste0("  ", entry$scoring)
  )

  if (isTRUE(includeAliases) && length(entry$prettyLabels) > 0) {
    lines = c(lines, "", "Heatmap x-axis label(s):", paste0("  ", paste(entry$prettyLabels, collapse = ", ")))
  }

  if (isTRUE(includeAliases) && length(entry$aliases) > 0) {
    lines = c(lines, "", "Recognised aliases:", paste0("  ", paste(entry$aliases, collapse = ", ")))
  }

  if (isTRUE(includeExamples)) {
    lines = c(
      lines,
      "",
      "Example of a stronger / better case:",
      paste0("  ", entry$examplesHigh),
      "",
      "Example of a weaker / poorer case:",
      paste0("  ", entry$examplesLow)
    )
  }

  cat(paste(lines, collapse = "\n"), "\n", sep = "")
  invisible(lines)
}
