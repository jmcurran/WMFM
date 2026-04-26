#' Build deterministic model-aware explanation rules
#'
#' Converts explanation model-profile metadata into a compact set of
#' deterministic rules for later prompt construction, validation, and developer
#' feedback. This function does not generate explanation text and does not call
#' an LLM. It records the scale, comparison scope, skeleton, language guidance,
#' avoided terms, and quality flags implied by the model profile.
#'
#' @param modelProfile A model-profile list, usually returned by
#'   `buildExplanationModelProfile()`.
#'
#' @return A named list of deterministic model-aware explanation rules.
#'
#' @keywords internal
buildExplanationRuleProfile = function(modelProfile) {

  validateExplanationModelProfileForRules(modelProfile)

  modelFamily = modelProfile$modelFamily
  modelStructure = modelProfile$modelStructure
  predictorTypes = normalizeExplanationRulePredictorTypes(modelProfile$predictorTypes)
  hasInteractions = isTRUE(modelProfile$hasInteractions)
  comparisonScope = getExplanationRuleScalar(
    modelProfile$comparisonScope,
    default = getExplanationRuleComparisonScope(
      modelStructure = modelStructure,
      predictorTypes = predictorTypes,
      hasInteractions = hasInteractions
    )
  )

  skeletonId = selectExplanationSkeletonId(
    modelFamily = modelFamily,
    modelStructure = modelStructure,
    hasInteractions = hasInteractions
  )

  list(
    skeletonId = skeletonId,
    skeletonSteps = buildExplanationSkeletonSteps(
      skeletonId = skeletonId,
      comparisonScope = comparisonScope
    ),
    modelFamily = modelFamily,
    modelStructure = modelStructure,
    modelScale = getExplanationRuleScalar(modelProfile$modelScale, default = "unknown"),
    interpretationScale = getExplanationRuleScalar(
      modelProfile$interpretationScale,
      default = "response"
    ),
    responseVariable = getExplanationRuleScalar(
      modelProfile$responseVariable,
      default = NA_character_
    ),
    transformationType = getExplanationRuleScalar(
      modelProfile$transformationType,
      default = "none"
    ),
    comparisonScope = comparisonScope,
    comparisonGuidance = buildExplanationComparisonGuidance(comparisonScope),
    scaleGuidance = buildExplanationScaleGuidance(modelProfile),
    effectLanguage = buildExplanationEffectLanguage(
      modelStructure = modelStructure,
      predictorTypes = predictorTypes,
      hasInteractions = hasInteractions
    ),
    avoidTerms = buildExplanationAvoidTerms(
      modelFamily = modelFamily,
      modelStructure = modelStructure,
      hasInteractions = hasInteractions
    ),
    qualityFlagsToCheck = buildExplanationModelAwareQualityFlags(
      modelFamily = modelFamily,
      modelStructure = modelStructure,
      predictorTypes = predictorTypes,
      hasInteractions = hasInteractions,
      comparisonScope = comparisonScope
    ),
    sourceFunction = "buildExplanationRuleProfile"
  )
}

validateExplanationModelProfileForRules = function(modelProfile) {

  if (!is.list(modelProfile)) {
    stop("`modelProfile` must be a list.", call. = FALSE)
  }

  requiredNames = c("modelFamily", "modelStructure")
  missingNames = setdiff(requiredNames, names(modelProfile))

  if (length(missingNames) > 0) {
    stop(
      "`modelProfile` is missing required fields: ",
      paste(missingNames, collapse = ", "),
      call. = FALSE
    )
  }

  invisible(TRUE)
}

getExplanationRuleScalar = function(x, default) {

  if (is.null(x) || length(x) == 0 || is.na(x[1])) {
    return(default)
  }

  as.character(x[1])
}

normalizeExplanationRulePredictorTypes = function(predictorTypes) {

  out = list(
    numeric = character(0),
    factor = character(0),
    other = character(0)
  )

  if (!is.list(predictorTypes)) {
    return(out)
  }

  for (fieldName in names(out)) {
    if (!is.null(predictorTypes[[fieldName]])) {
      out[[fieldName]] = as.character(predictorTypes[[fieldName]])
    }
  }

  out
}

getExplanationRuleComparisonScope = function(modelStructure, predictorTypes, hasInteractions) {

  if (identical(modelStructure, "interceptOnly")) {
    return("none")
  }

  if (isTRUE(hasInteractions)) {
    return("targeted")
  }

  if (length(predictorTypes$factor) > 0) {
    return("minimal")
  }

  "minimal"
}

selectExplanationSkeletonId = function(modelFamily, modelStructure, hasInteractions) {

  if (identical(modelStructure, "interceptOnly")) {
    return(paste(modelFamily, "interceptOnly", sep = "_"))
  }

  if (isTRUE(hasInteractions) || identical(modelStructure, "interaction")) {
    return(paste(modelFamily, "interaction", sep = "_"))
  }

  paste(modelFamily, modelStructure, sep = "_")
}

buildExplanationSkeletonSteps = function(skeletonId, comparisonScope) {

  if (grepl("_interceptOnly$", skeletonId)) {
    return(buildExplanationSkeletonDataFrame(c(
      estimate = "Give the overall estimate on the interpretation scale, using the supplied formatted quantity.",
      uncertainty = "Give the confidence interval if available, keeping it next to the estimate and framing it as uncertainty about the underlying average, probability, or expected count.",
      answer = "Answer the research question directly using the numeric estimate and uncertainty, without adding model-mechanics filler."
    )))
  }

  if (grepl("_interaction$", skeletonId)) {
    return(buildExplanationSkeletonDataFrame(c(
      firstWithinGroupEffect = "Describe the effect within the first relevant group or selected value, keeping the estimate and uncertainty together if available.",
      secondWithinGroupEffect = "Describe the same kind of effect within the second relevant group or selected value, using the same interpretation scale.",
      effectComparison = "Compare the within-group effects directly, focusing on whether one effect is larger, smaller, steeper, weaker, or in a different direction.",
      interactionConclusion = "State what the different within-group effects mean for the research question in plain language.",
      answer = "Answer the research question with a clear closing takeaway that uses the key within-group comparison and avoids decomposing coefficients or naming the interaction term."
    )))
  }

  if (grepl("factorMainEffect$", skeletonId)) {
    return(buildExplanationSkeletonDataFrame(c(
      modelContext = "Identify the groups or treatments being compared.",
      comparisonSummary = buildExplanationSkeletonComparisonStep(comparisonScope),
      evidence = "Give targeted numeric support only when it helps answer the research question.",
      uncertainty = "Give uncertainty if available, keeping it with the comparison or estimate.",
      answer = "Answer the research question with a clear closing takeaway that uses the most relevant comparison and avoids listing unnecessary pairwise comparisons."
    )))
  }

  if (grepl("numericMainEffect$", skeletonId)) {
    return(buildExplanationSkeletonDataFrame(c(
      typicalCase = "Describe the fitted value at a meaningful anchor if needed.",
      effect = "Describe the effect of a change in the numeric predictor.",
      uncertainty = "Give uncertainty if available, keeping it with the effect estimate.",
      answer = "Answer the research question with a clear closing takeaway that states the direction and size of change."
    )))
  }

  buildExplanationSkeletonDataFrame(c(
    modelContext = "Briefly identify the predictors without turning the explanation into model mechanics.",
    effect = "Explain one relevant effect at a time on the interpretation scale.",
    comparisonSummary = buildExplanationSkeletonComparisonStep(comparisonScope),
    uncertainty = "Give uncertainty if available, keeping it next to the relevant estimate.",
    answer = "Answer the research question with a clear closing takeaway using only relevant effects or comparisons."
  ))
}

buildExplanationSkeletonDataFrame = function(stepMap) {

  data.frame(
    stepId = seq_along(stepMap),
    stepRole = names(stepMap),
    instruction = unname(stepMap),
    stringsAsFactors = FALSE
  )
}

buildExplanationSkeletonComparisonStep = function(comparisonScope) {

  if (identical(comparisonScope, "none")) {
    return("Do not include explicit group comparisons.")
  }

  if (identical(comparisonScope, "targeted")) {
    return("Compare only groups, values, or treatment combinations that answer the research question.")
  }

  if (identical(comparisonScope, "full")) {
    return("List comparisons only when the research question explicitly asks for them.")
  }

  "Summarise group or treatment differences without enumerating all pairwise comparisons."
}

buildExplanationComparisonGuidance = function(comparisonScope) {

  switch(
    comparisonScope,
    none = "Do not make group or treatment comparisons because the model has no comparison structure.",
    targeted = "Use only targeted comparisons that answer the research question or explain an interaction.",
    full = "Full comparison output is allowed only when explicitly requested and the comparison set is small.",
    minimal = "Summarise differences without enumerating all pairwise group or treatment comparisons.",
    "Summarise differences without enumerating all pairwise group or treatment comparisons."
  )
}

buildExplanationScaleGuidance = function(modelProfile) {

  modelFamily = getExplanationRuleScalar(modelProfile$modelFamily, default = "unknown")
  modelScale = getExplanationRuleScalar(modelProfile$modelScale, default = "unknown")
  interpretationScale = getExplanationRuleScalar(
    modelProfile$interpretationScale,
    default = "response"
  )
  transformationType = getExplanationRuleScalar(
    modelProfile$transformationType,
    default = "none"
  )

  if (identical(modelFamily, "logistic")) {
    return(paste(
      "Explain fitted values as probabilities.",
      "Use odds ratios or odds multipliers only for effect sizes and direct comparisons.",
      "Do not mix raw odds, probabilities, and odds ratios in the same reasoning chain; avoid log-odds."
    ))
  }

  if (identical(modelFamily, "poisson")) {
    return("Explain fitted values as expected counts and effects as multiplicative count changes; avoid log expected counts.")
  }

  if (!identical(transformationType, "none") || identical(interpretationScale, "originalResponse")) {
    return(paste0(
      "The model is fit on ", modelScale,
      ", but student-facing explanations should use the interpretation scale where possible."
    ))
  }

  "Explain estimates and effects on the response scale."
}

buildExplanationEffectLanguage = function(modelStructure, predictorTypes, hasInteractions) {

  if (identical(modelStructure, "interceptOnly")) {
    return("Do not describe predictor effects because the model has no predictors.")
  }

  if (isTRUE(hasInteractions) || identical(modelStructure, "interaction")) {
    return("Describe the effect within each relevant group or selected value first, then compare the within-group effects directly.")
  }

  hasNumeric = length(predictorTypes$numeric) > 0
  hasFactor = length(predictorTypes$factor) > 0

  if (hasNumeric && hasFactor) {
    return("Use change language for numeric predictors and comparison language for factor or treatment levels.")
  }

  if (hasNumeric) {
    return("Use explicit change language, such as the effect of a one-unit increase or another meaningful change.")
  }

  if (hasFactor) {
    return("Use group, level, or treatment comparison language without listing unnecessary pairwise comparisons.")
  }

  "Describe relevant effects in plain language on the interpretation scale."
}

buildExplanationAvoidTerms = function(modelFamily, modelStructure, hasInteractions) {

  out = c(
    "fitted model",
    "regression results",
    "coefficient",
    "intercept"
  )

  if (identical(modelFamily, "logistic")) {
    out = c(out, "log-odds", "logit")
  }

  if (identical(modelFamily, "poisson")) {
    out = c(out, "log count", "log expected count")
  }

  if (identical(modelStructure, "interaction") || isTRUE(hasInteractions)) {
    out = c(out, "interaction term")
  }

  unique(out)
}

buildExplanationModelAwareQualityFlags = function(
    modelFamily,
    modelStructure,
    predictorTypes,
    hasInteractions,
    comparisonScope) {

  out = c(
    "technicalLanguageLeakage",
    "responseScaleMismatch",
    "numericVerbalMismatch"
  )

  if (identical(modelStructure, "interceptOnly")) {
    out = c(out, "interceptOnlyMentionsPredictors", "interceptOnlyUsesBaselineLanguage")

    if (identical(modelFamily, "lm")) {
      out = c(out, "lmInterceptOnlyUsesRSquared")
    }
  }

  if (identical(modelFamily, "logistic")) {
    out = c(out, "logisticMentionsLogOdds")
  }

  if (identical(modelFamily, "poisson")) {
    out = c(out, "poissonMentionsLogCount")
  }

  if (length(predictorTypes$numeric) > 0) {
    out = c(out, "effectWithoutChangeLanguage")
  }

  if (length(predictorTypes$factor) > 0) {
    out = c(out, "excessiveComparisons", "irrelevantComparisons", "comparisonExplosion")
  }

  if (identical(modelStructure, "interaction") || isTRUE(hasInteractions)) {
    out = c(
      out,
      "interactionTermMentioned",
      "interactionExplainedAsComponent",
      "interactionNotCompared",
      "interactionWithoutGroupContext"
    )
  }

  if (identical(comparisonScope, "minimal")) {
    out = c(out, "excessiveComparisons")
  }

  unique(out)
}
