#' Build a model coefficient table while muffling known perfect-fit warnings
#'
#' @param model A fitted model object.
#'
#' @return A data frame of coefficient summaries.
#' @keywords internal
buildModelExplanationAuditCoefficientTable = function(model) {

  coefficientTable = withCallingHandlers(
    as.data.frame(stats::coef(summary(model))),
    warning = function(w) {
      if (grepl("essentially perfect fit", conditionMessage(w), fixed = TRUE)) {
        invokeRestart("muffleWarning")
      }
    }
  )

  coefficientTable$term = rownames(coefficientTable)
  rownames(coefficientTable) = NULL
  coefficientTable[, c("term", setdiff(names(coefficientTable), "term")), drop = FALSE]
}

#' Build a deterministic audit trail for a model explanation
#'
#' Creates a structured, student-facing summary of the deterministic inputs,
#' interpretation choices, and supporting model quantities used when WMFM asks
#' the language model to write a plain-language explanation.
#'
#' This audit is intentionally not presented as hidden reasoning or private
#' chain-of-thought. Instead, it records inspectable ingredients such as prompt
#' rules, interpretation scale, numeric anchors, reference levels, and
#' confidence-interval evidence.
#'
#' The returned object is expected to follow a stable top-level contract with
#' sections for overview metadata, prompt inputs, prompt rules,
#' interpretation-scale metadata, numeric anchors, reference levels,
#' confidence-interval summary, baseline and effect evidence, coefficient data,
#' and raw prompt ingredients.
#'
#' @param model A fitted model object, typically of class `lm` or `glm`.
#'
#' @return An object of class `wmfmExplanationAudit`.
#' @keywords internal
#' @importFrom stats coef formula model.frame na.omit
buildModelExplanationAudit = function(model) {

  mf = stats::model.frame(model)
  predictorNames = names(mf)[-1]
  responseName = names(mf)[1]
  numericAnchorInfo = buildModelNumericAnchorInfo(
    model = model,
    mf = mf,
    predictorNames = predictorNames
  )
  ciData = buildModelConfidenceIntervalData(
    model = model,
    numericReference = numericAnchorInfo$numericReference
  )

  coefficientTable = buildModelExplanationAuditCoefficientTable(model = model)
  numericColumns = vapply(coefficientTable, is.numeric, logical(1))
  coefficientTable[numericColumns] = lapply(coefficientTable[numericColumns], round, digits = 4)

  out = list(
    transparencyNote = paste(
      "This panel shows deterministic inputs and evidence used to construct the explanation.",
      "It does not claim to reveal hidden chain-of-thought."
    ),
    overview = buildModelExplanationAuditOverview(
      model = model,
      mf = mf,
      predictorNames = predictorNames,
      responseName = responseName
    ),
    promptInputs = buildModelExplanationAuditPromptInputs(
      model = model,
      mf = mf,
      predictorNames = predictorNames,
      responseName = responseName
    ),
    promptRules = buildModelExplanationAuditPromptRules(model = model),
    interpretationScale = buildModelExplanationAuditScaleInfo(model = model),
    numericAnchor = buildModelExplanationAuditNumericAnchor(
      model = model,
      mf = mf,
      predictorNames = predictorNames,
      numericAnchorInfo = numericAnchorInfo
    ),
    referenceLevels = buildModelExplanationAuditReferenceLevels(
      mf = mf,
      predictorNames = predictorNames
    ),
    confidenceIntervals = buildModelExplanationAuditConfidenceIntervals(
      ciData = ciData
    ),
    baselineEvidence = buildModelExplanationAuditEvidenceTable(
      ciData = ciData,
      section = "baseline"
    ),
    effectEvidence = buildModelExplanationAuditEvidenceTable(
      ciData = ciData,
      section = c("effect", "contrast")
    ),
    coefficientTable = coefficientTable,
    rawPromptIngredients = list(
      languageContract = buildWmfmLanguageContractText(context = "summary"),
      numericAnchorPrompt = numericAnchorInfo$promptText,
      anchoredBaselinePrompt = buildAnchoredBaselinePromptBlock(
        model = model,
        mf = mf,
        predictorNames = predictorNames
      ),
      formattedQuantityPrompt = buildFormattedPromptQuantityBlock(
        model = model,
        mf = mf,
        predictorNames = predictorNames
      ),
      explanationSkeletonPrompt = buildExplanationSkeletonPromptBlock(
        model = model,
        mf = mf
      ),
      responseScaleControlPrompt = buildResponseScaleControlPromptBlock(
        model = model,
        mf = mf
      ),
      comparisonControlPrompt = buildComparisonControlPromptBlock(
        model = model,
        mf = mf
      ),
      promptValidationGuardPrompt = buildPromptValidationGuardBlock(
        model = model,
        mf = mf
      )
    )
  )

  class(out) = c("wmfmExplanationAudit", class(out))
  validateWmfmExplanationAudit(x = out)
  out
}

#' Build explanation-audit overview metadata
#'
#' @param model A fitted model object.
#' @param mf Model frame for the fitted model.
#' @param predictorNames Predictor names.
#' @param responseName Response variable name.
#'
#' @return A named list.
#' @keywords internal
buildModelExplanationAuditOverview = function(model, mf, predictorNames, responseName) {

  modelFamily = if (inherits(model, "glm")) {
    model$family$family
  } else {
    "gaussian"
  }

  linkName = if (inherits(model, "glm")) {
    model$family$link
  } else {
    "identity"
  }

  list(
    response = responseName,
    predictors = predictorNames,
    nObservations = nrow(mf),
    modelFamily = modelFamily,
    link = linkName,
    hasDatasetContext = !is.null(attr(model, "wmfm_dataset_doc", exact = TRUE)),
    hasResearchQuestion = nzchar(trimws(attr(model, "wmfm_research_question", exact = TRUE) %||% ""))
  )
}

#' Build explanation-audit prompt-input metadata
#'
#' @param model A fitted model object.
#' @param mf Model frame for the fitted model.
#' @param predictorNames Predictor names.
#' @param responseName Response variable name.
#'
#' @return A named list.
#' @keywords internal
buildModelExplanationAuditPromptInputs = function(model, mf, predictorNames, responseName) {

  researchQuestion = trimws(attr(model, "wmfm_research_question", exact = TRUE) %||% "")
  datasetContext = attr(model, "wmfm_dataset_doc", exact = TRUE)
  datasetName = attr(model, "wmfm_dataset_name", exact = TRUE) %||% NA_character_
  responseNounPhrase = attr(model, "wmfm_response_noun_phrase", exact = TRUE) %||% responseName

  list(
    response = responseName,
    responseNounPhrase = responseNounPhrase,
    predictors = predictorNames,
    datasetContextUsed = !is.null(datasetContext),
    datasetName = datasetName,
    researchQuestionUsed = nzchar(researchQuestion),
    researchQuestion = if (nzchar(researchQuestion)) researchQuestion else NA_character_,
    coefficientTableIncluded = TRUE,
    confidenceIntervalsIncluded = TRUE,
    formattedQuantitiesIncluded = TRUE,
    explanationSkeletonIncluded = TRUE,
    responseScaleControlIncluded = TRUE,
    comparisonControlIncluded = TRUE,
    promptValidationGuardIncluded = TRUE,
    rawCoefficientTableRetainedInAudit = TRUE,
    precomputedBaselineValuesIncluded = TRUE,
    numericAnchorRuleIncluded = TRUE,
    nObservations = nrow(mf)
  )
}

#' Build explanation-audit prompt-rule summary
#'
#' @param model A fitted model object.
#'
#' @return A character vector of concise prompt-side rules.
#' @keywords internal
buildModelExplanationAuditPromptRules = function(model) {

  rules = c(
    "Start in plain language with the outcome and the main comparison rather than model mechanics.",
    "Interpret effects on a single student-facing scale rather than leaving them on the raw coefficient scale.",
    "Use the chosen numeric anchor for baseline and conditional interpretation instead of automatically using 0.",
    "Use confidence intervals to support cautious conclusions about direction and size, without treating them as hypothesis tests.",
    "Avoid raw transformation expressions, coefficient jargon, and unnecessary numerical precision.",
    "Use comparison-scope guidance to avoid unnecessary exhaustive pairwise treatment or group comparisons.",
    "Use deterministic validation-guard targets to flag likely prompt failures without automatically regenerating explanations."
  )

  researchQuestion = trimws(attr(model, "wmfm_research_question", exact = TRUE) %||% "")

  if (nzchar(researchQuestion)) {
    rules = c(
      rules,
      "Use the user-supplied research question to frame the opening and closing of the explanation."
    )
  }

  rules
}

#' Build explanation-audit interpretation-scale metadata
#'
#' @param model A fitted model object.
#'
#' @return A named list.
#' @keywords internal
buildModelExplanationAuditScaleInfo = function(model) {

  responseExpr = paste(deparse(stats::formula(model)[[2]]), collapse = " ")
  responseTransform = detectRespTransform(responseExpr) %||% "none"

  if (inherits(model, "glm") && identical(model$family$family, "binomial") && identical(model$family$link, "logit")) {
    return(list(
      responseExpression = responseExpr,
      responseTransform = responseTransform,
      fittedValueScale = "probability and odds",
      effectScale = "odds multipliers",
      backTransformation = "Coefficients are converted from log-odds to probabilities and odds using the inverse-logit transformation."
    ))
  }

  if (inherits(model, "glm") && identical(model$family$family, "poisson") && identical(model$family$link, "log")) {
    return(list(
      responseExpression = responseExpr,
      responseTransform = responseTransform,
      fittedValueScale = "expected count",
      effectScale = "expected-count multipliers",
      backTransformation = "Coefficients are converted from the log scale to the expected-count scale using exponentiation."
    ))
  }

  list(
    responseExpression = responseExpr,
    responseTransform = responseTransform,
    fittedValueScale = "response scale",
    effectScale = "additive response-scale differences",
    backTransformation = "No back-transformation is required."
  )
}

#' Build explanation-audit numeric-anchor summary
#'
#' @param model A fitted model object.
#' @param mf Model frame for the fitted model.
#' @param predictorNames Predictor names.
#' @param numericAnchorInfo Numeric anchor info from
#'   `buildModelNumericAnchorInfo()`.
#'
#' @return A named list.
#' @keywords internal
buildModelExplanationAuditNumericAnchor = function(model, mf, predictorNames, numericAnchorInfo) {

  numericPredictors = predictorNames[vapply(mf[predictorNames], is.numeric, logical(1))]

  if (length(numericPredictors) == 0) {
    return(list(
      numericReference = numericAnchorInfo$numericReference,
      note = "No numeric predictors were present, so no numeric anchor table was needed.",
      table = data.frame(
        predictor = character(0),
        observedRange = character(0),
        anchor = numeric(0),
        reason = character(0),
        stringsAsFactors = FALSE
      )
    ))
  }

  anchorRows = lapply(numericPredictors, function(name) {
    x = mf[[name]]
    observedMin = min(x, na.rm = TRUE)
    observedMax = max(x, na.rm = TRUE)
    zeroInRange = observedMin <= 0 && observedMax >= 0
    anchor = if (zeroInRange) 0 else mean(x, na.rm = TRUE)
    reason = if (zeroInRange) {
      "0 is inside the observed range, so it is used as the interpretation anchor."
    } else {
      "0 is outside the observed range, so the sample mean is used as the interpretation anchor."
    }

    data.frame(
      predictor = name,
      observedRange = paste0("[", round(observedMin, 4), ", ", round(observedMax, 4), "]"),
      anchor = round(anchor, 4),
      reason = reason,
      stringsAsFactors = FALSE
    )
  })

  note = if (identical(numericAnchorInfo$numericReference, "zero")) {
    "At least one numeric predictor included 0 in its observed range, so zero-based interpretation was available."
  } else {
    "No numeric predictor included 0 in its observed range, so mean-based interpretation was used instead."
  }

  list(
    numericReference = numericAnchorInfo$numericReference,
    note = note,
    table = do.call(rbind, anchorRows)
  )
}

#' Build explanation-audit reference-level summary
#'
#' @param mf Model frame for the fitted model.
#' @param predictorNames Predictor names.
#'
#' @return A data frame.
#' @keywords internal
buildModelExplanationAuditReferenceLevels = function(mf, predictorNames) {

  factorPredictors = predictorNames[vapply(mf[predictorNames], is.factor, logical(1))]

  if (length(factorPredictors) == 0) {
    return(data.frame(
      predictor = character(0),
      referenceLevel = character(0),
      levels = character(0),
      stringsAsFactors = FALSE
    ))
  }

  out = lapply(factorPredictors, function(name) {
    x = mf[[name]]
    data.frame(
      predictor = name,
      referenceLevel = levels(x)[1],
      levels = paste(levels(x), collapse = ", "),
      stringsAsFactors = FALSE
    )
  })

  do.call(rbind, out)
}

#' Build explanation-audit confidence-interval summary
#'
#' @param ciData Confidence-interval data from
#'   `buildModelConfidenceIntervalData()`.
#'
#' @return A named list.
#' @keywords internal
buildModelExplanationAuditConfidenceIntervals = function(ciData) {

  if (!is.data.frame(ciData) || nrow(ciData) == 0) {
    return(list(
      level = NA_real_,
      mode = NA_character_,
      note = NA_character_,
      teachingNote = NA_character_,
      displayedScales = character(0)
    ))
  }

  list(
    level = unique(ciData$level)[1],
    mode = unique(ciData$mode)[1],
    note = unique(ciData$note)[1],
    teachingNote = unique(ciData$teachingNote)[1],
    displayedScales = unique(stats::na.omit(ciData$displayScale))
  )
}

#' Build explanation-audit evidence table for CI sections
#'
#' @param ciData Confidence-interval data from
#'   `buildModelConfidenceIntervalData()`.
#' @param section Character vector of CI sections to keep.
#'
#' @return A data frame.
#' @keywords internal
buildModelExplanationAuditEvidenceTable = function(ciData, section) {

  if (!is.data.frame(ciData) || nrow(ciData) == 0) {
    return(data.frame())
  }

  keep = ciData$section %in% section

  if (!any(keep)) {
    return(data.frame())
  }

  out = ciData[keep, c(
    "section",
    "label",
    "estimate",
    "lower",
    "upper",
    "scale",
    "displayScale"
  ), drop = FALSE]

  names(out) = c(
    "ciSection",
    "quantity",
    "estimate",
    "lower",
    "upper",
    "scale",
    "displayScale"
  )

  numericColumns = vapply(out, is.numeric, logical(1))
  out[numericColumns] = lapply(out[numericColumns], round, digits = 4)
  rownames(out) = NULL
  out
}
