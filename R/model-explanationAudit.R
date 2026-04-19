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
      )
    )
  )

  class(out) = c("wmfmExplanationAudit", class(out))
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
    "Avoid raw transformation expressions, coefficient jargon, and unnecessary numerical precision."
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
      backTransformation = "Log-odds quantities are exponentiated to odds, and fitted values are obtained by inverse-logit transformation.",
      explanationScaleNote = "The narrative is instructed to describe effects on the odds scale rather than as raw log-odds coefficients."
    ))
  }

  if (inherits(model, "glm") && identical(model$family$family, "poisson") && identical(model$family$link, "log")) {
    return(list(
      responseExpression = responseExpr,
      responseTransform = responseTransform,
      fittedValueScale = "expected count",
      effectScale = "expected-count multipliers",
      backTransformation = "Log-count quantities are exponentiated to the expected-count scale.",
      explanationScaleNote = "The narrative is instructed to describe multiplicative changes in the expected count rather than raw log coefficients."
    ))
  }

  if (identical(responseTransform, "none")) {
    return(list(
      responseExpression = responseExpr,
      responseTransform = responseTransform,
      fittedValueScale = "response scale",
      effectScale = "additive response-scale differences",
      backTransformation = "No back-transformation is required.",
      explanationScaleNote = "The narrative is instructed to describe additive changes on the response scale."
    ))
  }

  transformLabel = switch(
    responseTransform,
    log = "log scale",
    log1p = "log(1 + y) scale",
    sqrt = "square-root scale",
    paste0(responseTransform, " scale")
  )

  list(
    responseExpression = responseExpr,
    responseTransform = responseTransform,
    fittedValueScale = transformLabel,
    effectScale = paste("additive differences on the", transformLabel),
    backTransformation = "No automatic back-transformation is imposed for transformed lm responses in the current explanation workflow.",
    explanationScaleNote = "The narrative is instructed to stay on the detected transformed scale rather than inventing an unsupported back-transformation."
  )
}

#' Build explanation-audit numeric-anchor metadata
#'
#' @param model A fitted model object.
#' @param mf Model frame for the fitted model.
#' @param predictorNames Predictor names.
#' @param numericAnchorInfo Output from `buildModelNumericAnchorInfo()`.
#'
#' @return A named list.
#' @keywords internal
buildModelExplanationAuditNumericAnchor = function(model, mf, predictorNames, numericAnchorInfo) {

  numericNames = predictorNames[vapply(mf[predictorNames], is.numeric, logical(1))]

  if (length(numericNames) == 0) {
    return(list(
      numericReference = numericAnchorInfo$numericReference,
      note = "No numeric predictors were present, so no special numeric anchor was needed.",
      table = data.frame()
    ))
  }

  fmtValue = function(x) {
    format(round(x, 4), trim = TRUE, scientific = FALSE)
  }

  anchorTable = do.call(
    rbind,
    lapply(numericNames, function(varName) {
      x = stats::na.omit(mf[[varName]])

      if (length(x) == 0) {
        anchorValue = if (identical(numericAnchorInfo$numericReference, "zero")) 0 else NA_real_
        reason = if (identical(numericAnchorInfo$numericReference, "zero")) {
          "0 used because all observed values are missing."
        } else {
          "Sample mean would usually be used, but all observed values are missing."
        }

        return(data.frame(
          predictor = varName,
          observedMin = NA_real_,
          observedMax = NA_real_,
          anchor = anchorValue,
          reason = reason,
          stringsAsFactors = FALSE
        ))
      }

      zeroInRange = min(x) <= 0 && max(x) >= 0
      anchorValue = if (identical(numericAnchorInfo$numericReference, "zero")) 0 else mean(x)
      reason = if (zeroInRange) {
        "0 lies inside the observed range."
      } else {
        "0 lies outside the observed range, so the sample mean is used."
      }

      data.frame(
        predictor = varName,
        observedMin = as.numeric(min(x)),
        observedMax = as.numeric(max(x)),
        anchor = as.numeric(anchorValue),
        reason = reason,
        stringsAsFactors = FALSE
      )
    })
  )

  anchorTable$observedRange = paste0(
    "[",
    ifelse(is.na(anchorTable$observedMin), "NA", fmtValue(anchorTable$observedMin)),
    ", ",
    ifelse(is.na(anchorTable$observedMax), "NA", fmtValue(anchorTable$observedMax)),
    "]"
  )
  anchorTable = anchorTable[, c("predictor", "observedRange", "anchor", "reason"), drop = FALSE]

  list(
    numericReference = numericAnchorInfo$numericReference,
    note = buildNumericAnchorUiNote(model = model, mf = mf, predictorNames = predictorNames),
    table = anchorTable
  )
}

#' Build explanation-audit factor reference-level metadata
#'
#' @param mf Model frame for the fitted model.
#' @param predictorNames Predictor names.
#'
#' @return A data frame.
#' @keywords internal
buildModelExplanationAuditReferenceLevels = function(mf, predictorNames) {

  factorNames = predictorNames[vapply(mf[predictorNames], is.factor, logical(1))]

  if (length(factorNames) == 0) {
    return(data.frame())
  }

  do.call(
    rbind,
    lapply(factorNames, function(varName) {
      x = mf[[varName]]
      data.frame(
        predictor = varName,
        referenceLevel = levels(x)[1],
        levels = paste(levels(x), collapse = ", "),
        stringsAsFactors = FALSE
      )
    })
  )
}

#' Build explanation-audit confidence-interval metadata
#'
#' @param ciData Output from `buildModelConfidenceIntervalData()`.
#'
#' @return A named list.
#' @keywords internal
buildModelExplanationAuditConfidenceIntervals = function(ciData) {

  ciTable = ciData$table %||% data.frame()

  if (!is.data.frame(ciTable) || nrow(ciTable) == 0) {
    return(list(
      level = 0.95,
      mode = ciData$mode %||% NA_character_,
      note = ciData$note %||% NA_character_,
      teachingNote = ciData$teachingNote %||% NA_character_,
      displayedScales = character(0)
    ))
  }

  displayedScales = unique(ciTable$scale)
  displayedScales = displayedScales[!is.na(displayedScales) & nzchar(displayedScales)]

  list(
    level = 0.95,
    mode = ciData$mode %||% NA_character_,
    note = ciData$note %||% NA_character_,
    teachingNote = ciData$teachingNote %||% NA_character_,
    displayedScales = displayedScales
  )
}

#' Build explanation-audit evidence tables
#'
#' @param ciData Output from `buildModelConfidenceIntervalData()`.
#' @param section CI section or sections to keep.
#'
#' @return A data frame.
#' @keywords internal
buildModelExplanationAuditEvidenceTable = function(ciData, section) {

  ciTable = ciData$table %||% data.frame()

  if (!is.data.frame(ciTable) || nrow(ciTable) == 0 || !("ciSection" %in% names(ciTable))) {
    return(data.frame())
  }

  out = ciTable[ciTable$ciSection %in% section, , drop = FALSE]

  keepNames = intersect(
    c("ciSection", "quantity", "estimate", "lower", "upper", "scale", "displayScale"),
    names(out)
  )

  out[, keepNames, drop = FALSE]
}
