#' Build a student-facing teaching summary for a model explanation
#'
#' Derives a plain-language teaching summary from the deterministic explanation
#' audit. The returned object is designed for student-facing display and avoids
#' exposing raw prompt ingredients or developer-facing internal structures.
#'
#' @param audit A `wmfmExplanationAudit` object.
#' @param model A fitted model object, typically of class `lm` or `glm`.
#' @param researchQuestion Optional character string giving the research
#'   question the model is being used to address. When `NULL`, the function
#'   falls back to any research question stored on the fitted model.
#'
#' @return An object of class `wmfmExplanationTeachingSummary`.
#' @export
#' @importFrom stats formula model.frame na.omit
buildExplanationTeachingSummary = function(audit, model, researchQuestion = NULL) {

  if (is.null(audit)) {
    stop("`audit` must not be NULL.", call. = FALSE)
  }

  if (!inherits(model, c("lm", "glm"))) {
    stop("`model` must be a fitted lm or glm object.", call. = FALSE)
  }

  if (is.null(researchQuestion)) {
    researchQuestion = trimws(attr(model, "wmfm_research_question", exact = TRUE) %||% "")
  }

  if (!is.character(researchQuestion) || length(researchQuestion) != 1 || is.na(researchQuestion)) {
    stop("`researchQuestion` must be NULL or a single non-missing character string.", call. = FALSE)
  }

  mf = stats::model.frame(model)
  responseName = all.vars(stats::formula(model)[[2]])[1] %||% names(mf)[1] %||% "the response"
  predictorNames = names(mf)[-1]
  numericPredictors = predictorNames[vapply(mf[predictorNames], is.numeric, logical(1))]
  factorPredictors = predictorNames[vapply(mf[predictorNames], is.factor, logical(1))]

  out = list(
    interpretationScale = buildExplanationTeachingInterpretationScale(
      audit = audit,
      responseName = responseName
    ),
    baselineChoice = buildExplanationTeachingBaselineChoice(
      audit = audit,
      numericPredictors = numericPredictors,
      factorPredictors = factorPredictors
    ),
    xChangeDescription = buildExplanationTeachingXChangeDescription(
      responseName = responseName,
      numericPredictors = numericPredictors,
      factorPredictors = factorPredictors,
      audit = audit
    ),
    mainEffectDescription = buildExplanationTeachingMainEffectDescription(
      audit = audit,
      responseName = responseName,
      numericPredictors = numericPredictors,
      factorPredictors = factorPredictors
    ),
    uncertaintySummary = buildExplanationTeachingUncertaintySummary(
      audit = audit,
      responseName = responseName
    ),
    evidenceTable = buildExplanationTeachingEvidenceTable(
      audit = audit,
      responseName = responseName,
      numericPredictors = numericPredictors,
      factorPredictors = factorPredictors,
      researchQuestion = researchQuestion
    ),
    researchQuestionLink = buildExplanationTeachingResearchQuestionLink(
      researchQuestion = researchQuestion,
      responseName = responseName,
      predictorNames = predictorNames
    )
  )

  class(out) = c("wmfmExplanationTeachingSummary", class(out))
  out
}

#' Format a variable name for student-facing inline display
#'
#' @param x A variable name.
#'
#' @return A single character string wrapped in backticks.
#' @keywords internal
formatExplanationTeachingVariable = function(x) {

  paste0("`", x, "`")
}

#' Collapse variable names into a readable list
#'
#' @param x Character vector of variable names.
#'
#' @return A single character string.
#' @keywords internal
collapseExplanationTeachingVariables = function(x) {

  x = x[!is.na(x) & nzchar(x)]

  if (length(x) == 0) {
    return("")
  }

  x = vapply(x, formatExplanationTeachingVariable, character(1))

  if (length(x) == 1) {
    return(x)
  }

  if (length(x) == 2) {
    return(paste(x, collapse = " and "))
  }

  paste0(paste(x[-length(x)], collapse = ", "), ", and ", x[length(x)])
}

#' Translate audit scale labels into plainer student language
#'
#' @param x Scale label from the audit.
#'
#' @return A single character string.
#' @keywords internal
translateExplanationTeachingScaleLabel = function(x) {

  x = trimws(x %||% "")

  if (!nzchar(x)) {
    return("a student-friendly set of outcome statements")
  }

  lookup = c(
    "response scale" = "the original outcome units",
    "probability and odds" = "chance of the outcome happening, with odds used only when needed",
    "expected count" = "predicted counts",
    "odds multipliers" = "changes in the odds",
    "expected-count multipliers" = "multiplicative changes in the predicted count",
    "additive response-scale differences" = "changes in the original outcome units"
  )

  if (x %in% names(lookup)) {
    return(unname(lookup[[x]]))
  }

  if (grepl("^additive differences on the ", x)) {
    return(sub("^additive differences on the ", "changes on the ", x))
  }

  x
}

#' Simplify the audit note about the explanation scale
#'
#' @param x Explanation-scale note from the audit.
#'
#' @return A single character string.
#' @keywords internal
translateExplanationTeachingScaleNote = function(x) {

  x = trimws(x %||% "")

  if (!nzchar(x)) {
    return("")
  }

  lookup = c(
    "The narrative is instructed to describe effects on the odds scale rather than as raw log-odds coefficients." = "So the app talks about changes in odds rather than leaving the result on the harder log-odds scale.",
    "The narrative is instructed to describe multiplicative changes in the expected count rather than raw log coefficients." = "So the app explains how the predicted count is multiplied, rather than leaving the result on the harder log scale.",
    "The narrative is instructed to describe additive changes on the response scale." = "So the app keeps the wording in the original outcome units.",
    "The narrative is instructed to stay on the detected transformed scale rather than inventing an unsupported back-transformation." = "So the app stays on the transformed outcome scale used in the model instead of pretending it can safely convert everything back."
  )

  if (x %in% names(lookup)) {
    return(unname(lookup[[x]]))
  }

  x
}

#' Make a sentence start from a lower-case audit reason
#'
#' @param x Reason text from the audit.
#'
#' @return A single character string.
#' @keywords internal
sentenceCaseExplanationTeachingReason = function(x) {

  x = trimws(x %||% "")

  if (!nzchar(x)) {
    return("")
  }

  paste0(toupper(substr(x, 1, 1)), substring(x, 2))
}

#' Build the interpretation-scale teaching text
#'
#' @param audit A `wmfmExplanationAudit` object.
#' @param responseName Name of the response variable.
#'
#' @return A single character string.
#' @keywords internal
buildExplanationTeachingInterpretationScale = function(audit, responseName) {

  fittedScale = translateExplanationTeachingScaleLabel(audit$interpretationScale$fittedValueScale)
  effectScale = translateExplanationTeachingScaleLabel(audit$interpretationScale$effectScale)
  backTransformation = trimws(audit$interpretationScale$backTransformation %||% "")
  scaleNote = translateExplanationTeachingScaleNote(audit$interpretationScale$explanationScaleNote)
  responseLabel = formatExplanationTeachingVariable(responseName)

  parts = c(
    paste0(
      "The app explains ",
      responseLabel,
      " in a way that stays close to what students would naturally talk about in class. ",
      "Here that means using ",
      fittedScale,
      " for fitted values and ",
      effectScale,
      " for describing change."
    )
  )

  if (nzchar(backTransformation) && !identical(backTransformation, "No back-transformation is required.")) {
    parts = c(
      parts,
      paste0(
        "Some calculations happen on a less direct model scale first, then are translated into the more readable form used in the explanation. ",
        backTransformation
      )
    )
  }

  if (nzchar(scaleNote)) {
    parts = c(parts, scaleNote)
  }

  paste(parts, collapse = " ")
}

#' Build the baseline-choice teaching text
#'
#' @param audit A `wmfmExplanationAudit` object.
#' @param numericPredictors Character vector of numeric predictor names.
#' @param factorPredictors Character vector of factor predictor names.
#'
#' @return A single character string.
#' @keywords internal
buildExplanationTeachingBaselineChoice = function(audit, numericPredictors, factorPredictors) {

  parts = character(0)

  if (length(numericPredictors) > 0 && is.data.frame(audit$numericAnchor$table) && nrow(audit$numericAnchor$table) > 0) {
    numericText = apply(audit$numericAnchor$table, 1, function(row) {
      predictorLabel = formatExplanationTeachingVariable(as.character(row[["predictor"]]))
      reasonText = sentenceCaseExplanationTeachingReason(as.character(row[["reason"]]))

      paste0(
        "When the app needed a typical starting value for ",
        predictorLabel,
        ", it used ",
        as.character(row[["anchor"]]),
        ". ",
        reasonText
      )
    })

    parts = c(
      parts,
      paste(
        "For number-valued predictors, the app chose realistic starting values instead of automatically pretending every variable begins at 0.",
        paste(numericText, collapse = " ")
      )
    )
  }

  if (length(factorPredictors) > 0 && is.data.frame(audit$referenceLevels) && nrow(audit$referenceLevels) > 0) {
    factorText = apply(audit$referenceLevels, 1, function(row) {
      predictorLabel = formatExplanationTeachingVariable(as.character(row[["predictor"]]))

      paste0(
        "For the group variable ",
        predictorLabel,
        ", the app used ",
        sQuote(as.character(row[["referenceLevel"]])),
        " as the starting comparison group."
      )
    })

    parts = c(
      parts,
      paste(
        "For group predictors, the app picked one group to act as the starting comparison point.",
        paste(factorText, collapse = " ")
      )
    )
  }

  if (length(parts) == 0) {
    return("No special starting values were needed here, so the explanation could be written without extra comparison settings.")
  }

  paste(parts, collapse = " ")
}

#' Build the x-change teaching text
#'
#' @param responseName Name of the response variable.
#' @param numericPredictors Character vector of numeric predictor names.
#' @param factorPredictors Character vector of factor predictor names.
#' @param audit A `wmfmExplanationAudit` object.
#'
#' @return A single character string.
#' @keywords internal
buildExplanationTeachingXChangeDescription = function(responseName, numericPredictors, factorPredictors, audit) {

  parts = character(0)
  responseLabel = formatExplanationTeachingVariable(responseName)

  if (length(numericPredictors) > 0) {
    predictorText = collapseExplanationTeachingVariables(numericPredictors)

    parts = c(
      parts,
      paste0(
        "For number-valued predictors such as ",
        predictorText,
        ", the explanation reads the main effect as what happens to ",
        responseLabel,
        " when that predictor goes up by 1 unit, while the other predictors are held at their chosen starting values."
      )
    )
  }

  if (length(factorPredictors) > 0) {
    predictorText = collapseExplanationTeachingVariables(factorPredictors)

    parts = c(
      parts,
      paste0(
        "For group predictors such as ",
        predictorText,
        ", the explanation reads the effect as a group comparison, not as a 1-unit increase."
      )
    )
  }

  if (length(parts) == 0) {
    parts = c(
      parts,
      paste0(
        "The explanation focuses on how the fitted model changes ",
        responseLabel,
        ", but there were no special predictor changes that needed to be highlighted here."
      )
    )
  }

  scaleNote = translateExplanationTeachingScaleNote(audit$interpretationScale$explanationScaleNote)

  if (nzchar(scaleNote)) {
    parts = c(parts, scaleNote)
  }

  paste(parts, collapse = " ")
}

#' Build the main-effect teaching text
#'
#' @param audit A `wmfmExplanationAudit` object.
#' @param responseName Name of the response variable.
#' @param numericPredictors Character vector of numeric predictor names.
#' @param factorPredictors Character vector of factor predictor names.
#'
#' @return A single character string.
#' @keywords internal
buildExplanationTeachingMainEffectDescription = function(audit, responseName, numericPredictors, factorPredictors) {

  effectQuantities = character(0)

  if (is.data.frame(audit$effectEvidence) && nrow(audit$effectEvidence) > 0 && "quantity" %in% names(audit$effectEvidence)) {
    effectQuantities = unique(stats::na.omit(as.character(audit$effectEvidence$quantity)))
  }

  responseLabel = formatExplanationTeachingVariable(responseName)
  predictorText = collapseExplanationTeachingVariables(c(numericPredictors, factorPredictors))

  if (length(effectQuantities) > 0) {
    shownExamples = paste(utils::head(effectQuantities, 2), collapse = " and ")

    return(paste0(
      "The main result was not written by copying the coefficient table straight into words. ",
      "Instead, the app used calculated results from the fitted model, such as ",
      shownExamples,
      ", to show what the model says about ",
      responseLabel,
      ". ",
      if (nzchar(predictorText)) {
        paste0(
          "This helps students see the effect of ",
          predictorText,
          " in terms of predicted outcomes rather than table entries."
        )
      } else {
        "This keeps the focus on predicted outcomes rather than table entries."
      }
    ))
  }

  if (length(numericPredictors) > 0 || length(factorPredictors) > 0) {
    return(paste0(
      "The main result was built by translating the fitted model into plain language. ",
      "Instead of repeating technical table language, the app summarised what changes in the predictors mean for ",
      responseLabel,
      "."
    ))
  }

  paste0(
    "The main result was built from the fitted model in plain language so the explanation stays focused on what happens to ",
    responseLabel,
    " rather than on technical model output."
  )
}

#' Build the uncertainty teaching text
#'
#' @param audit A `wmfmExplanationAudit` object.
#' @param responseName Name of the response variable.
#'
#' @return A single character string.
#' @keywords internal
buildExplanationTeachingUncertaintySummary = function(audit, responseName) {

  confidenceLevel = round((audit$confidenceIntervals$level %||% 0.95) * 100)
  displayedScales = audit$confidenceIntervals$displayedScales %||% character(0)
  displayedScales = displayedScales[!is.na(displayedScales) & nzchar(displayedScales)]
  responseLabel = formatExplanationTeachingVariable(responseName)

  summaryText = paste0(
    "The app showed uncertainty using ",
    confidenceLevel,
    "% confidence intervals. ",
    "These intervals help students see a range of values that are still reasonably consistent with the fitted model, rather than treating the model as exact. ",
    "So statements about ",
    responseLabel,
    " can stay careful about both direction and size."
  )

  if (length(displayedScales) > 0) {
    summaryText = paste0(
      summaryText,
      " Where possible, those ranges were shown in the same easy-to-read form used in the explanation: ",
      paste(vapply(displayedScales, translateExplanationTeachingScaleLabel, character(1)), collapse = ", "),
      "."
    )
  }

  teachingNote = trimws(audit$confidenceIntervals$teachingNote %||% audit$confidenceIntervals$note %||% "")

  if (nzchar(teachingNote)) {
    summaryText = paste(summaryText, teachingNote)
  }

  summaryText
}

#' Build the teaching evidence table
#'
#' @param audit A `wmfmExplanationAudit` object.
#' @param responseName Name of the response variable.
#' @param numericPredictors Character vector of numeric predictor names.
#' @param factorPredictors Character vector of factor predictor names.
#' @param researchQuestion Optional research question string.
#'
#' @return A data frame.
#' @keywords internal
buildExplanationTeachingEvidenceTable = function(
    audit,
    responseName,
    numericPredictors,
    factorPredictors,
    researchQuestion
) {

  rows = list(
    data.frame(
      section = "Outcome wording",
      summary = paste0(
        "The app explained ",
        formatExplanationTeachingVariable(responseName),
        " using ",
        translateExplanationTeachingScaleLabel(audit$interpretationScale$fittedValueScale),
        " so the wording stays readable for students."
      ),
      stringsAsFactors = FALSE
    ),
    data.frame(
      section = "Starting values",
      summary = paste0(
        "When a typical starting value was needed, the app used realistic settings for ",
        if (length(numericPredictors) > 0) collapseExplanationTeachingVariables(numericPredictors) else "number-valued predictors",
        " and starting comparison groups for ",
        if (length(factorPredictors) > 0) collapseExplanationTeachingVariables(factorPredictors) else "group predictors",
        "."
      ),
      stringsAsFactors = FALSE
    ),
    data.frame(
      section = "Type of change",
      summary = paste(
        "Number-valued predictors were read as 1-unit increases, while group predictors were read as comparisons against a chosen group.",
        "That lets the app explain what changes in the model mean in everyday terms."
      ),
      stringsAsFactors = FALSE
    ),
    data.frame(
      section = "Range of likely values",
      summary = paste0(
        "The explanation used ",
        round((audit$confidenceIntervals$level %||% 0.95) * 100),
        "% confidence intervals so the wording stays cautious about how much uncertainty remains."
      ),
      stringsAsFactors = FALSE
    )
  )

  if (nzchar(researchQuestion)) {
    rows[[length(rows) + 1]] = data.frame(
      section = "Research question",
      summary = "The explanation was tied back to the supplied research question so the model output answers the question the student cares about, not just the regression table.",
      stringsAsFactors = FALSE
    )
  }

  do.call(rbind, rows)
}

#' Build the research-question teaching text
#'
#' @param researchQuestion Optional research question string.
#' @param responseName Name of the response variable.
#' @param predictorNames Character vector of predictor names.
#'
#' @return A single character string.
#' @keywords internal
buildExplanationTeachingResearchQuestionLink = function(researchQuestion, responseName, predictorNames) {

  responseLabel = formatExplanationTeachingVariable(responseName)
  predictorText = collapseExplanationTeachingVariables(predictorNames)

  if (!nzchar(trimws(researchQuestion))) {
    return(paste0(
      "No separate research question was supplied, so the explanation stays focused on what the predictors tell us about ",
      responseLabel,
      ". ",
      if (nzchar(predictorText)) {
        paste0("In practice, that means linking changes in ", predictorText, " back to the outcome.")
      } else {
        ""
      }
    ))
  }

  paste(
    "The explanation is linked back to the research question so the model output answers a teaching question, not just a statistical one.",
    "Here the model explanation should help the student respond to:",
    researchQuestion
  )
}
