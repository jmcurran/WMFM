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
  responseNounPhrase = attr(model, "wmfm_response_noun_phrase", exact = TRUE) %||% responseName
  outcomeLabel = buildExplanationTeachingOutcomeLabel(responseNounPhrase = responseNounPhrase)

  dataDescription = buildExplanationTeachingDataDescription(
    model = model,
    responseName = responseName,
    numericPredictors = numericPredictors,
    factorPredictors = factorPredictors,
    outcomeLabel = outcomeLabel
  )

  out = list(
    dataDescription = dataDescription,
    interpretationScale = buildExplanationTeachingInterpretationScale(
      audit = audit,
      outcomeLabel = outcomeLabel
    ),
    baselineChoice = buildExplanationTeachingBaselineChoice(
      audit = audit,
      numericPredictors = numericPredictors,
      factorPredictors = factorPredictors
    ),
    xChangeDescription = buildExplanationTeachingXChangeDescription(
      numericPredictors = numericPredictors,
      factorPredictors = factorPredictors,
      outcomeLabel = outcomeLabel
    ),
    mainEffectDescription = buildExplanationTeachingMainEffectDescription(
      audit = audit,
      responseName = responseName,
      outcomeLabel = outcomeLabel,
      numericPredictors = numericPredictors,
      factorPredictors = factorPredictors
    ),
    uncertaintySummary = buildExplanationTeachingUncertaintySummary(
      audit = audit,
      outcomeLabel = outcomeLabel
    ),
    evidenceTable = buildExplanationTeachingEvidenceTable(
      audit = audit,
      responseName = responseName,
      outcomeLabel = outcomeLabel,
      numericPredictors = numericPredictors,
      factorPredictors = factorPredictors,
      researchQuestion = researchQuestion,
      dataDescription = dataDescription
    ),
    researchQuestionLink = buildExplanationTeachingResearchQuestionLink(
      researchQuestion = researchQuestion,
      responseName = responseName,
      outcomeLabel = outcomeLabel
    )
  )

  class(out) = c("wmfmExplanationTeachingSummary", class(out))
  out
}

#' Build a plain-language description of the data used
#'
#' @param model A fitted model object.
#' @param responseName Name of the response variable.
#' @param numericPredictors Character vector of numeric predictor names.
#' @param factorPredictors Character vector of factor predictor names.
#' @param outcomeLabel Student-friendly outcome label.
#'
#' @return A single character string.
#' @keywords internal
buildExplanationTeachingDataDescription = function(
    model,
    responseName,
    numericPredictors,
    factorPredictors,
    outcomeLabel
) {

  responseText = paste0(
    "The response variable is `",
    responseName,
    "`, so the model is trying to explain ",
    outcomeLabel,
    "."
  )

  predictorParts = character(0)

  if (length(numericPredictors) > 0) {
    predictorParts = c(
      predictorParts,
      paste0(
        "Number-valued predictors: ",
        collapseTeachingNames(backtickNames(numericPredictors)),
        "."
      )
    )
  }

  if (length(factorPredictors) > 0) {
    predictorParts = c(
      predictorParts,
      paste0(
        "Group-based predictors: ",
        collapseTeachingNames(backtickNames(factorPredictors)),
        "."
      )
    )
  }

  if (length(predictorParts) == 0) {
    predictorText = "No separate predictors were included, so the fitted model mainly describes the overall pattern in the response."
  } else {
    predictorText = paste(
      "The explanation uses these predictors to orient the student before discussing the fitted results.",
      paste(predictorParts, collapse = " ")
    )
  }

  paste(responseText, predictorText)
}

#' Build a student-friendly outcome label
#'
#' @param responseNounPhrase Character scalar describing the response.
#'
#' @return A single character string.
#' @keywords internal
buildExplanationTeachingOutcomeLabel = function(responseNounPhrase) {

  phrase = trimws(as.character(responseNounPhrase %||% ""))
  phraseLower = tolower(phrase)

  if (!nzchar(phrase)) {
    return("the outcome")
  }

  if (grepl("\\bmark(s)?\\b", phraseLower)) {
    return("marks")
  }

  if (grepl("\\bscore(s)?\\b", phraseLower)) {
    return("scores")
  }

  phrase
}

#' Format a teaching number for inline prose
#'
#' @param x A numeric-like value.
#' @param digits Number of decimal places to show when needed.
#'
#' @return A single character string.
#' @keywords internal
buildExplanationTeachingNumber = function(x, digits = 2) {

  value = suppressWarnings(as.numeric(x))[1]

  if (is.na(value)) {
    return(as.character(x)[1] %||% "")
  }

  roundedValue = round(value, digits)

  if (isTRUE(all.equal(roundedValue, round(roundedValue)))) {
    return(as.character(as.integer(round(roundedValue))))
  }

  out = formatC(roundedValue, format = "f", digits = digits)
  out = sub("0+$", "", out)
  out = sub("\\.$", "", out)
  out
}

#' Convert sentence-opening digits into words when simple
#'
#' @param text A single character string.
#'
#' @return A single character string.
#' @keywords internal
spellOutSentenceStartNumber = function(text) {

  text = as.character(text %||% "")

  replacements = c(
    "0" = "Zero",
    "1" = "One",
    "2" = "Two",
    "3" = "Three",
    "4" = "Four",
    "5" = "Five",
    "6" = "Six",
    "7" = "Seven",
    "8" = "Eight",
    "9" = "Nine",
    "10" = "Ten"
  )

  for (digit in names(replacements)) {
    pattern = paste0("^", digit, "\\b")
    if (grepl(pattern, text)) {
      return(sub(pattern, replacements[[digit]], text))
    }
  }

  text
}

#' Wrap variable names in backticks for UI display
#'
#' @param x Character vector.
#'
#' @return Character vector.
#' @keywords internal
backtickNames = function(x) {
  paste0("`", x, "`")
}

#' Build a natural language list from names
#'
#' @param x Character vector.
#'
#' @return A single character string.
#' @keywords internal
collapseTeachingNames = function(x) {

  x = as.character(x)
  x = x[!is.na(x) & nzchar(x)]

  if (length(x) == 0) {
    return("")
  }

  if (length(x) == 1) {
    return(x)
  }

  if (length(x) == 2) {
    return(paste(x, collapse = " and "))
  }

  paste0(paste(x[-length(x)], collapse = ", "), ", and ", x[length(x)])
}

#' Build the interpretation teaching text
#'
#' @param audit A `wmfmExplanationAudit` object.
#' @param outcomeLabel Student-friendly outcome label.
#'
#' @return A single character string.
#' @keywords internal
buildExplanationTeachingInterpretationScale = function(audit, outcomeLabel) {

  effectScale = tolower(audit$interpretationScale$effectScale %||% "")
  backTransformation = audit$interpretationScale$backTransformation %||% ""

  if (grepl("odds", effectScale)) {
    text = paste(
      "The explanation was written in language students can read more easily.",
      "Instead of leaving results on the raw coefficient scale, the app described changes using odds so the model could be discussed in more familiar terms."
    )
  } else if (grepl("multiplicative", effectScale) || grepl("multiplier", effectScale)) {
    text = paste(
      "The explanation was written in language students can read more easily.",
      "Instead of leaving results on the raw coefficient scale, the app described how the expected",
      outcomeLabel,
      "changes in multiplicative terms."
    )
  } else {
    text = paste(
      "The explanation was written in language students can read more easily.",
      "Instead of leaving results on the raw coefficient scale, the app described how the expected",
      outcomeLabel,
      "changes in the original outcome wording."
    )
  }

  if (nzchar(backTransformation) && !identical(backTransformation, "No back-transformation is required.")) {
    text = paste(
      text,
      "Where needed, the app also converted fitted values back into a form that matches the explanation students see."
    )
  }

  text
}

#' Build the starting-values teaching text
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
    numericText = vapply(seq_len(nrow(audit$numericAnchor$table)), function(i) {
      row = audit$numericAnchor$table[i, , drop = FALSE]
      predictor = paste0("`", row$predictor[[1]], "`")
      anchorValue = buildExplanationTeachingNumber(row$anchor[[1]])
      reasonText = trimws(as.character(row$reason[[1]] %||% ""))

      if (grepl("outside the observed range", reasonText, ignore.case = TRUE)) {
        paste(
          "When the app needed a typical starting value for",
          predictor,
          ", it used",
          anchorValue,
          ".",
          spellOutSentenceStartNumber("0 lies outside the observed range, so the sample mean was used.")
        )
      } else {
        paste(
          "When the app needed a typical starting value for",
          predictor,
          ", it used",
          anchorValue,
          ".",
          if (nzchar(reasonText)) reasonText else "This gave the explanation a realistic place to start."
        )
      }
    }, character(1))

    parts = c(
      parts,
      paste(
        "For number-valued predictors, the app chose realistic starting values instead of automatically pretending every variable begins at zero.",
        paste(numericText, collapse = " ")
      )
    )
  }

  if (length(factorPredictors) > 0 && is.data.frame(audit$referenceLevels) && nrow(audit$referenceLevels) > 0) {
    factorText = vapply(seq_len(nrow(audit$referenceLevels)), function(i) {
      row = audit$referenceLevels[i, , drop = FALSE]
      paste0(
        "For the group variable `",
        row$predictor[[1]],
        "`, the app used '",
        row$referenceLevel[[1]],
        "' as the starting comparison group."
      )
    }, character(1))

    parts = c(
      parts,
      paste(
        "For group predictors, the app picked one group to act as the starting comparison point.",
        paste(factorText, collapse = " ")
      )
    )
  }

  if (length(parts) == 0) {
    return("No special starting values were needed because there were no number-valued or group predictors that required them.")
  }

  paste(parts, collapse = " ")
}

#' Build the x-change teaching text
#'
#' @param numericPredictors Character vector of numeric predictor names.
#' @param factorPredictors Character vector of factor predictor names.
#' @param outcomeLabel Student-friendly outcome label.
#'
#' @return A single character string.
#' @keywords internal
buildExplanationTeachingXChangeDescription = function(numericPredictors, factorPredictors, outcomeLabel) {

  parts = character(0)

  if (length(numericPredictors) > 0) {
    numericText = collapseTeachingNames(backtickNames(numericPredictors))
    parts = c(
      parts,
      paste0(
        "For number-valued predictors such as ",
        numericText,
        ", the explanation reads the main effect as what happens to the outcome when that predictor goes up by one unit, while the other predictors are held at their chosen starting values."
      )
    )
  }

  if (length(factorPredictors) > 0) {
    factorText = collapseTeachingNames(backtickNames(factorPredictors))
    parts = c(
      parts,
      paste0(
        "For group predictors such as ",
        factorText,
        ", the explanation reads the effect as a group comparison, not as a one-unit increase."
      )
    )
  }

  if (length(parts) == 0) {
    parts = c(parts, "This explanation did not need any special discussion of predictor changes.")
  }

  parts = c(
    parts,
    paste(
      "So the app keeps the wording in the original outcome wording, here",
      outcomeLabel,
      "."
    )
  )

  paste(parts, collapse = " ")
}

#' Build the main-effect teaching text
#'
#' @param audit A `wmfmExplanationAudit` object.
#' @param responseName Name of the response variable.
#' @param outcomeLabel Student-friendly outcome label.
#' @param numericPredictors Character vector of numeric predictor names.
#' @param factorPredictors Character vector of factor predictor names.
#'
#' @return A single character string.
#' @keywords internal
buildExplanationTeachingMainEffectDescription = function(audit, responseName, outcomeLabel, numericPredictors, factorPredictors) {

  effectQuantities = character(0)

  if (is.data.frame(audit$effectEvidence) && nrow(audit$effectEvidence) > 0 && "quantity" %in% names(audit$effectEvidence)) {
    effectQuantities = unique(stats::na.omit(as.character(audit$effectEvidence$quantity)))
  }

  if (length(effectQuantities) > 0) {
    return(paste(
      "The main effect explanation was built from model-based quantities, not from the raw regression table alone.",
      "That helped the app explain what changes in",
      outcomeLabel,
      "mean in practice."
    ))
  }

  if (length(numericPredictors) > 0 || length(factorPredictors) > 0) {
    return(paste(
      "The main effect explanation was built by translating the fitted model into plain language.",
      "Instead of repeating technical coefficient wording, the app focused on how the predictors relate to",
      outcomeLabel,
      "in a way students can read more easily."
    ))
  }

  paste(
    "The main effect explanation was built from the fitted model in plain language so that the explanation stays focused on what happens to",
    responseName,
    "rather than on technical coefficient details."
  )
}

#' Extract a teaching-summary confidence level percent
#'
#' @param audit A `wmfmExplanationAudit` object.
#' @param defaultLevel Default confidence level when audit metadata is missing or invalid.
#'
#' @return A single numeric confidence percent.
#' @keywords internal
getExplanationTeachingConfidenceLevelPercent = function(audit, defaultLevel = 0.95) {

  level = audit$confidenceIntervals$level %||% defaultLevel

  if (!is.numeric(level) || length(level) != 1 || is.na(level) || level <= 0 || level >= 1) {
    level = defaultLevel
  }

  round(level * 100)
}

#' Extract a teaching-summary confidence note
#'
#' @param audit A `wmfmExplanationAudit` object.
#'
#' @return A single character string.
#' @keywords internal
getExplanationTeachingConfidenceNote = function(audit) {

  note = audit$confidenceIntervals$teachingNote %||% audit$confidenceIntervals$note %||% ""
  note = trimws(as.character(note))

  if (!length(note) || is.na(note) || !nzchar(note) || identical(note, "NA") || identical(note, "coefficient.")) {
    return("")
  }

  note
}

#' Build the uncertainty teaching text
#'
#' @param audit A `wmfmExplanationAudit` object.
#' @param outcomeLabel Student-friendly outcome label.
#'
#' @return A single character string.
#' @keywords internal
buildExplanationTeachingUncertaintySummary = function(audit, outcomeLabel) {

  confidenceLevel = getExplanationTeachingConfidenceLevelPercent(audit = audit)

  summaryText = paste(
    "Uncertainty was handled using",
    confidenceLevel,
    "% confidence intervals.",
    "These were used to support careful statements about the likely direction and size of changes in",
    outcomeLabel,
    ", rather than presenting the model as exact."
  )

  teachingNote = getExplanationTeachingConfidenceNote(audit = audit)

  if (nzchar(teachingNote)) {
    summaryText = paste(summaryText, teachingNote)
  }

  summaryText
}

#' Build the teaching evidence list
#'
#' @param audit A `wmfmExplanationAudit` object.
#' @param responseName Name of the response variable.
#' @param outcomeLabel Student-friendly outcome label.
#' @param numericPredictors Character vector of numeric predictor names.
#' @param factorPredictors Character vector of factor predictor names.
#' @param researchQuestion Optional research question string.
#' @param dataDescription Plain-language data description.
#'
#' @return A data frame.
#' @keywords internal
buildExplanationTeachingEvidenceTable = function(
    audit,
    responseName,
    outcomeLabel,
    numericPredictors,
    factorPredictors,
    researchQuestion,
    dataDescription
) {

  rows = list()

  if (nzchar(researchQuestion)) {
    rows[[length(rows) + 1]] = data.frame(
      section = "Research question",
      summary = paste(
        "Start by reminding the student of the question the model is being used to answer:",
        researchQuestion
      ),
      stringsAsFactors = FALSE
    )
  }

  rows[[length(rows) + 1]] = data.frame(
    section = "Data used",
    summary = dataDescription,
    stringsAsFactors = FALSE
  )

  rows = c(
    rows,
    list(
      data.frame(
        section = "Scale used",
        summary = paste(
          "Explain the model in student-friendly language using the original outcome wording for",
          outcomeLabel,
          "."
        ),
        stringsAsFactors = FALSE
      ),
      data.frame(
        section = "Starting values",
        summary = paste(
          "Choose realistic starting values for number-valued predictors and starting comparison groups for group predictors."
        ),
        stringsAsFactors = FALSE
      ),
      data.frame(
        section = "Main comparison",
        summary = if (length(numericPredictors) > 0 || length(factorPredictors) > 0) {
          "Describe number-valued predictors as one-unit changes and group predictors as comparisons between groups."
        } else {
          paste("Use the fitted model to describe changes in", responseName, "in plain language.")
        },
        stringsAsFactors = FALSE
      ),
      data.frame(
        section = "Uncertainty",
        summary = paste0(
          "Use ",
          getExplanationTeachingConfidenceLevelPercent(audit = audit),
          "% confidence intervals to describe uncertainty carefully."
        ),
        stringsAsFactors = FALSE
      )
    )
  )

  do.call(rbind, rows)
}

#' Build the research-question teaching text
#'
#' @param researchQuestion Optional research question string.
#' @param responseName Name of the response variable.
#' @param outcomeLabel Student-friendly outcome label.
#'
#' @return A single character string.
#' @keywords internal
buildExplanationTeachingResearchQuestionLink = function(researchQuestion, responseName, outcomeLabel) {

  if (!nzchar(trimws(researchQuestion))) {
    return(paste(
      "No separate research question was supplied, so the explanation stays focused on how the predictors relate to",
      outcomeLabel,
      "."
    ))
  }

  paste(
    "The explanation is linked back to the research question so that the model output answers a teaching goal, not just a statistical one.",
    "Here, the explanation should help the student judge what the fitted model says about this question:",
    researchQuestion
  )
}
