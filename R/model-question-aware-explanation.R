
#' Determine whether a research question already has a complete specialised response
#'
#' @param model Fitted model object.
#'
#' @return Logical scalar.
#' @keywords internal
#' @noRd
isSpecialisedResearchQuestionResponse = function(model) {
  objective = attr(model, "wmfm_research_question_objective", exact = TRUE)
  if (!inherits(objective, "wmfmQuestionObjective")) {
    return(FALSE)
  }

  if (isTRUE(objective$requiresFollowup) ||
      objective$route %in% c(
        "needs_input",
        "needs_clarification",
        "alternative_analysis_needed",
        "out_of_scope"
      )) {
    return(TRUE)
  }

  identical(objective$route, "model_answer") &&
    objective$archetype %in% c(
      "individual_prediction",
      "expected_response",
      "compare_groups_or_profiles"
    )
}

#' Build a question-aware prompt contract
#'
#' @param model Fitted model object.
#'
#' @return Character scalar prompt block.
#' @keywords internal
#' @noRd
buildQuestionAwareExplanationPromptBlock = function(model) {
  objective = attr(model, "wmfm_research_question_objective", exact = TRUE)
  if (!inherits(objective, "wmfmQuestionObjective")) {
    return("")
  }

  if (identical(objective$archetype, "individual_prediction")) {
    prediction = objective$predictionPayload$predictionResult %||% list()
    if (!identical(prediction$status, "ok")) {
      missingText = paste(objective$unsupportedOrMissing %||% character(0), collapse = ", ")
      return(paste(
        "Question-aware explanation contract:",
        "Primary task: explain that the individual prediction cannot yet be made.",
        paste0("Missing information: ", missingText),
        "Do not silently substitute average or reference predictor values.",
        sep = "\n"
      ))
    }

    if (identical(prediction$modelType, "glm") && identical(prediction$responseDescription, "probability")) {
      return(paste(
        "Question-aware explanation contract:",
        "Primary task: answer the student's individual-outcome question with the fitted probability.",
        "WMFM will place its deterministic probability answer before the language-model explanation.",
        "A general model summary by itself does not answer this individual-outcome question.",
        paste0("Resolved predictor profile: ", formatFollowupPredictorSettings(prediction$resolvedPredictorValues)),
        paste0("Deterministic fitted probability: ", formatFollowupPredictionNumber(prediction$fittedPrediction)),
        "An individual future outcome is binary; do not invent or imply a continuous prediction interval.",
        "Do not convert the probability into a pass/fail or yes/no classification unless the question supplies an explicit threshold.",
        "Keep coefficient interpretation concise and secondary.",
        "Do not recompute, replace, contradict, or relabel the deterministic probability.",
        sep = "\n"
      ))
    }

    if (identical(prediction$modelType, "glm") && identical(prediction$responseDescription, "expected_count")) {
      futureText = if (is.list(prediction$predictionInterval)) {
        paste0(
          "Deterministic conditional 95% future-count interval: ",
          formatFollowupPredictionNumber(prediction$predictionInterval$lwr), " to ",
          formatFollowupPredictionNumber(prediction$predictionInterval$upr),
          ". This interval conditions on the fitted mean and does not include parameter uncertainty."
        )
      } else {
        "No future-count interval has been supplied; do not invent one."
      }
      return(paste(
        "Question-aware explanation contract:",
        "Primary task: answer the student's future-count question with the fitted expected count.",
        "WMFM will place its deterministic expected-count answer before the language-model explanation.",
        "A general model summary by itself does not answer this future-count question.",
        paste0("Resolved predictor profile: ", formatFollowupPredictorSettings(prediction$resolvedPredictorValues)),
        paste0("Deterministic expected count: ", formatFollowupPredictionNumber(prediction$fittedPrediction)),
        futureText,
        "An expected count is not the exact count that will occur.",
        "Keep coefficient interpretation concise and secondary.",
        "Do not recompute, replace, contradict, or relabel the deterministic quantities.",
        sep = "\n"
      ))
    }

    intervalText = if (is.list(prediction$predictionInterval)) {
      paste0(formatFollowupPredictionNumber(prediction$predictionInterval$lwr), " to ", formatFollowupPredictionNumber(prediction$predictionInterval$upr))
    } else {
      "not supplied"
    }
    return(paste(
      "Question-aware explanation contract:",
      "Primary task: answer the student's individual-prediction research question directly.",
      "WMFM will place its deterministic numerical answer before the language-model explanation.",
      "A general model summary by itself does not answer this individual-prediction question.",
      "Begin with practical uncertainty; keep coefficient interpretation concise and secondary.",
      paste0("Resolved predictor profile: ", formatFollowupPredictorSettings(prediction$resolvedPredictorValues)),
      paste0("Deterministic fitted prediction: ", formatFollowupPredictionNumber(prediction$fittedPrediction)),
      paste0("Deterministic 95% individual prediction interval: ", intervalText),
      "Do not recompute, replace, contradict, or relabel these quantities.",
      sep = "\n"
    ))
  }

  if (identical(objective$archetype, "expected_response")) {
    prediction = objective$answerPayload$predictionResult %||% list()
    if (!identical(prediction$status, "ok")) {
      return(paste(
        "Question-aware explanation contract:",
        "Primary task: explain what information is missing for the expected-response estimate.",
        "Do not substitute an average profile.",
        sep = "\n"
      ))
    }
    familyInstruction = if (identical(prediction$responseDescription, "probability")) {
      "Interpret this as a fitted probability. Do not present it as a guaranteed individual outcome or apply a classification threshold unless one is explicitly supplied."
    } else if (identical(prediction$responseDescription, "expected_count")) {
      "Interpret this as an expected count. Do not claim that it is the exact future count."
    } else {
      "Interpret this as an expected or average response."
    }
    quantityLabel = if (identical(prediction$responseDescription, "probability")) {
      "Deterministic fitted probability: "
    } else if (identical(prediction$responseDescription, "expected_count")) {
      "Deterministic expected count: "
    } else {
      "Deterministic expected response: "
    }
    return(paste(
      "Question-aware explanation contract:",
      "Primary task: answer the expected or average response question directly.",
      paste0("Resolved predictor profile: ", formatFollowupPredictorSettings(prediction$resolvedPredictorValues)),
      paste0(quantityLabel, formatFollowupPredictionNumber(prediction$fittedPrediction)),
      "Use the supplied confidence interval for the fitted mean, probability, or expected count when available.",
      familyInstruction,
      "Do not call this an individual prediction and do not describe its confidence interval as a prediction interval.",
      "Keep general coefficient interpretation secondary.",
      sep = "\n"
    ))
  }

  if (identical(objective$archetype, "compare_groups_or_profiles")) {
    comparison = objective$answerPayload %||% list()
    if (!identical(comparison$status, "ok")) {
      return(paste(
        "Question-aware explanation contract:",
        "Primary task: explain that two complete comparison profiles are required.",
        "Do not invent omitted predictor values.",
        sep = "\n"
      ))
    }
    familyInstruction = if (identical(comparison$responseDescription, "probability")) {
      "Compare fitted probabilities on the response scale; do not turn them into guaranteed individual outcomes."
    } else if (identical(comparison$responseDescription, "expected_count")) {
      "Compare expected counts on the response scale; do not describe either expected count as an exact future count."
    } else {
      "Compare fitted mean responses on the response scale."
    }
    return(paste(
      "Question-aware explanation contract:",
      "Primary task: compare the two expected responses directly.",
      paste0("First profile: ", formatFollowupPredictorSettings(comparison$leftProfile)),
      paste0("Second profile: ", formatFollowupPredictorSettings(comparison$rightProfile)),
      paste0("Deterministic second-minus-first difference: ", formatFollowupPredictionNumber(comparison$difference)),
      familyInstruction,
      "Interpret the supplied confidence interval as uncertainty in the expected-response difference.",
      "Do not relabel it as an individual prediction interval.",
      sep = "\n"
    ))
  }

  ""
}


#' Build a deterministic research-question clarification
#'
#' @param objective A `wmfmQuestionObjective` object.
#' @param model Fitted model object.
#'
#' @return Character scalar clarification or capability response.
#' @keywords internal
#' @noRd
buildDeterministicResearchQuestionClarification = function(objective, model) {
  route = attr(model, "wmfm_research_question_route", exact = TRUE)
  routeResponse = trimws(as.character(route$deterministicResponse %||% ""))

  missing = unique(trimws(as.character(objective$unsupportedOrMissing %||% character(0))))
  missing = missing[nzchar(missing)]
  predictorNames = names(stats::model.frame(model))[-1]
  missingPredictors = intersect(missing, predictorNames)

  if (length(missingPredictors) > 0L) {
    missing = missingPredictors
    missingText = if (length(missing) == 1L) {
      missing
    } else if (length(missing) == 2L) {
      paste(missing, collapse = " and ")
    } else {
      paste0(paste(missing[-length(missing)], collapse = ", "), ", and ", missing[[length(missing)]])
    }

    return(paste0(
      "I cannot calculate the requested result yet. Please provide ",
      missingText,
      ". WMFM will then use those values rather than silently substituting average or reference values."
    ))
  }

  if (nzchar(routeResponse)) {
    return(routeResponse)
  }

  "I need a more specific research question before I can determine what the fitted model should answer."
}

#' Prepend a deterministic research-question answer
#'
#' @param explanation Character scalar language-model explanation.
#' @param model Fitted model object.
#'
#' @return Character scalar with the verified research-question answer first.
#' @keywords internal
#' @noRd
prependDeterministicResearchQuestionAnswer = function(explanation, model) {
  objective = attr(model, "wmfm_research_question_objective", exact = TRUE)
  if (!inherits(objective, "wmfmQuestionObjective")) {
    return(explanation)
  }

  answer = ""
  if (objective$archetype %in% c("individual_prediction", "expected_response")) {
    payload = if (identical(objective$archetype, "expected_response")) objective$answerPayload else objective$predictionPayload
    prediction = payload$predictionResult %||% list()
    if (is.list(payload) && identical(prediction$status, "ok")) {
      modelCopy = model
      attr(modelCopy, "wmfm_model_followup_payload") = payload
      answer = buildDeterministicFollowupAnswer(model = modelCopy)
      threshold = suppressWarnings(as.numeric(objective$outcomeThreshold %||% NA_real_))
      if (length(threshold) == 1L && is.finite(threshold) &&
          is.numeric(prediction$fittedPrediction) && length(prediction$fittedPrediction) == 1L) {
        direction = if (prediction$fittedPrediction >= threshold) "above" else "below"
        answer = paste0(
          answer,
          " The fitted expected value is ", direction,
          " the supplied threshold of ", formatFollowupPredictionNumber(threshold),
          ". The individual prediction interval should be used to judge how uncertain an individual outcome remains."
        )
      }
    }
  } else if (identical(objective$archetype, "compare_groups_or_profiles")) {
    answer = buildDeterministicResearchQuestionComparisonAnswer(objective$answerPayload, model)
  }

  if (!nzchar(answer) && isTRUE(objective$requiresFollowup)) {
    clarification = buildDeterministicResearchQuestionClarification(
      objective = objective,
      model = model
    )
    if (nzchar(clarification)) {
      return(clarification)
    }
  }

  if (!nzchar(answer)) {
    return(explanation)
  }

  # A verified specialised answer is complete in its own right. Appending the
  # general model explanation makes clarification and prediction responses
  # repetitive and can distract from the requested result.
  answer
}
