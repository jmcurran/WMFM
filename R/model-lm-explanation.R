#' Get a plain-language explanation via the language model
#'
#' Calls the chat provider with a prompt constructed from a fitted model and
#' returns a narrative explanation of the model.
#'
#' By default, results are cached based on the model formula and coefficients
#' so that repeated requests for the same fitted model do not repeatedly query
#' the language model. This behaviour can be disabled by setting
#' `useCache = FALSE`, which is useful when deliberately testing variation
#' across repeated language model calls.
#'
#' @param model A fitted model object, typically of class \code{"lm"} or
#'   \code{"glm"}.
#' @param chat A chat provider object as returned by \code{getChatProvider()}.
#' @param useCache Logical. Should explanation results be cached and reused for
#'   identical fitted models? Defaults to `TRUE`.
#' @param diagnostics Optional environment populated with prompt and explanation
#'   text at each generation boundary. Intended for evaluation diagnostics.
#'
#' @return A character scalar containing the explanation text returned by the
#'   language model.
#' @keywords internal
#' @importFrom stats formula model.frame
lmExplanation = function(model, chat, useCache = TRUE, diagnostics = NULL) {

  if (!is.logical(useCache) || length(useCache) != 1 || is.na(useCache)) {
    stop("`useCache` must be TRUE or FALSE.", call. = FALSE)
  }

  if (!is.null(diagnostics) && !is.environment(diagnostics)) {
    stop("`diagnostics` must be an environment or NULL.", call. = FALSE)
  }

  recordDiagnostic = function(name, value) {
    if (!is.null(diagnostics)) {
      diagnostics[[name]] = value
    }
  }

  recordDiagnostic("cacheHit", FALSE)
  recordDiagnostic("llmCalled", FALSE)
  recordDiagnostic("promptText", NULL)
  recordDiagnostic("rawLlmText", NULL)
  recordDiagnostic("normalisedLlmText", NULL)
  recordDiagnostic("assembledExplanationText", NULL)

  followupPayload = attr(model, "wmfm_model_followup_payload", exact = TRUE)
  followupRoute = if (is.list(followupPayload)) {
    followupPayload$questionRoute %||% NULL
  } else {
    NULL
  }
  if (inherits(followupRoute, "wmfmQuestionRoute") &&
      !followupRoute$route %in% c("model_answer", "explanation_preference") &&
      nzchar(trimws(as.character(followupRoute$deterministicResponse %||% "")))) {
    output = trimws(as.character(followupRoute$deterministicResponse))
    recordDiagnostic("assembledExplanationText", output)
    return(output)
  }

  researchRoute = attr(model, "wmfm_research_question_route", exact = TRUE)
  researchObjective = attr(model, "wmfm_research_question_objective", exact = TRUE)
  objectiveAllowsModelAnswer = inherits(researchObjective, "wmfmQuestionObjective") &&
    researchObjective$route %in% c("model_answer", "explanation_preference")
  if (inherits(researchRoute, "wmfmQuestionRoute") &&
      !researchRoute$route %in% c("model_answer", "explanation_preference") &&
      !isTRUE(objectiveAllowsModelAnswer)) {
    output = if (inherits(researchObjective, "wmfmQuestionObjective")) {
      buildDeterministicResearchQuestionClarification(
        objective = researchObjective,
        model = model
      )
    } else {
      trimws(as.character(researchRoute$deterministicResponse %||% ""))
    }
    output = appendDeterministicFollowupAnswer(explanation = output, model = model)
    recordDiagnostic("assembledExplanationText", output)
    return(output)
  }

  formulaStr = paste(deparse(formula(model)), collapse = " ")
  coefStr = paste(coef(model), collapse = ";")
  mf = stats::model.frame(model)
  predictors = names(mf)[-1]
  numericAnchorInfo = buildModelNumericAnchorInfo(
    model = model,
    mf = mf,
    predictorNames = predictors
  )

  researchQuestion = attr(model, "wmfm_research_question", exact = TRUE) %||% ""
  followupQuestion = attr(model, "wmfm_model_followup_question", exact = TRUE) %||% ""
  key = buildLmExplanationCacheKey(
    formulaStr = formulaStr,
    coefStr = coefStr,
    numericAnchorCacheKey = numericAnchorInfo$cacheKey,
    researchQuestion = researchQuestion,
    followupQuestion = followupQuestion,
    adjustmentVariables = getModelAdjustmentVariables(model = model)
  )

  if (isTRUE(useCache) && !is.null(.env_cache[[key]])) {
    recordDiagnostic("cacheHit", TRUE)
    output = .env_cache[[key]]
    output = prependDeterministicResearchQuestionAnswer(
      explanation = output,
      model = model
    )
    output = appendDeterministicFollowupAnswer(
      explanation = output,
      model = model
    )
    recordDiagnostic("assembledExplanationText", output)
    return(output)
  }

  prompt = lmToExplanationPrompt(model)
  recordDiagnostic("promptText", prompt)
  recordDiagnostic("llmCalled", TRUE)

  rawOutput = chat$chat(prompt)
  recordDiagnostic("rawLlmText", rawOutput)

  output = normaliseNumericExpressions(rawOutput)
  recordDiagnostic("normalisedLlmText", output)

  output = prependDeterministicResearchQuestionAnswer(explanation = output, model = model)
  output = appendDeterministicFollowupAnswer(explanation = output, model = model)
  recordDiagnostic("assembledExplanationText", output)

  if (isTRUE(useCache)) {
    .env_cache[[key]] = output
  }

  output
}

#' Build cache key for plain-language explanation generation
#'
#' @param formulaStr Character scalar model formula text.
#' @param coefStr Character scalar coefficient payload text.
#' @param numericAnchorCacheKey Character scalar numeric-anchor cache token.
#' @param researchQuestion Character scalar research question.
#' @param followupQuestion Character scalar bounded follow-up model question.
#' @param adjustmentVariables Character vector adjustment variables.
#'
#' @return Character scalar cache key.
#' @keywords internal
buildLmExplanationCacheKey = function(
    formulaStr,
    coefStr,
    numericAnchorCacheKey,
    researchQuestion,
    followupQuestion = "",
    adjustmentVariables = character(0)) {

  adjustmentKey = paste(sort(unique(as.character(adjustmentVariables))), collapse = ",")
  policyVersion = "stage20.13-v1"

  paste(
    "expl",
    "v6-adjustment-policy-cache",
    policyVersion,
    formulaStr,
    coefStr,
    numericAnchorCacheKey,
    trimws(researchQuestion %||% ""),
    trimws(followupQuestion %||% ""),
    adjustmentKey
  )
}
