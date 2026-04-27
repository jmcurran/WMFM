#' Build response-scale control guidance for explanation prompts
#'
#' Builds deterministic prompt guidance from the Stage 9 model profile and rule
#' profile so the language model explains the fitted model on the intended
#' student-facing scale rather than on the raw coefficient or link-function
#' scale.
#'
#' @param model A fitted model object.
#' @param mf Optional model frame.
#'
#' @return A character scalar containing response-scale prompt guidance, or an
#'   empty string if the profile cannot be built.
#' @keywords internal
buildResponseScaleControlPromptBlock = function(model, mf = NULL) {

  if (is.null(mf)) {
    mf = tryCatch(
      stats::model.frame(model),
      error = function(e) {
        NULL
      }
    )
  }

  modelProfile = tryCatch(
    buildExplanationModelProfile(model = model, data = mf),
    error = function(e) {
      NULL
    }
  )

  if (is.null(modelProfile)) {
    return("")
  }

  ruleProfile = tryCatch(
    buildExplanationRuleProfile(modelProfile = modelProfile),
    error = function(e) {
      NULL
    }
  )

  if (is.null(ruleProfile)) {
    return("")
  }

  modelFamily = modelProfile$modelFamily %||% "unknown"
  modelScale = modelProfile$modelScale %||% "unknown"
  interpretationScale = modelProfile$interpretationScale %||% "response"
  transformationType = modelProfile$transformationType %||% "none"

  lines = c(
    "Response-scale control:",
    paste0("- Model scale: ", modelScale, "."),
    paste0("- Student-facing interpretation scale: ", interpretationScale, "."),
    paste0("- Response transformation type: ", transformationType, "."),
    paste0("- ", ruleProfile$scaleGuidance),
    "- Use the formatted model quantities above whenever they are available.",
    "- Do not describe raw coefficients as the substantive effects.",
    "- Do not ask the reader to mentally convert from the coefficient or link-function scale."
  )

  lines = c(lines, buildResponseScaleFamilyRules(modelFamily))
  lines = c(lines, buildResponseScaleTransformationRules(transformationType))

  avoidTerms = ruleProfile$avoidTerms

  if (length(avoidTerms) > 0) {
    lines = c(
      lines,
      paste0("- Avoid technical scale terms in the student-facing explanation: ", paste(avoidTerms, collapse = ", "), ".")
    )
  }

  paste(lines, collapse = "\n")
}

buildResponseScaleFamilyRules = function(modelFamily) {

  if (identical(modelFamily, "logistic")) {
    return(c(
      "- For fitted values, use probability language rather than log-odds language.",
      "- For numeric effects, use an odds multiplier only to describe the multiplicative change in odds for the stated change in the predictor.",
      "- For factor comparisons, use the supplied odds ratio for the direct comparison rather than separately interpreting each group's raw odds.",
      "- Do not reason from whether a single group's odds interval contains 1:1; use the direct odds-ratio interval when discussing group differences.",
      "- Do not use overlap or non-overlap of separate fitted-value intervals as evidence for a group difference.",
      "- Do not write log-odds, logit, or coefficient-scale interpretations as the main explanation."
    ))
  }

  if (identical(modelFamily, "poisson")) {
    return(c(
      "- For fitted values, use expected-count language rather than log-count language.",
      "- For effects, use multiplicative expected-count language when the formatted quantity is a multiplier.",
      "- Do not write log expected count, log count, or coefficient-scale interpretations as the main explanation."
    ))
  }

  character(0)
}

buildResponseScaleTransformationRules = function(transformationType) {

  if (identical(transformationType, "none")) {
    return(character(0))
  }

  c(
    "- The response was modelled after a transformation; treat inline transformations and derived transformed response variables consistently.",
    "- When formatted original-response quantities are available, use those quantities directly.",
    "- When only transformed-scale quantities are available, explicitly say that the statement is on the transformed response scale.",
    "- Do not leave transformation notation such as log(y), log.y, or sqrt.y as the explanation of the effect."
  )
}
