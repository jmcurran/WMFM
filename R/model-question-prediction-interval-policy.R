#' Build GLM prediction-interval policy metadata
#'
#' Creates deterministic metadata describing whether a future-observation
#' prediction interval is currently supported for a GLM follow-up request. This
#' keeps confidence intervals for fitted means separate from future-observation
#' prediction intervals and gives developer diagnostics a stable contract before
#' future count intervals are implemented.
#'
#' @param familyName Character scalar GLM family name.
#' @param linkName Character scalar GLM link name.
#' @param requestedPredictionInterval Logical scalar indicating whether the user
#'   explicitly requested a prediction interval.
#'
#' @return Named list with stable prediction-interval policy metadata.
#' @keywords internal
#' @noRd
buildGlmPredictionIntervalPolicy = function(familyName, linkName, requestedPredictionInterval = FALSE) {
  familyName = tolower(as.character(familyName %||% ""))
  linkName = tolower(as.character(linkName %||% ""))
  requestedPredictionInterval = isTRUE(requestedPredictionInterval)

  if (identical(familyName, "poisson")) {
    return(list(
      supported = TRUE,
      requested = requestedPredictionInterval,
      glmFamily = familyName,
      glmLink = linkName,
      futureObservationType = "future_count",
      recommendedNextStage = "consider_parameter_uncertainty_for_poisson_future_count_interval",
      method = "conditional_poisson_quantile",
      parameterUncertaintyIncluded = FALSE,
      studentExplanation = paste(
        "WMFM can report a conditional prediction interval for a future Poisson count when the fitted expected count is available.",
        "This interval uses the Poisson distribution with the fitted expected count treated as fixed, so it does not include fitted-mean parameter uncertainty."
      ),
      developerExplanation = paste(
        "Poisson GLM future-observation intervals are implemented as discrete, non-negative count intervals.",
        "Stage 27.5 uses stats::qpois() with the fitted mean as the Poisson rate parameter.",
        "The method metadata explicitly states that fitted-mean parameter uncertainty is not included."
      )
    ))
  }

  if (identical(familyName, "binomial") && identical(linkName, "logit")) {
    return(list(
      supported = TRUE,
      requested = requestedPredictionInterval,
      glmFamily = familyName,
      glmLink = linkName,
      futureObservationType = "future_binary_outcome",
      recommendedNextStage = "consider_parameter_uncertainty_for_logistic_probability_interval",
      method = "bernoulli_outcome_framing",
      parameterUncertaintyIncluded = FALSE,
      studentExplanation = paste(
        "WMFM can report deterministic Bernoulli future-outcome framing for a logistic model.",
        "This is not a conventional continuous prediction interval: an individual future response is one of the outcome levels, with probabilities determined by the fitted probability."
      ),
      developerExplanation = paste(
        "Stage 27.6 represents logistic future-observation uncertainty as Bernoulli outcome probabilities rather than a continuous prediction interval.",
        "The fitted probability is treated as fixed, so fitted-mean parameter uncertainty is not included in the future-outcome framing."
      )
    ))
  }

  list(
    supported = FALSE,
    requested = requestedPredictionInterval,
    glmFamily = familyName,
    glmLink = linkName,
    futureObservationType = "unsupported_glm_family",
    recommendedNextStage = "none",
    method = NULL,
    parameterUncertaintyIncluded = FALSE,
    studentExplanation = "Future-observation prediction intervals are not currently supported for this GLM family.",
    developerExplanation = "No GLM prediction-interval implementation policy is currently defined for this family/link combination."
  )
}
