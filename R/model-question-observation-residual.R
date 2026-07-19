#' Classify existing-observation residual questions
#'
#' Identifies questions that ask WMFM to rank observations already used in an
#' ordinary linear model by how far their observed responses lie below, above,
#' or on either side of their fitted values.
#'
#' @param normalizedText Lower-case normalized question text.
#'
#' @return A named list containing `matched`, `direction`, and `reasonCode`.
#' @keywords internal
#' @noRd
classifyObservationResidualQuestion = function(normalizedText) {
  text = trimws(as.character(normalizedText %||% ""))

  result = list(
    matched = FALSE,
    direction = NULL,
    reasonCode = NULL
  )

  if (!nzchar(text)) {
    return(result)
  }

  selectionPattern = paste(
    c(
      "\\bwhich\\b",
      "\\bidentify\\b",
      "\\blist\\b",
      "\\bshow\\b",
      "\\brank\\b",
      "\\bfind\\b"
    ),
    collapse = "|"
  )
  observationPattern = paste(
    c(
      "\\b(observation|observations|case|cases|row|rows|student|students)\\b",
      "\\b(diamond|diamonds|house|houses|patient|patients|point|points|person|people|car|cars)\\b"
    ),
    collapse = "|"
  )
  modelRelativePattern = paste(
    c(
      "\\bresiduals?\\b",
      "\\bfitted (value|values)\\b",
      "\\b(model )?(expected|predicted)\\b",
      "\\bthan (the )?model (expects|expected|predicts|predicted)\\b",
      "\\brelative to (the |their )?(model|fitted values?)\\b"
    ),
    collapse = "|"
  )

  absolutePattern = paste(
    c(
      "\\b(largest|biggest|greatest|most extreme) (absolute )?residuals?\\b",
      "\\bfurthest from (their |the )?fitted values?\\b",
      "\\bfarthest from (their |the )?fitted values?\\b",
      "\\bmost unusual relative to (the )?model\\b",
      "\\beither direction\\b"
    ),
    collapse = "|"
  )
  lowerPattern = paste(
    c(
      "\\b(lowest|most negative) residuals?\\b",
      "\\b(furthest|farthest) below (their |the )?fitted values?\\b",
      "\\bbelow (their |the )?(fitted|expected|predicted) values?\\b",
      "\\blower than (the )?model (expects|expected|predicts|predicted)\\b",
      "\\b(cheaper|lower) than expected\\b",
      "\\b(cheap|cheaper|lower) relative to (the |their )?fitted values?\\b",
      "\\bperformed worse than expected\\b"
    ),
    collapse = "|"
  )
  higherPattern = paste(
    c(
      "\\b(highest|most positive) residuals?\\b",
      "\\b(furthest|farthest) above (their |the )?fitted values?\\b",
      "\\babove (their |the )?(fitted|expected|predicted) values?\\b",
      "\\bhigher than (the )?model (expects|expected|predicts|predicted)\\b",
      "\\b(more expensive|higher) than expected\\b",
      "\\bperformed better than expected\\b"
    ),
    collapse = "|"
  )

  hasSelection = grepl(selectionPattern, text, perl = TRUE)
  hasObservation = grepl(observationPattern, text, perl = TRUE)
  hasModelRelativeLanguage = grepl(modelRelativePattern, text, perl = TRUE)

  if (!(hasSelection && hasObservation && hasModelRelativeLanguage)) {
    return(result)
  }

  direction = NULL
  if (grepl(absolutePattern, text, perl = TRUE)) {
    direction = "absolute"
  } else if (grepl(lowerPattern, text, perl = TRUE)) {
    direction = "lower"
  } else if (grepl(higherPattern, text, perl = TRUE)) {
    direction = "higher"
  }

  if (is.null(direction)) {
    return(result)
  }

  result$matched = TRUE
  result$direction = direction
  result$reasonCode = "existing_observation_residual_ranking"
  result
}

#' Add deterministic residual inspection to a follow-up payload
#'
#' @param model Fitted model object.
#' @param followupPayload List returned by \code{classifyModelFollowupQuestion()}.
#' @param observationCount Maximum number of ranked observations to retain.
#'
#' @return Updated payload list. Adds \code{observationResidualResult} for
#'   existing-observation residual requests.
#' @keywords internal
#' @noRd
enrichFollowupPayloadWithObservationResiduals = function(model, followupPayload, observationCount = 5L) {
  payload = followupPayload
  if (!is.list(payload) || !identical(payload$category, "observation_residual_request")) {
    return(payload)
  }

  payload$observationResidualResult = computeObservationResidualResult(
    model = model,
    direction = payload$observationDirection %||% "absolute",
    observationCount = observationCount
  )
  payload$requiresDeterministicComputation = TRUE
  payload
}

#' @keywords internal
#' @noRd
computeObservationResidualResult = function(model, direction = "absolute", observationCount = 5L) {
  if (!inherits(model, "lm") || inherits(model, "glm")) {
    return(list(
      status = "unsupported",
      reason = "ordinary_lm_required",
      modelType = class(model)[[1]] %||% "unknown",
      warnings = "Existing-observation residual inspection currently supports ordinary linear models only."
    ))
  }

  direction = match.arg(direction, c("lower", "higher", "absolute"))
  if (!length(observationCount)) {
    observationCount = 5L
  }
  observationCount = suppressWarnings(as.integer(observationCount[[1]]))
  if (is.na(observationCount) || observationCount < 1L) {
    observationCount = 5L
  }

  modelFrame = stats::model.frame(model)
  observed = stats::model.response(modelFrame)
  fittedValues = stats::fitted(model)
  residualValues = observed - fittedValues

  if (!is.numeric(observed) || !length(residualValues)) {
    return(list(
      status = "unsupported",
      reason = "numeric_response_required",
      modelType = "lm",
      warnings = "Residual inspection requires a fitted ordinary linear model with a numeric response."
    ))
  }

  rowLabels = row.names(modelFrame)
  sourceRows = suppressWarnings(as.integer(rowLabels))
  if (length(sourceRows) != length(residualValues) || anyNA(sourceRows)) {
    sourceRows = seq_along(residualValues)
  }

  sequentialLabels = identical(rowLabels, as.character(seq_along(rowLabels)))
  meaningfulRowNames = length(unique(rowLabels)) == length(rowLabels) && !sequentialLabels
  observationIds = if (meaningfulRowNames) {
    rowLabels
  } else {
    paste("Row", sourceRows)
  }

  rankingValues = if (identical(direction, "absolute")) {
    abs(residualValues)
  } else {
    residualValues
  }
  rankingPercentiles = 100 * (rank(rankingValues, ties.method = "average") - 0.5) / length(rankingValues)

  orderIndex = switch(
    direction,
    lower = order(residualValues, sourceRows, na.last = NA),
    higher = order(-residualValues, sourceRows, na.last = NA),
    absolute = order(-abs(residualValues), sourceRows, na.last = NA)
  )
  keep = head(orderIndex, min(observationCount, length(orderIndex)))

  residualNames = names(stats::residuals(model))[keep]
  if (is.null(residualNames) || length(residualNames) != length(keep)) {
    residualNames = rowLabels[keep]
  }

  observationColumns = list(
    rank = seq_along(keep),
    observation = observationIds[keep],
    row = sourceRows[keep],
    observed = as.numeric(observed[keep]),
    fitted = as.numeric(fittedValues[keep]),
    residual = stats::setNames(as.numeric(residualValues[keep]), residualNames),
    percentile = as.numeric(rankingPercentiles[keep])
  )
  observations = structure(
    observationColumns,
    class = "data.frame",
    row.names = .set_row_names(length(keep))
  )

  interpretation = switch(
    direction,
    lower = "observations with the most negative raw residuals, meaning observed responses furthest below their fitted values",
    higher = "observations with the most positive raw residuals, meaning observed responses furthest above their fitted values",
    absolute = "observations with the largest absolute raw residuals, meaning observed responses furthest from their fitted values in either direction"
  )

  list(
    status = "ok",
    reason = "existing_observation_residual_ranking",
    modelType = "lm",
    responseName = names(modelFrame)[[1]],
    responseScale = "fitted_model_response_scale",
    rankingMetric = if (identical(direction, "absolute")) "absolute_raw_residual" else "raw_residual",
    direction = direction,
    interpretation = interpretation,
    observationCount = nrow(observations),
    totalFittedObservations = length(residualValues),
    observations = observations,
    limitations = c(
      "Residual ranking describes observations already used to fit this model.",
      "It does not by itself establish that an observation is a bargain, anomaly, data error, or causal effect.",
      "Raw residuals are reported on the fitted model response scale."
    )
  )
}

#' @keywords internal
#' @noRd
formatObservationResidualRows = function(observations) {
  if (!is.data.frame(observations) || !nrow(observations)) {
    return("[no ranked observations available]")
  }

  apply(observations, 1L, function(row) {
    paste0(
      "Rank ", row[["rank"]],
      ": ", row[["observation"]],
      "; source row ", row[["row"]],
      "; observed=", signif(as.numeric(row[["observed"]]), 6),
      "; fitted=", signif(as.numeric(row[["fitted"]]), 6),
      "; residual=", signif(as.numeric(row[["residual"]]), 6),
      "; ranking percentile=", signif(as.numeric(row[["percentile"]]), 4), "%"
    )
  }) |>
    paste(collapse = "\n")
}
