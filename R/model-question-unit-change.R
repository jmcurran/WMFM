#' Compute deterministic alternative-unit follow-up payload
#'
#' @param model Fitted model object.
#' @param followupPayload List returned by \code{classifyModelFollowupQuestion()}.
#'
#' @return Updated payload list with \code{unitChangeResult} for supported
#'   deterministic alternative-unit interpretation requests.
#' @keywords internal
#' @noRd
enrichFollowupPayloadWithUnitChange = function(model, followupPayload) {
  payload = followupPayload
  if (!is.list(payload) || !identical(payload$category, "alternative_unit_change")) {
    return(payload)
  }

  payload$unitChangeResult = computeDeterministicUnitChange(model = model, followupQuestion = payload$originalText %||% "")
  payload
}

computeDeterministicUnitChange = function(model, followupQuestion) {
  if (!inherits(model, "lm") || inherits(model, "glm")) {
    return(list(status = "unsupported", reason = "stage23.9_supports_only_ordinary_lm_unit_change"))
  }

  parsed = extractFollowupUnitChangeRequest(model = model, followupQuestion = followupQuestion)
  if (!isTRUE(parsed$ok)) {
    return(list(status = "unsupported", reason = parsed$reason %||% "ambiguous_or_unsupported_unit_change_request"))
  }

  cf = stats::coef(model)
  slope = unname(cf[[parsed$coefficientName]])
  transformed = as.numeric(slope) * parsed$unitChange

  ciOut = NULL
  ciMat = tryCatch(stats::confint(model), error = function(e) NULL)
  if (is.matrix(ciMat) && parsed$coefficientName %in% rownames(ciMat)) {
    ciOut = list(
      oneUnit = list(lwr = unname(ciMat[parsed$coefficientName, 1]), upr = unname(ciMat[parsed$coefficientName, 2])),
      transformed = list(
        lwr = unname(ciMat[parsed$coefficientName, 1]) * parsed$unitChange,
        upr = unname(ciMat[parsed$coefficientName, 2]) * parsed$unitChange
      )
    )
  }

  list(
    status = "ok",
    modelType = "lm",
    variable = parsed$variable,
    coefficientName = parsed$coefficientName,
    unitChange = parsed$unitChange,
    oneUnitEffect = as.numeric(slope),
    transformedEffect = transformed,
    confidenceInterval = ciOut
  )
}

extractFollowupUnitChangeRequest = function(model, followupQuestion) {
  txt = tolower(trimws(as.character(followupQuestion %||% "")))
  txt = gsub("\\s+", " ", txt, perl = TRUE)

  m = regexpr("\\b(\\d+(?:\\.\\d+)?)\\s*[- ]?unit\\b", txt, perl = TRUE)
  if (m[[1]] == -1L) {
    return(list(ok = FALSE, reason = "no_numeric_unit_change_detected"))
  }
  val = suppressWarnings(as.numeric(sub("\\b(\\d+(?:\\.\\d+)?)\\s*[- ]?unit\\b.*", "\\1", regmatches(txt, m), perl = TRUE)))
  if (!is.finite(val) || val == 0) {
    return(list(ok = FALSE, reason = "invalid_unit_change_value"))
  }

  coefNames = names(stats::coef(model))
  slopeNames = coefNames[coefNames != "(Intercept)"]
  if (!length(slopeNames)) {
    return(list(ok = FALSE, reason = "no_non_intercept_effects_available"))
  }

  clean = function(x) tolower(gsub("[^[:alnum:]_]", "", x, perl = TRUE))
  txtClean = clean(txt)
  matched = slopeNames[vapply(slopeNames, function(nm) nzchar(clean(nm)) && grepl(clean(nm), txtClean, fixed = TRUE), logical(1))]

  if (length(matched) != 1L) {
    return(list(ok = FALSE, reason = "ambiguous_or_missing_target_variable"))
  }

  list(ok = TRUE, unitChange = val, coefficientName = matched[[1]], variable = matched[[1]])
}
