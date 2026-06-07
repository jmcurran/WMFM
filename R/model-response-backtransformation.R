#' Build deterministic response back-transformation metadata
#'
#' Uses recorded derived-variable metadata to construct conservative prompt
#' payloads for models where the response variable itself was user-created from
#' another variable. The helper only reports quantities WMFM can compute
#' deterministically; it does not ask the language model to infer an inverse.
#'
#' @param model A fitted model object.
#' @param mf Optional model frame.
#' @param predictorNames Optional predictor names.
#'
#' @return A list describing availability and any original-response-scale rows.
#' @keywords internal
buildResponseBackTransformationPayload = function(
  model,
  mf = NULL,
  predictorNames = NULL
) {
  if (is.null(mf)) {
    mf = stats::model.frame(model)
  }

  if (is.null(predictorNames)) {
    predictorNames = names(mf)[-1]
  }

  mode = getModelResponseTransformationMode(model = model)
  responseRecord = getModelResponseTransformationRecord(model = model)

  if (identical(mode, "model")) {
    return(emptyResponseBackTransformationPayload(
      mode = mode,
      status = "not_requested",
      note = "Response-scale back-transformation was not requested for this fitted model."
    ))
  }

  if (is.null(responseRecord)) {
    return(emptyResponseBackTransformationPayload(
      mode = mode,
      status = "not_available",
      note = "The fitted response is not a recorded user-created derived variable."
    ))
  }

  inverseType = responseRecord$inverseType %||% "unknown"
  if (!isUsableResponseInverseType(inverseType = inverseType)) {
    return(emptyResponseBackTransformationPayload(
      mode = mode,
      status = "not_available",
      responseVariable = responseRecord$variable %||% "",
      originalVariable = firstSourceVariable(responseRecord),
      transformationType = responseRecord$transformationType %||% "custom",
      inverseType = inverseType,
      note = "The response transformation does not have a recognised deterministic inverse."
    ))
  }

  numericReference = chooseModelNumericReference(
    model = model,
    mf = mf,
    predictorNames = predictorNames
  )

  ciOut = tryCatch(
    buildModelConfidenceIntervalData(
      model = model,
      numericReference = numericReference
    ),
    error = function(e) {
      NULL
    }
  )

  if (is.null(ciOut) || is.null(ciOut$table) || !is.data.frame(ciOut$table)) {
    return(emptyResponseBackTransformationPayload(
      mode = mode,
      status = "not_available",
      responseVariable = responseRecord$variable %||% "",
      originalVariable = firstSourceVariable(responseRecord),
      transformationType = responseRecord$transformationType %||% "custom",
      inverseType = inverseType,
      note = "No deterministic confidence-interval table was available for response back-transformation."
    ))
  }

  rows = buildResponseBackTransformationRows(
    ciTable = ciOut$table,
    responseRecord = responseRecord
  )

  if (nrow(rows) == 0) {
    return(emptyResponseBackTransformationPayload(
      mode = mode,
      status = "not_available",
      responseVariable = responseRecord$variable %||% "",
      originalVariable = firstSourceVariable(responseRecord),
      transformationType = responseRecord$transformationType %||% "custom",
      inverseType = inverseType,
      note = "The fitted model did not produce any safe rows for original-response-scale back-transformation."
    ))
  }

  list(
    mode = mode,
    status = "available",
    responseVariable = responseRecord$variable %||% "",
    originalVariable = firstSourceVariable(responseRecord),
    transformationType = responseRecord$transformationType %||% "custom",
    inverseType = inverseType,
    note = buildResponseBackTransformationAvailabilityNote(
      responseRecord = responseRecord,
      rows = rows
    ),
    table = rows
  )
}

#' Build an empty response back-transformation payload
#'
#' @param mode Response-transformation handling mode.
#' @param status Availability status.
#' @param responseVariable Derived response variable name.
#' @param originalVariable Original response variable name.
#' @param transformationType Transformation type label.
#' @param inverseType Inverse type label.
#' @param note Human-readable availability note.
#'
#' @return A named list.
#' @keywords internal
emptyResponseBackTransformationPayload = function(
  mode,
  status,
  responseVariable = "",
  originalVariable = "",
  transformationType = "",
  inverseType = "",
  note = ""
) {
  list(
    mode = mode,
    status = status,
    responseVariable = responseVariable,
    originalVariable = originalVariable,
    transformationType = transformationType,
    inverseType = inverseType,
    note = note,
    table = emptyResponseBackTransformationTable()
  )
}

#' Build an empty response back-transformation table
#'
#' @return A zero-row data frame with stable columns.
#' @keywords internal
emptyResponseBackTransformationTable = function() {
  data.frame(
    ciSection = character(0),
    quantity = character(0),
    sourceScale = character(0),
    originalScale = character(0),
    estimate = numeric(0),
    lower = numeric(0),
    upper = numeric(0),
    meaning = character(0),
    stringsAsFactors = FALSE
  )
}

#' Get the fitted-model response transformation record
#'
#' @param model A fitted model object.
#'
#' @return A transformation record or NULL.
#' @keywords internal
getModelResponseTransformationRecord = function(model) {
  records = getModelVariableTransformations(model = model)
  if (length(records) == 0) {
    return(NULL)
  }

  roles = getModelVariableTransformationRoles(model = model)
  responseNames = names(roles)[roles %in% "response"]

  if (length(responseNames) == 0) {
    return(NULL)
  }

  records[[responseNames[[1]]]]
}

#' Check whether an inverse type can be used for response back-transformation
#'
#' @param inverseType Character inverse label.
#'
#' @return Logical scalar.
#' @keywords internal
isUsableResponseInverseType = function(inverseType) {
  inverseType %in% c(
    "exp",
    "power10",
    "expm1",
    "square",
    "addConstant",
    "subtractConstant",
    "subtractFromConstant",
    "multiplyConstant",
    "divideConstant",
    "rootConstant"
  )
}

#' Return the first source variable in a transformation record
#'
#' @param record A transformation record.
#' @return Character scalar.
#' @keywords internal
firstSourceVariable = function(record) {
  sourceVariables = record$sourceVariables %||% character(0)
  if (length(sourceVariables) == 0) {
    return("")
  }

  sourceVariables[[1]]
}

#' Build safe response back-transformation rows
#'
#' @param ciTable Confidence-interval table from WMFM.
#' @param responseRecord Response transformation record.
#'
#' @return Data frame of back-transformed rows.
#' @keywords internal
buildResponseBackTransformationRows = function(ciTable, responseRecord) {
  if (!is.data.frame(ciTable) || nrow(ciTable) == 0) {
    return(emptyResponseBackTransformationTable())
  }

  baselineRows = ciTable[ciTable$ciSection %in% "baseline", , drop = FALSE]
  effectRows = ciTable[ciTable$ciSection %in% c("effect", "contrast"), , drop = FALSE]
  rows = list()

  if (nrow(baselineRows) > 0) {
    for (i in seq_len(nrow(baselineRows))) {
      rows[[length(rows) + 1L]] = backTransformResponseRow(
        row = baselineRows[i, , drop = FALSE],
        responseRecord = responseRecord,
        rowMeaning = "original response fitted value"
      )
    }
  }

  if (nrow(effectRows) > 0 && isResponseDifferenceInverseSupported(responseRecord$inverseType %||% "unknown")) {
    for (i in seq_len(nrow(effectRows))) {
      rows[[length(rows) + 1L]] = backTransformResponseDifferenceRow(
        row = effectRows[i, , drop = FALSE],
        responseRecord = responseRecord
      )
    }
  }

  rows = Filter(Negate(is.null), rows)
  if (length(rows) == 0) {
    return(emptyResponseBackTransformationTable())
  }

  out = do.call(rbind, rows)
  rownames(out) = NULL
  out
}

#' Back-transform one fitted-value row
#'
#' @param row One CI table row.
#' @param responseRecord Response transformation record.
#' @param rowMeaning Meaning label for the new row.
#'
#' @return One-row data frame or NULL.
#' @keywords internal
backTransformResponseRow = function(row, responseRecord, rowMeaning) {
  estimate = backTransformResponseValues(
    values = row$estimate,
    inverseType = responseRecord$inverseType,
    parameters = responseRecord$transformationParameters %||% list()
  )
  lower = backTransformResponseValues(
    values = row$lower,
    inverseType = responseRecord$inverseType,
    parameters = responseRecord$transformationParameters %||% list()
  )
  upper = backTransformResponseValues(
    values = row$upper,
    inverseType = responseRecord$inverseType,
    parameters = responseRecord$transformationParameters %||% list()
  )

  if (any(is.na(c(estimate, lower, upper)))) {
    return(NULL)
  }

  makeResponseBackTransformationRow(
    row = row,
    originalVariable = firstSourceVariable(responseRecord),
    estimate = estimate,
    lower = lower,
    upper = upper,
    meaning = rowMeaning
  )
}

#' Back-transform one transformed-scale difference row
#'
#' @param row One CI table row.
#' @param responseRecord Response transformation record.
#'
#' @return One-row data frame or NULL.
#' @keywords internal
backTransformResponseDifferenceRow = function(row, responseRecord) {
  estimate = backTransformResponseDifferenceValues(
    values = row$estimate,
    inverseType = responseRecord$inverseType
  )
  lower = backTransformResponseDifferenceValues(
    values = row$lower,
    inverseType = responseRecord$inverseType
  )
  upper = backTransformResponseDifferenceValues(
    values = row$upper,
    inverseType = responseRecord$inverseType
  )

  if (any(is.na(c(estimate, lower, upper)))) {
    return(NULL)
  }

  makeResponseBackTransformationRow(
    row = row,
    originalVariable = firstSourceVariable(responseRecord),
    estimate = estimate,
    lower = lower,
    upper = upper,
    meaning = "original response multiplier"
  )
}

#' Build a response back-transformation data-frame row
#'
#' @param row Original CI table row.
#' @param originalVariable Original response variable name.
#' @param estimate Back-transformed estimate.
#' @param lower Back-transformed lower endpoint.
#' @param upper Back-transformed upper endpoint.
#' @param meaning Meaning label.
#'
#' @return One-row data frame.
#' @keywords internal
makeResponseBackTransformationRow = function(
  row,
  originalVariable,
  estimate,
  lower,
  upper,
  meaning
) {
  data.frame(
    ciSection = row$ciSection,
    quantity = row$quantity,
    sourceScale = row$scale,
    originalScale = originalVariable,
    estimate = round(as.numeric(estimate), 4),
    lower = round(as.numeric(lower), 4),
    upper = round(as.numeric(upper), 4),
    meaning = meaning,
    stringsAsFactors = FALSE
  )
}

#' Apply an inverse response transformation to fitted values
#'
#' @param values Numeric vector on the transformed-response scale.
#' @param inverseType Character inverse label.
#' @param parameters Transformation parameter list.
#'
#' @return Numeric vector.
#' @keywords internal
backTransformResponseValues = function(values, inverseType, parameters = list()) {
  values = as.numeric(values)

  if (identical(inverseType, "exp")) {
    return(exp(values))
  }

  if (identical(inverseType, "power10")) {
    return(10^values)
  }

  if (identical(inverseType, "expm1")) {
    return(expm1(values))
  }

  if (identical(inverseType, "square")) {
    return(values^2)
  }

  constant = as.numeric(parameters$constant %||% NA_real_)

  if (identical(inverseType, "addConstant")) {
    return(values + constant)
  }

  if (identical(inverseType, "subtractConstant")) {
    return(values - constant)
  }

  if (identical(inverseType, "subtractFromConstant")) {
    return(constant - values)
  }

  if (identical(inverseType, "multiplyConstant")) {
    return(values * constant)
  }

  if (identical(inverseType, "divideConstant")) {
    return(values / constant)
  }

  if (identical(inverseType, "rootConstant")) {
    return(values^(1 / constant))
  }

  rep(NA_real_, length(values))
}

#' Check whether differences can become original-scale multipliers
#'
#' @param inverseType Character inverse label.
#'
#' @return Logical scalar.
#' @keywords internal
isResponseDifferenceInverseSupported = function(inverseType) {
  inverseType %in% c("exp", "power10", "expm1")
}

#' Back-transform transformed-scale differences where mathematically safe
#'
#' @param values Numeric vector of transformed-scale differences.
#' @param inverseType Character inverse label.
#'
#' @return Numeric vector.
#' @keywords internal
backTransformResponseDifferenceValues = function(values, inverseType) {
  values = as.numeric(values)

  if (identical(inverseType, "exp") || identical(inverseType, "expm1")) {
    return(exp(values))
  }

  if (identical(inverseType, "power10")) {
    return(10^values)
  }

  rep(NA_real_, length(values))
}


#' Build response back-transformation mode guidance
#'
#' @param payload Response back-transformation payload.
#'
#' @return Character vector of prompt guidance lines.
#' @keywords internal
buildResponseBackTransformationModeGuidance = function(payload) {
  mode = payload$mode %||% "both"
  responseVariable = payload$responseVariable %||% "the fitted response"
  originalVariable = payload$originalVariable %||% "the original response"

  if (identical(mode, "original")) {
    return(c(
      paste0("- Explain fitted values and effects on the original `", originalVariable, "` scale only."),
      paste0("- Do not describe expected values or effects for `", responseVariable, "` in the final explanation."),
      "- Mention the transformation only if needed to explain why the original response scale is being used."
    ))
  }

  if (identical(mode, "both")) {
    return(c(
      paste0("- Briefly acknowledge that the model was fitted to `", responseVariable, "` if this helps orient the reader."),
      paste0("- Use the original `", originalVariable, "` scale for all substantive fitted values and effect interpretations."),
      paste0("- Do not report numeric fitted values or effect sizes for `", responseVariable, "` in the final explanation."),
      "- In this mode, both means: keep the model-scale context available, but write the substantive interpretation on the original response scale."
    ))
  }

  character(0)
}

#' Check whether original response scale should drive interpretation
#'
#' @param payload Response back-transformation payload.
#'
#' @return Logical scalar.
#' @keywords internal
responseBackTransformationUsesOriginalScale = function(payload) {
  is.list(payload) &&
    identical(payload$status, "available") &&
    (payload$mode %in% c("both", "original"))
}

#' Build a response back-transformation availability note
#'
#' @param responseRecord Response transformation record.
#' @param rows Back-transformation table.
#'
#' @return Character scalar.
#' @keywords internal
buildResponseBackTransformationAvailabilityNote = function(responseRecord, rows) {
  originalVariable = firstSourceVariable(responseRecord)
  responseVariable = responseRecord$variable %||% "the fitted response"

  paste0(
    "The fitted response `",
    responseVariable,
    "` was recorded as a user-created transformation of `",
    originalVariable,
    "`. WMFM has supplied deterministic original-response-scale quantities for the explanation."
  )
}

#' Build response back-transformation prompt guidance
#'
#' @param model A fitted model object.
#' @param mf Optional model frame.
#' @param predictorNames Optional predictor names.
#' @param payload Optional precomputed response back-transformation payload.
#'
#' @return Character scalar prompt block.
#' @keywords internal
buildResponseBackTransformationPromptBlock = function(
  model,
  mf = NULL,
  predictorNames = NULL,
  payload = NULL
) {
  if (is.null(payload)) {
    payload = buildResponseBackTransformationPayload(
      model = model,
      mf = mf,
      predictorNames = predictorNames
    )
  }

  if (!identical(payload$status, "available")) {
    if (identical(payload$mode, "model")) {
      return("")
    }

    return(paste(c(
      "Response back-transformation payload:",
      paste0("- Status: ", payload$status, "."),
      paste0("- ", payload$note),
      "- Do not invent original-response-scale fitted values, effects, or confidence intervals."
    ), collapse = "\n"))
  }

  modeGuidance = buildResponseBackTransformationModeGuidance(payload = payload)

  lines = c(
    "Response back-transformation payload:",
    paste0("- Response modelled by WMFM: `", payload$responseVariable, "`."),
    paste0("- Original response variable: `", payload$originalVariable, "`."),
    paste0("- Transformation: ", payload$transformationType, "; inverse: ", payload$inverseType, "."),
    paste0("- Requested response transformation mode: ", payload$mode, "."),
    modeGuidance,
    "- Use these deterministic original-response-scale quantities directly when the selected response-transformation mode calls for original scale or both scales.",
    "- Do not recompute, round again, or invent additional back-transformed quantities.",
    "- If a row is labelled as an original response multiplier, describe it as a multiplicative change, not an additive difference.",
    "- For original response multipliers, prefer wording such as `the expected response is multiplied by X` or `the expected response is about X times as high`.",
    "- Do not write that the expected response `multiplies by` a value; the response is not the thing doing the multiplying.",
    "- Do not give both a multiplier and the equivalent percentage change unless the research question specifically asks for percentages.",
    "- Do not report expected values or effects on the transformed model scale when original-response-scale rows are supplied, unless the selected mode is model scale only."
  )

  for (i in seq_len(nrow(payload$table))) {
    row = payload$table[i, , drop = FALSE]
    quantityType = if (identical(row$meaning[[1]], "original response multiplier")) {
      "multiplier"
    } else {
      "number"
    }

    estimateText = formatExplanationQuantity(row$estimate[[1]], quantityType = quantityType)
    lowerText = formatExplanationQuantity(row$lower[[1]], quantityType = quantityType)
    upperText = formatExplanationQuantity(row$upper[[1]], quantityType = quantityType)

    lines = c(lines, paste0(
      "- ",
      row$quantity[[1]],
      " on original `",
      row$originalScale[[1]],
      "` scale (",
      row$meaning[[1]],
      "): estimate = ",
      estimateText,
      "; 95% confidence interval = ",
      lowerText,
      " to ",
      upperText
    ))
  }

  paste(lines, collapse = "\n")
}
