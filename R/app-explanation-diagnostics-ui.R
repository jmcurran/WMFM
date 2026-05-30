#' Build Developer Mode explanation prompt diagnostics UI
#'
#' @param diagnostics Optional diagnostics list from model fitting.
#'
#' @return Shiny tag list.
#' @keywords internal
#' @noRd
buildExplanationPromptDiagnosticsUi = function(diagnostics = NULL) {
  if (is.null(diagnostics)) {
    return(tags$p(
      class = "wmfm-explanation-helper-note",
      "No explanation prompt diagnostics are available yet. Fit a model first."
    ))
  }

  payload = diagnostics$followupPayload %||% list()
  prediction = payload$predictionResult %||% list()
  unitChange = payload$unitChangeResult %||% list()
  deterministic = if (length(unitChange)) {
    unitChange
  } else {
    prediction
  }
  missingOrAmbiguous = deterministic$warnings %||% ""
  diagnosticsBundle = paste(
    "Raw follow-up question:", payload$originalText %||% diagnostics$followupText %||% "",
    "",
    "Follow-up category:", as.character(payload$category %||% ""),
    "",
    "Deterministic status:", as.character(deterministic$status %||% "not_applicable"),
    "",
    "Model type:", as.character(deterministic$modelType %||% "not_applicable"),
    "",
    "GLM family/link:", paste(as.character(prediction$glmFamily %||% "not_applicable"), as.character(prediction$glmLink %||% "not_applicable"), sep = "/"),
    "",
    "Response scale:", as.character(prediction$responseScale %||% "not_applicable"),
    "",
    "Resolved predictor values:", paste(capture.output(str(prediction$resolvedPredictorValues %||% list())), collapse = "\n"),
    "",
    "Completed predictor values:", paste(capture.output(str(prediction$completedPredictorValues %||% list())), collapse = "\n"),
    "",
    "Prediction payload:", paste(capture.output(str(prediction)), collapse = "\n"),
    "",
    "Unit-change payload:", paste(capture.output(str(unitChange)), collapse = "\n"),
    "",
    "Assembled prompt excerpt:", substr(diagnostics$assembledPrompt %||% "", 1, 8000),
    sep = "\n"
  )

  diagnosticsJson = buildExplanationPromptDiagnosticsJson(diagnostics = diagnostics)

  tagList(
    tags$strong("Explanation prompt diagnostics"),
    tags$p(
      class = "wmfm-explanation-helper-note",
      "Developer-mode diagnostics for follow-up classification, deterministic prediction or unit-change payload, and assembled explanation prompt. Copy the diagnostics bundle into a debugging request when follow-up answers look wrong."
    ),
    tags$strong("Copyable diagnostics bundle"),
    tags$textarea(
      id = "diag_followup_bundle",
      readonly = "readonly",
      rows = 18,
      style = "width: 100%; font-family: monospace; white-space: pre;",
      diagnosticsBundle
    ),
    tags$strong("Downloadable diagnostics JSON"),
    tags$p(
      class = "wmfm-explanation-helper-note",
      "Use this button to save the same diagnostics as a JSON file for upload into a debugging chat. The default filename includes a timestamp, for example wmfm-followup-diagnostics-YYYYMMDD-HHMMSS.json."
    ),
    downloadButton(
      outputId = "diag_followup_json_download",
      label = "Download diagnostics JSON"
    ),
    tags$textarea(
      id = "diag_followup_json_bundle",
      readonly = "readonly",
      rows = 10,
      style = "width: 100%; font-family: monospace; white-space: pre;",
      diagnosticsJson
    ),
    tags$strong("Raw follow-up question text received by server"),
    tags$pre(id = "diag_followup_raw_text", payload$originalText %||% diagnostics$followupText %||% ""),
    tags$strong("Follow-up classification"),
    tags$pre(id = "diag_followup_classification", as.character(payload$category %||% "")),
    tags$strong("Deterministic follow-up status"),
    tags$pre(id = "diag_followup_deterministic_status", as.character(deterministic$status %||% "not_applicable")),
    tags$strong("Resolved predictor values"),
    tags$pre(id = "diag_followup_resolved_values", paste(capture.output(str(prediction$resolvedPredictorValues %||% list())), collapse = "\n")),
    tags$strong("Missing or ambiguous predictor values"),
    tags$pre(id = "diag_followup_missing_values", paste(capture.output(str(missingOrAmbiguous)), collapse = "\n")),
    tags$strong("Deterministic prediction payload"),
    tags$pre(id = "diag_followup_prediction_payload", paste(capture.output(str(prediction)), collapse = "\n")),
    tags$strong("Deterministic unit-change payload"),
    tags$pre(id = "diag_followup_unit_change_payload", paste(capture.output(str(unitChange)), collapse = "\n")),
    tags$strong("Final assembled prompt excerpt"),
    tags$pre(id = "diag_followup_prompt_excerpt", substr(diagnostics$assembledPrompt %||% "", 1, 8000))
  )
}



#' Sanitise explanation diagnostics values for JSON
#'
#' Converts provider outputs and classed values into plain JSON-safe lists,
#' vectors, and scalars before diagnostics serialisation.
#'
#' @param x Object to sanitise.
#' @param depth Current recursion depth.
#' @param maxDepth Maximum recursion depth.
#'
#' @return A plain JSON-safe object.
#' @keywords internal
#' @noRd
sanitiseExplanationDiagnosticsJsonValue = function(x, depth = 0L, maxDepth = 10L) {
  if (depth > maxDepth) {
    return("<max depth reached>")
  }

  if (is.null(x)) {
    return(NULL)
  }

  if (inherits(x, "POSIXt")) {
    return(as.character(x))
  }

  if (is.function(x) || is.environment(x) || inherits(x, "externalptr")) {
    return(paste0("<", paste(class(x), collapse = "/"), ">"))
  }

  if (is.data.frame(x)) {
    out = as.data.frame(x, stringsAsFactors = FALSE)
    for (colName in names(out)) {
      out[[colName]] = sanitiseExplanationDiagnosticsJsonValue(
        out[[colName]],
        depth = depth + 1L,
        maxDepth = maxDepth
      )
    }
    rownames(out) = NULL
    return(out)
  }

  if (is.factor(x)) {
    return(as.character(x))
  }

  if (is.atomic(x)) {
    out = unclass(x)
    attributes(out) = NULL

    if (is.numeric(out)) {
      out[!is.finite(out)] = NA_real_
    }

    return(unname(out))
  }

  if (is.list(x)) {
    x = unclass(x)
    attributes(x) = attributes(x)[intersect(names(attributes(x)), "names")]

    out = lapply(
      x,
      sanitiseExplanationDiagnosticsJsonValue,
      depth = depth + 1L,
      maxDepth = maxDepth
    )

    itemNames = names(out)

    if (!is.null(itemNames)) {
      names(out) = make.names(itemNames, unique = TRUE)
    }

    return(out)
  }

  as.character(x)
}

#' Coerce generated explanation diagnostics text to a JSON-safe scalar
#'
#' @param x Candidate generated explanation object.
#'
#' @return Character scalar.
#' @keywords internal
#' @noRd
coerceExplanationDiagnosticsText = function(x) {
  if (is.null(x)) {
    return("")
  }

  if (is.character(x)) {
    return(paste(x, collapse = "\n"))
  }

  if (is.atomic(x)) {
    out = unclass(x)
    attributes(out) = NULL
    return(paste(as.character(out), collapse = "\n"))
  }

  if (is.list(x) && !is.null(x$content) && is.character(x$content)) {
    return(paste(x$content, collapse = "\n"))
  }

  text = tryCatch(
    as.character(x),
    error = function(e) {
      character(0)
    }
  )

  if (length(text) > 0L) {
    return(paste(text, collapse = "\n"))
  }

  paste(capture.output(str(x)), collapse = "\n")
}

#' Build Developer Mode explanation prompt diagnostics JSON
#'
#' @param diagnostics Optional diagnostics list from model fitting.
#'
#' @return Character scalar containing pretty JSON diagnostics.
#' @keywords internal
#' @noRd
buildExplanationPromptDiagnosticsJson = function(diagnostics = NULL) {
  if (is.null(diagnostics)) {
    diagnostics = list()
  }

  payload = diagnostics$followupPayload %||% list()
  prediction = payload$predictionResult %||% list()
  unitChange = payload$unitChangeResult %||% list()
  deterministic = if (length(unitChange)) {
    unitChange
  } else {
    prediction
  }
  out = list(
    rawFollowupQuestion = payload$originalText %||% diagnostics$followupText %||% "",
    followupCategory = as.character(payload$category %||% ""),
    deterministicStatus = as.character(deterministic$status %||% "not_applicable"),
    reason = as.character(deterministic$reason %||% ""),
    modelType = as.character(deterministic$modelType %||% ""),
    glmFamily = as.character(prediction$glmFamily %||% ""),
    glmLink = as.character(prediction$glmLink %||% ""),
    responseScale = as.character(prediction$responseScale %||% ""),
    responseDescription = as.character(prediction$responseDescription %||% ""),
    warnings = prediction$warnings %||% character(0),
    suppliedPredictorValues = prediction$suppliedPredictorValues %||% list(),
    resolvedPredictorValues = prediction$resolvedPredictorValues %||% list(),
    completedPredictorValues = prediction$completedPredictorValues %||% list(),
    predictionPayload = prediction,
    unitChangePayload = unitChange,
    requestedPredictor = as.character(unitChange$predictorName %||% ""),
    requestedUnitChange = unitChange$requestedUnitChange %||% NULL,
    transformedUnitChangeEffect = unitChange$unitChangeEffect %||% NULL,
    transformedUnitChangeInterval = unitChange$confidenceInterval %||% NULL,
    generatedExplanation = coerceExplanationDiagnosticsText(
      diagnostics$generatedExplanation %||% diagnostics$finalExplanation %||% ""
    ),
    promptExcerpt = substr(coerceExplanationDiagnosticsText(diagnostics$assembledPrompt %||% ""), 1, 8000),
    assembledPromptExcerpt = substr(coerceExplanationDiagnosticsText(diagnostics$assembledPrompt %||% ""), 1, 8000)
  )

  out = sanitiseExplanationDiagnosticsJsonValue(out)

  toJSON(
    out,
    auto_unbox = TRUE,
    pretty = TRUE,
    null = "null",
    na = "null"
  )
}
