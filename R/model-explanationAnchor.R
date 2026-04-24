#' Choose deterministic explanation anchors for numeric predictors
#'
#' Builds auditable anchor metadata for student-facing explanations. These
#' anchors are intended for explanation and prompt inputs, not for changing the
#' fitted model. The raw anchor value is kept separately from the displayed
#' value so explanations can use readable numbers while calculations remain
#' traceable.
#'
#' @param model Optional fitted model object. Used only when `mf` is not
#'   supplied.
#' @param mf Optional model frame. If omitted, it is computed from `model`.
#' @param predictorNames Optional character vector of predictor names. If
#'   omitted, all non-response columns in `mf` are considered.
#' @param userAnchors Optional named numeric vector or list of user-specified
#'   anchors. These take priority over all other anchor sources.
#' @param meaningfulAnchors Optional named numeric vector or list of
#'   pedagogically meaningful anchors. These are used after `userAnchors`.
#' @param defaultSource Character scalar. Either `"median"` or `"mean"`.
#'
#' @return A named list containing anchor metadata, prompt text, and a cache key.
#' @keywords internal
#'
#' @importFrom stats median model.frame
chooseExplanationAnchor = function(
    model = NULL,
    mf = NULL,
    predictorNames = NULL,
    userAnchors = NULL,
    meaningfulAnchors = NULL,
    defaultSource = c("median", "mean")) {

  defaultSource = match.arg(defaultSource)

  mf = getExplanationAnchorModelFrame(model = model, mf = mf)
  predictorNames = getExplanationAnchorPredictorNames(
    mf = mf,
    predictorNames = predictorNames
  )

  numericNames = getExplanationAnchorNumericPredictors(
    mf = mf,
    predictorNames = predictorNames
  )

  userAnchors = normalizeExplanationAnchorInput(userAnchors, "userAnchors")
  meaningfulAnchors = normalizeExplanationAnchorInput(
    meaningfulAnchors,
    "meaningfulAnchors"
  )

  if (length(numericNames) == 0) {
    return(list(
      anchorReference = "none",
      table = buildEmptyExplanationAnchorTable(),
      promptText = "Explanation anchors: no numeric predictors require anchors.",
      cacheKey = "no_numeric_anchors"
    ))
  }

  anchorTable = do.call(
    rbind,
    lapply(
      numericNames,
      buildExplanationAnchorRow,
      mf = mf,
      userAnchors = userAnchors,
      meaningfulAnchors = meaningfulAnchors,
      defaultSource = defaultSource
    )
  )

  list(
    anchorReference = paste(unique(anchorTable$anchorSource), collapse = "+"),
    table = anchorTable,
    promptText = buildExplanationAnchorPromptText(anchorTable),
    cacheKey = buildExplanationAnchorCacheKey(anchorTable)
  )
}

#' Format explanation-anchor metadata for display
#'
#' @param anchorInfo Anchor metadata from `chooseExplanationAnchor()`.
#'
#' @return A data frame with raw and displayed anchor values.
#' @keywords internal
formatExplanationAnchorInfo = function(anchorInfo) {

  if (!is.list(anchorInfo) || !is.data.frame(anchorInfo$table)) {
    stop("`anchorInfo` must be the output from chooseExplanationAnchor().", call. = FALSE)
  }

  anchorInfo$table
}

#' Build explanation-anchor audit metadata
#'
#' @param anchorInfo Anchor metadata from `chooseExplanationAnchor()`.
#'
#' @return A named list suitable for audit or developer-feedback use.
#' @keywords internal
buildAnchorAuditEntry = function(anchorInfo) {

  if (!is.list(anchorInfo) || !is.data.frame(anchorInfo$table)) {
    stop("`anchorInfo` must be the output from chooseExplanationAnchor().", call. = FALSE)
  }

  list(
    anchorReference = anchorInfo$anchorReference,
    note = buildExplanationAnchorAuditNote(anchorInfo$table),
    table = anchorInfo$table
  )
}

getExplanationAnchorModelFrame = function(model = NULL, mf = NULL) {

  if (!is.null(mf)) {
    if (!is.data.frame(mf)) {
      stop("`mf` must be a data.frame or NULL.", call. = FALSE)
    }

    return(mf)
  }

  if (is.null(model)) {
    stop("Supply either `model` or `mf`.", call. = FALSE)
  }

  model.frame(model)
}

getExplanationAnchorPredictorNames = function(mf, predictorNames = NULL) {

  if (!is.data.frame(mf)) {
    stop("`mf` must be a data.frame.", call. = FALSE)
  }

  if (is.null(predictorNames)) {
    if (ncol(mf) <= 1) {
      return(character(0))
    }

    return(names(mf)[-1])
  }

  if (!is.character(predictorNames)) {
    stop("`predictorNames` must be a character vector or NULL.", call. = FALSE)
  }

  predictorNames[predictorNames %in% names(mf)]
}

getExplanationAnchorNumericPredictors = function(mf, predictorNames) {

  if (length(predictorNames) == 0) {
    return(character(0))
  }

  predictorNames[vapply(mf[predictorNames], is.numeric, logical(1))]
}

normalizeExplanationAnchorInput = function(x, argumentName) {

  if (is.null(x)) {
    return(numeric(0))
  }

  if (is.list(x) && !is.data.frame(x)) {
    x = unlist(x, use.names = TRUE)
  }

  if (!is.numeric(x)) {
    stop("`", argumentName, "` must be a named numeric vector, named numeric list, or NULL.", call. = FALSE)
  }

  if (is.null(names(x)) || any(!nzchar(names(x)))) {
    stop("`", argumentName, "` must be named.", call. = FALSE)
  }

  x
}

buildExplanationAnchorRow = function(
    varName,
    mf,
    userAnchors,
    meaningfulAnchors,
    defaultSource) {

  x = mf[[varName]]
  nonMissing = x[!is.na(x)]

  observedMin = if (length(nonMissing) == 0) NA_real_ else min(nonMissing)
  observedMax = if (length(nonMissing) == 0) NA_real_ else max(nonMissing)
  zeroInRange = isTRUE(!is.na(observedMin) && !is.na(observedMax) && observedMin <= 0 && observedMax >= 0)

  chosen = chooseExplanationAnchorValue(
    varName = varName,
    nonMissing = nonMissing,
    userAnchors = userAnchors,
    meaningfulAnchors = meaningfulAnchors,
    defaultSource = defaultSource
  )

  qualityFlags = buildExplanationAnchorQualityFlags(
    anchorValue = chosen$value,
    anchorSource = chosen$source,
    observedMin = observedMin,
    observedMax = observedMax,
    zeroInRange = zeroInRange
  )

  data.frame(
    anchorVariable = varName,
    anchorValue = chosen$value,
    anchorDisplayValue = if (is.na(chosen$value)) NA_character_ else formatExplanationAnchor(chosen$value),
    anchorSource = chosen$source,
    anchorReason = chosen$reason,
    anchorScale = "original",
    observedMin = observedMin,
    observedMax = observedMax,
    observedRange = formatExplanationAnchorRange(observedMin, observedMax),
    zeroInRange = zeroInRange,
    qualityFlags = paste(qualityFlags, collapse = ";"),
    stringsAsFactors = FALSE
  )
}

chooseExplanationAnchorValue = function(
    varName,
    nonMissing,
    userAnchors,
    meaningfulAnchors,
    defaultSource) {

  if (varName %in% names(userAnchors)) {
    return(list(
      value = as.numeric(userAnchors[[varName]]),
      source = "user",
      reason = "The user supplied this anchor value."
    ))
  }

  if (varName %in% names(meaningfulAnchors)) {
    return(list(
      value = as.numeric(meaningfulAnchors[[varName]]),
      source = "meaningful",
      reason = "A pedagogically meaningful anchor value was supplied."
    ))
  }

  if (length(nonMissing) == 0) {
    return(list(
      value = NA_real_,
      source = "missing",
      reason = "No non-missing observed values were available."
    ))
  }

  if (identical(defaultSource, "mean")) {
    return(list(
      value = mean(nonMissing),
      source = "mean",
      reason = "The sample mean was used as a typical observed value."
    ))
  }

  list(
    value = stats::median(nonMissing),
    source = "median",
    reason = "The sample median was used as a typical observed value."
  )
}

buildExplanationAnchorQualityFlags = function(
    anchorValue,
    anchorSource,
    observedMin,
    observedMax,
    zeroInRange) {

  flags = character(0)

  if (is.na(anchorValue)) {
    return("anchorMissing")
  }

  if (identical(anchorValue, 0) && identical(anchorSource, "median") && zeroInRange) {
    flags = c(flags, "zeroAnchorFromTypicalValue")
  }

  if (!is.na(observedMin) && !is.na(observedMax)) {
    if (anchorValue < observedMin || anchorValue > observedMax) {
      flags = c(flags, "anchorOutsideObservedRange")
    }
  }

  flags
}

formatExplanationAnchorRange = function(observedMin, observedMax) {

  if (is.na(observedMin) || is.na(observedMax)) {
    return(NA_character_)
  }

  paste0(
    "[",
    formatExplanationAnchor(observedMin),
    ", ",
    formatExplanationAnchor(observedMax),
    "]"
  )
}

buildEmptyExplanationAnchorTable = function() {
  data.frame(
    anchorVariable = character(0),
    anchorValue = numeric(0),
    anchorDisplayValue = character(0),
    anchorSource = character(0),
    anchorReason = character(0),
    anchorScale = character(0),
    observedMin = numeric(0),
    observedMax = numeric(0),
    observedRange = character(0),
    zeroInRange = logical(0),
    qualityFlags = character(0),
    stringsAsFactors = FALSE
  )
}

buildExplanationAnchorPromptText = function(anchorTable) {

  if (!is.data.frame(anchorTable) || nrow(anchorTable) == 0) {
    return("Explanation anchors: no numeric predictors require anchors.")
  }

  lines = vapply(
    seq_len(nrow(anchorTable)),
    function(i) {
      row = anchorTable[i, , drop = FALSE]
      paste0(
        "- ",
        row$anchorVariable,
        ": use ",
        row$anchorDisplayValue,
        " as the explanation anchor (source: ",
        row$anchorSource,
        "; observed range ",
        row$observedRange,
        "). ",
        row$anchorReason
      )
    },
    character(1)
  )

  paste(
    c(
      "Explanation anchors:",
      lines,
      "Use the displayed anchor values in student-facing wording, but keep calculations traceable to the raw anchor values."
    ),
    collapse = "\n"
  )
}

buildExplanationAnchorCacheKey = function(anchorTable) {

  if (!is.data.frame(anchorTable) || nrow(anchorTable) == 0) {
    return("no_numeric_anchors")
  }

  paste(
    vapply(
      seq_len(nrow(anchorTable)),
      function(i) {
        row = anchorTable[i, , drop = FALSE]
        paste(
          row$anchorVariable,
          row$anchorSource,
          format(row$anchorValue, digits = 16, scientific = FALSE),
          sep = "="
        )
      },
      character(1)
    ),
    collapse = "|"
  )
}

buildExplanationAnchorAuditNote = function(anchorTable) {

  if (!is.data.frame(anchorTable) || nrow(anchorTable) == 0) {
    return("No numeric predictors were present, so no explanation anchors were needed.")
  }

  sources = unique(anchorTable$anchorSource)

  paste0(
    "Explanation anchors were selected using source(s): ",
    paste(sources, collapse = ", "),
    ". Raw anchor values are stored separately from displayed anchor values."
  )
}
