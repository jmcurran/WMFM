#' Generate bad explanations with an LLM
#'
#' @param x A `wmfmModel` object.
#' @param baseExplanation Character scalar giving the good explanation.
#' @param plan A plan object produced by `buildBadExplanationPlan()`.
#' @param chat A chat provider object with a callable `$chat()` method.
#' @param showProgress Logical. Should a command-line status message be shown
#'   before awaiting the LLM response?
#'
#' @return Raw character response from the LLM.
#' @keywords internal
generateBadExplanationWithLlm = function(
    x,
    baseExplanation,
    plan,
    chat,
    showProgress = FALSE
) {

  if (!is.logical(showProgress) || length(showProgress) != 1 || is.na(showProgress)) {
    stop("`showProgress` must be TRUE or FALSE.", call. = FALSE)
  }

  if (is.null(chat)) {
    stop("`chat` must not be NULL.", call. = FALSE)
  }

  chatMethod = tryCatch(
    chat$chat,
    error = function(e) {
      NULL
    }
  )

  if (is.null(chatMethod) || !is.function(chatMethod)) {
    stop(
      "`chat` must provide a callable `$chat()` method.",
      call. = FALSE
    )
  }

  prompt = buildBadExplanationPrompt(
    x = x,
    baseExplanation = baseExplanation,
    plan = plan
  )

  if (isTRUE(showProgress)) {
    cat("  Awaiting LLM response...\n")
  }

  rawResponse = chatMethod(prompt)
  safeWmfmScalar(rawResponse, naString = "")
}

#' Parse JSON returned for bad explanation generation
#'
#' Parses the raw text returned by a language model generation call. The parser
#' is tolerant of fenced code blocks and leading or trailing non-JSON text, but
#' it expects either a single JSON object or a JSON array of objects.
#'
#' @param rawResponse Character scalar raw LLM response.
#'
#' @return A parsed list.
#' @keywords internal
#' @importFrom jsonlite fromJSON
#' @importFrom utils tail
parseBadExplanationResponse = function(rawResponse) {

  if (!is.character(rawResponse) || length(rawResponse) != 1L || is.na(rawResponse)) {
    stop("`rawResponse` must be a non-missing character scalar.", call. = FALSE)
  }

  jsonText = trimws(rawResponse)

  if (!nzchar(jsonText)) {
    stop("LLM returned an empty bad explanation response.", call. = FALSE)
  }

  jsonText = sub("^```json\\s*", "", jsonText)
  jsonText = sub("^```\\s*", "", jsonText)
  jsonText = sub("\\s*```$", "", jsonText)

  startArray = regexpr("\\[", jsonText, perl = TRUE)[1]
  endArray = tail(gregexpr("\\]", jsonText, perl = TRUE)[[1]], 1)
  startObject = regexpr("\\{", jsonText, perl = TRUE)[1]
  endObject = tail(gregexpr("\\}", jsonText, perl = TRUE)[[1]], 1)

  if (startArray != -1L && endArray != -1L && endArray > startArray) {
    jsonText = substr(jsonText, startArray, endArray)
  } else if (startObject != -1L && endObject != -1L && endObject > startObject) {
    jsonText = substr(jsonText, startObject, endObject)
  } else {
    stop("Could not locate JSON in the bad explanation response.", call. = FALSE)
  }

  parsed = tryCatch(
    jsonlite::fromJSON(jsonText, simplifyVector = FALSE),
    error = function(e) {
      stop(
        "Failed to parse bad explanation JSON: ",
        conditionMessage(e),
        call. = FALSE
      )
    }
  )

  if (is.list(parsed) && !is.null(names(parsed))) {
    parsed = list(parsed)
  }

  if (!is.list(parsed)) {
    stop("Parsed bad explanation response is not a JSON list.", call. = FALSE)
  }

  parsed
}

#' Validate parsed bad explanation output
#'
#' @param parsed Parsed JSON object.
#' @param plan Plan object produced by `buildBadExplanationPlan()`.
#'
#' @return Validated parsed object.
#' @keywords internal
validateBadExplanationResponse = function(parsed, plan) {

  if (!is.list(parsed) || length(parsed) != length(plan$explanationNames)) {
    stop(
      "Bad explanation response did not return the expected number of explanations.",
      call. = FALSE
    )
  }

  parsed = lapply(parsed, function(item) {

    if (!is.list(item)) {
      stop("Each generated explanation must be a JSON object.", call. = FALSE)
    }

    requiredFields = c("name", "text", "errorTypes", "severity")
    missingFields = setdiff(requiredFields, names(item))

    if (length(missingFields) > 0) {
      stop(
        paste0(
          "Generated explanation is missing required field(s): ",
          paste(missingFields, collapse = ", ")
        ),
        call. = FALSE
      )
    }

    itemName = safeWmfmScalar(item$name, naString = "")
    itemText = safeWmfmScalar(item$text, naString = "")
    itemSeverity = safeWmfmScalar(item$severity, naString = "")

    errorTypes = item$errorTypes

    if (is.list(errorTypes)) {
      errorTypes = unlist(errorTypes, use.names = FALSE)
    }

    if (is.null(errorTypes)) {
      errorTypes = character(0)
    }

    errorTypes = as.character(errorTypes)
    errorTypes = errorTypes[!is.na(errorTypes)]
    errorTypes = trimws(errorTypes)
    errorTypes = errorTypes[nzchar(errorTypes)]

    if (!nzchar(itemName)) {
      stop("Each generated explanation must have a valid `name` field.", call. = FALSE)
    }

    if (!nzchar(itemText)) {
      stop("Each generated explanation must have non-empty `text`.", call. = FALSE)
    }

    if (length(errorTypes) < 1) {
      stop(
        "Each generated explanation must have at least one valid `errorTypes` entry.",
        call. = FALSE
      )
    }

    if (!nzchar(itemSeverity)) {
      stop("Each generated explanation must have a valid `severity` field.", call. = FALSE)
    }

    list(
      name = itemName,
      text = itemText,
      errorTypes = errorTypes,
      severity = itemSeverity
    )
  })

  returnedNames = vapply(parsed, function(item) item$name, character(1))

  if (!identical(returnedNames, plan$explanationNames)) {
    stop(
      paste(
        "Generated explanation names did not match the requested plan names."
      ),
      call. = FALSE
    )
  }

  parsed
}

#' Audit whether bad explanations are being penalised by the current rubric
#'
#' Builds a compact audit summary around a set of already graded bad
#' explanations, with optional comparison against a graded good explanation.
#' This is intended as a lightweight helper for diagnosing adversarial or
#' deliberately flawed explanations that may be slipping through the current
#' deterministic rubric.
#'
#' @param x A `wmfmGradeListObj` containing graded bad explanations.
#' @param goodGrade Optional `wmfmGrade` object for a reference good
#'   explanation.
#' @param method Character scalar. Grading method to audit. One of
#'   `"deterministic"` or `"llm"`.
#' @param minExpectedDrop Numeric scalar. Minimum drop in mark, relative to the
#'   good explanation, required before an explanation is treated as clearly
#'   penalised.
#' @param flaggedThreshold Numeric scalar. Any explanation with a mark greater
#'   than or equal to this threshold is flagged.
#' @param expectedMetrics Optional named list. Each element name should match an
#'   explanation name in `x`, and each element value should be a character
#'   vector of metric names that you expected to lose marks.
#' @param ... Unused.
#'
#' @return An object of class `wmfmBadExplanationAudit`.
#' @export
#'
#' @examples
#' \dontrun{
#' badGrades = grade(modelObj, explanation = badVec, method = "deterministic")
#' goodGrade = grade(modelObj, explanation = modelObj$explanation, method = "deterministic")
#'
#' audit = auditBadExplanationGrading(
#'   badGrades,
#'   goodGrade = goodGrade,
#'   expectedMetrics = list(
#'     effectDirectionError = c("factualScore"),
#'     wrongScaleError = c("factualScore", "clarityScore"),
#'     rSquaredOverclaim = c("factualScore", "calibrationScore"),
#'     logicalContradiction = c("factualScore", "clarityScore")
#'   )
#' )
#'
#' print(audit)
#' }
auditBadExplanationGrading = function(
    x,
    goodGrade = NULL,
    method = c("deterministic", "llm"),
    minExpectedDrop = 1,
    flaggedThreshold = 9,
    expectedMetrics = NULL,
    ...
) {

  if (!inherits(x, "wmfmGradeListObj")) {
    stop("`x` must inherit from `wmfmGradeListObj`.", call. = FALSE)
  }

  if (!is.null(goodGrade) && !inherits(goodGrade, "wmfmGrade")) {
    stop("`goodGrade` must be NULL or inherit from `wmfmGrade`.", call. = FALSE)
  }

  method = match.arg(method)

  if (!is.numeric(minExpectedDrop) || length(minExpectedDrop) != 1 || is.na(minExpectedDrop)) {
    stop("`minExpectedDrop` must be a single non-missing number.", call. = FALSE)
  }

  if (!is.numeric(flaggedThreshold) || length(flaggedThreshold) != 1 || is.na(flaggedThreshold)) {
    stop("`flaggedThreshold` must be a single non-missing number.", call. = FALSE)
  }

  if (!is.null(expectedMetrics)) {
    if (!is.list(expectedMetrics) || is.null(names(expectedMetrics))) {
      stop("`expectedMetrics` must be NULL or a named list.", call. = FALSE)
    }
  }

  extractMethodBlock = function(gradeObj, method) {
    blocks = gradeObj$scores$byMethod %||% list()

    if (!method %in% names(blocks)) {
      return(NULL)
    }

    blocks[[method]]
  }

  extractMark = function(gradeObj, method) {
    block = extractMethodBlock(gradeObj, method)

    if (is.null(block)) {
      return(NA_real_)
    }

    suppressWarnings(as.numeric(block$mark)[1])
  }

  extractWhereMarksLost = function(gradeObj, method) {
    feedbackBlocks = gradeObj$feedback$byMethod %||% list()

    if (!method %in% names(feedbackBlocks)) {
      return(NULL)
    }

    feedbackBlocks[[method]]$whereMarksLost
  }

  extractMetricSummary = function(gradeObj, method) {
    block = extractMethodBlock(gradeObj, method)

    if (is.null(block)) {
      return(NULL)
    }

    block$metricSummary
  }

  extractMetricsWithLoss = function(gradeObj, method) {
    lossesDf = extractWhereMarksLost(gradeObj, method)

    if (!is.data.frame(lossesDf) || nrow(lossesDf) < 1 || !"metric" %in% names(lossesDf)) {
      return(character(0))
    }

    lossesDf = lossesDf[!is.na(lossesDf$metric), , drop = FALSE]
    lossesDf = lossesDf[lossesDf$metric != "overallScore", , drop = FALSE]

    unique(as.character(lossesDf$metric))
  }

  extractMetricLossDetails = function(gradeObj, method) {
    lossesDf = extractWhereMarksLost(gradeObj, method)

    if (!is.data.frame(lossesDf) || nrow(lossesDf) < 1) {
      return(data.frame(
        metric = character(0),
        label = character(0),
        marksLost = numeric(0),
        stringsAsFactors = FALSE
      ))
    }

    keepCols = intersect(c("metric", "label", "marksLost"), names(lossesDf))
    out = lossesDf[, keepCols, drop = FALSE]

    if (!"metric" %in% names(out)) {
      out$metric = NA_character_
    }

    if (!"label" %in% names(out)) {
      out$label = out$metric
    }

    if (!"marksLost" %in% names(out)) {
      out$marksLost = NA_real_
    }

    out = out[out$metric != "overallScore", , drop = FALSE]
    rownames(out) = NULL
    out
  }

  goodMark = NA_real_

  if (!is.null(goodGrade)) {
    goodMark = extractMark(goodGrade, method)
  }

  explanationNames = names(x$grades)

  auditRows = lapply(seq_along(x$grades), function(i) {
    gradeObj = x$grades[[i]]
    explanationName = explanationNames[i]
    badMark = extractMark(gradeObj, method)
    markDrop = if (is.na(goodMark) || is.na(badMark)) {
      NA_real_
    } else {
      goodMark - badMark
    }

    metricsWithLoss = extractMetricsWithLoss(gradeObj, method)
    expectedForThis = expectedMetrics[[explanationName]] %||% character(0)
    expectedForThis = unique(as.character(expectedForThis))
    expectedDetected = intersect(expectedForThis, metricsWithLoss)
    expectedMissed = setdiff(expectedForThis, metricsWithLoss)

    metricSummary = extractMetricSummary(gradeObj, method)
    metricLossDetails = extractMetricLossDetails(gradeObj, method)

    list(
      explanationName = explanationName,
      mark = badMark,
      goodMark = goodMark,
      markDrop = markDrop,
      isFlaggedHighMark = !is.na(badMark) && badMark >= flaggedThreshold,
      isFlaggedLowDrop = !is.na(markDrop) && markDrop < minExpectedDrop,
      expectedMetrics = expectedForThis,
      expectedDetected = expectedDetected,
      expectedMissed = expectedMissed,
      metricSummary = metricSummary,
      metricLossDetails = metricLossDetails
    )
  })

  summaryDf = data.frame(
    explanationName = vapply(auditRows, `[[`, character(1), "explanationName"),
    mark = vapply(auditRows, `[[`, numeric(1), "mark"),
    goodMark = vapply(auditRows, `[[`, numeric(1), "goodMark"),
    markDrop = vapply(auditRows, `[[`, numeric(1), "markDrop"),
    flaggedHighMark = vapply(auditRows, `[[`, logical(1), "isFlaggedHighMark"),
    flaggedLowDrop = vapply(auditRows, `[[`, logical(1), "isFlaggedLowDrop"),
    expectedMetricCount = vapply(auditRows, function(row) length(row$expectedMetrics), integer(1)),
    detectedExpectedMetricCount = vapply(auditRows, function(row) length(row$expectedDetected), integer(1)),
    missedExpectedMetricCount = vapply(auditRows, function(row) length(row$expectedMissed), integer(1)),
    stringsAsFactors = FALSE
  )

  summaryDf$flagged = summaryDf$flaggedHighMark | summaryDf$flaggedLowDrop | summaryDf$missedExpectedMetricCount > 0

  flaggedDf = summaryDf[summaryDf$flagged, , drop = FALSE]
  rownames(flaggedDf) = NULL

  out = list(
    method = method,
    nExplanations = length(x$grades),
    goodMark = goodMark,
    minExpectedDrop = minExpectedDrop,
    flaggedThreshold = flaggedThreshold,
    summary = summaryDf,
    flagged = flaggedDf,
    details = stats::setNames(auditRows, explanationNames),
    source = x
  )

  class(out) = c("wmfmBadExplanationAudit", "list")
  out
}
