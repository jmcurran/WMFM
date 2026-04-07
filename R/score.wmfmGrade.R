#' Score a WMFM grade object
#'
#' Scores a `wmfmGrade` object using either the deterministic WMFM rubric or an
#' LLM-based scoring rubric. The candidate explanation is always scored. If a
#' reference answer is present, it is scored separately and used to enrich
#' feedback.
#'
#' @param x A `wmfmGrade` object.
#' @param method Character. One of `"deterministic"` or `"llm"`.
#' @param preferredMinWords Integer. Passed to deterministic scoring.
#' @param preferredMaxWords Integer. Passed to deterministic scoring.
#' @param fatalFlawCap Numeric. Passed to deterministic scoring.
#' @param passThreshold Numeric. Passed to deterministic scoring.
#' @param chat Optional chat provider object. Used for LLM scoring.
#' @param useCache Logical. Passed to LLM scoring.
#' @param showProgress Logical. Should progress messages be shown for LLM
#'   scoring?
#' @param verbose Logical. Passed to LLM scoring.
#' @param ... Additional arguments passed to the relevant scoring helper.
#'
#' @return A scored `wmfmGrade` object.
#' @export
score.wmfmGrade = function(
    x,
    method = c("deterministic", "llm"),
    preferredMinWords = 80L,
    preferredMaxWords = 220L,
    fatalFlawCap = 40,
    passThreshold = 65,
    chat = NULL,
    useCache = FALSE,
    showProgress = TRUE,
    verbose = FALSE,
    ...
) {

  if (!inherits(x, "wmfmGrade")) {
    stop("`x` must inherit from `wmfmGrade`.", call. = FALSE)
  }

  if (is.null(x$records$student)) {
    stop("`x` does not contain a student run record.", call. = FALSE)
  }

  method = match.arg(method)

  dimensionMetrics = c(
    "factualScore",
    "inferenceScore",
    "completenessScore",
    "clarityScore",
    "calibrationScore"
  )

  scoreOneRecordDeterministic = function(record) {
    df = as.data.frame(record, stringsAsFactors = FALSE)
    scored = scoreWmfmRunRecordsCore(
      runsDf = df,
      preferredMinWords = preferredMinWords,
      preferredMaxWords = preferredMaxWords,
      penaliseDuplicates = FALSE,
      fatalFlawCap = fatalFlawCap,
      passThreshold = passThreshold,
      ...
    )

    rownames(scored) = NULL
    scored
  }

  scoreOneRecordLlm = function(record, chat) {
    scoredRecord = scoreWmfmRunWithLlm(
      runRecord = record,
      chat = chat,
      useCache = useCache,
      verbose = verbose
    )

    out = as.data.frame(scoredRecord, stringsAsFactors = FALSE)
    out$primaryScoringMethod = "llm"
    out
  }

  deriveOverallFromDimensions = function(scoreDf, scoreScale) {
    availableMetrics = intersect(dimensionMetrics, names(scoreDf))

    if (length(availableMetrics) != length(dimensionMetrics)) {
      rawOverallScore = suppressWarnings(as.numeric(scoreDf$overallScore[1]))
      return(list(
        overallScore = rawOverallScore,
        mark = rawOverallScore / 100 * scoreScale,
        derivedFromDimensions = FALSE
      ))
    }

    dimensionValues = suppressWarnings(as.numeric(scoreDf[1, dimensionMetrics, drop = FALSE]))
    if (any(is.na(dimensionValues))) {
      rawOverallScore = suppressWarnings(as.numeric(scoreDf$overallScore[1]))
      return(list(
        overallScore = rawOverallScore,
        mark = rawOverallScore / 100 * scoreScale,
        derivedFromDimensions = FALSE
      ))
    }

    maxDimensionTotal = 2 * length(dimensionMetrics)
    derivedOverallScore = sum(dimensionValues) / maxDimensionTotal * 100

    list(
      overallScore = derivedOverallScore,
      mark = derivedOverallScore / 100 * scoreScale,
      derivedFromDimensions = TRUE
    )
  }

  if (identical(method, "llm") && is.null(chat)) {
    chat = tryCatch(
      getChatProvider(),
      error = function(e) {
        stop(
          "Could not get a chat provider for LLM scoring. Details: ",
          conditionMessage(e),
          call. = FALSE
        )
      }
    )
  }

  methodScore = list(
    student = NULL,
    modelAnswer = NULL,
    metricSummary = NULL,
    mark = NA_real_,
    overallScore = NA_real_,
    rawOverallScore = NA_real_,
    overallDerivedFromDimensions = FALSE
  )

  studentScoreDf = NULL
  modelAnswerScoreDf = NULL

  if (identical(method, "deterministic")) {
    studentScoreDf = scoreOneRecordDeterministic(x$records$student)

    if (!is.null(x$records$modelAnswer)) {
      modelAnswerScoreDf = scoreOneRecordDeterministic(x$records$modelAnswer)
    }
  } else {
    if (isTRUE(showProgress)) {
      message("Scoring student explanation with LLM...")
    }

    studentScoreDf = scoreOneRecordLlm(x$records$student, chat = chat)

    if (!is.null(x$records$modelAnswer)) {
      if (isTRUE(showProgress)) {
        message("Scoring supplied model answer with LLM...")
      }

      modelAnswerScoreDf = scoreOneRecordLlm(x$records$modelAnswer, chat = chat)
    }
  }

  feedback = summariseWmfmGradeLosses(
    studentScoreDf = studentScoreDf,
    modelAnswerScoreDf = modelAnswerScoreDf,
    method = method
  )

  rawOverallScore = suppressWarnings(as.numeric(studentScoreDf$overallScore[1]))

  if (identical(method, "llm")) {
    overallInfo = deriveOverallFromDimensions(
      scoreDf = studentScoreDf,
      scoreScale = x$scoreScale
    )
    overallScore = overallInfo$overallScore
    mark = overallInfo$mark
    overallDerivedFromDimensions = isTRUE(overallInfo$derivedFromDimensions)
  } else {
    overallScore = rawOverallScore
    mark = overallScore / 100 * x$scoreScale
    overallDerivedFromDimensions = FALSE
  }

  methodScore$student = studentScoreDf
  methodScore$modelAnswer = modelAnswerScoreDf
  methodScore$metricSummary = feedback$metricSummary
  methodScore$overallScore = overallScore
  methodScore$mark = mark
  methodScore$rawOverallScore = rawOverallScore
  methodScore$overallDerivedFromDimensions = overallDerivedFromDimensions

  x$scores$byMethod[[method]] = methodScore
  x$feedback$byMethod[[method]] = list(
    whereMarksLost = feedback$whereMarksLost,
    strengths = feedback$strengths,
    weaknesses = feedback$weaknesses,
    missingElements = feedback$missingElements,
    advisoryFlags = feedback$advisoryFlags,
    modelAnswerComparison = feedback$modelAnswerComparison
  )

  x$scores$student = methodScore$student
  x$scores$modelAnswer = methodScore$modelAnswer
  x$scores$metricSummary = methodScore$metricSummary
  x$scores$overallScore = methodScore$overallScore
  x$scores$mark = methodScore$mark

  x$feedback$whereMarksLost = feedback$whereMarksLost
  x$feedback$strengths = feedback$strengths
  x$feedback$weaknesses = feedback$weaknesses
  x$feedback$missingElements = feedback$missingElements
  x$feedback$advisoryFlags = feedback$advisoryFlags
  x$feedback$modelAnswerComparison = feedback$modelAnswerComparison

  scoredMethods = unique(c(x$meta$scoredMethods %||% character(0), method))

  x$meta$scored = TRUE
  x$meta$scoredAt = as.character(Sys.time())
  x$meta$scoredMethods = scoredMethods
  x$meta$lastScoredMethod = method
  x$meta$preferredMinWords = preferredMinWords
  x$meta$preferredMaxWords = preferredMaxWords
  x$meta$fatalFlawCap = fatalFlawCap
  x$meta$passThreshold = passThreshold

  if (identical(method, "llm")) {
    x$meta$llmModel = safeWmfmScalar(class(chat)[1], naString = "")
    x$meta$llmUseCache = useCache
  }

  x
}
