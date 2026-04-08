#' Grade one or more explanations against a WMFM model
#'
#' Creates either a `wmfmGrade` object for a single explanation or a
#' `wmfmGradeListObj` when multiple explanations are supplied. By default the
#' returned object is immediately scored using either the deterministic or LLM
#' WMFM grading rubric.
#'
#' @param x A `wmfmModel` object, typically created by `runModel()`.
#' @param explanation Character vector, character scalar, or list of character
#'   scalars giving the explanation(s) to grade.
#' @param modelAnswer Optional character scalar giving a reference answer.
#' @param method Character. One of `"deterministic"`, `"llm"`, or `"both"`.
#' @param autoScore Logical. Should the returned object be scored immediately?
#'   Defaults to `TRUE`.
#' @param scoreScale Numeric scalar giving the displayed mark scale. Defaults to
#'   `10`.
#' @param nLlm Integer. Number of repeated LLM gradings per explanation when
#'   `method` includes `"llm"`.
#' @param confirmLargeLlmJob Logical. Set to `TRUE` to allow large LLM grading
#'   requests.
#' @param maxLlmJobsWithoutConfirmation Integer. Maximum number of LLM grading
#'   calls allowed without explicit confirmation.
#' @param ... Additional arguments passed to `score()` when `autoScore = TRUE`.
#'
#' @return An object of class `wmfmGrade` or `wmfmGradeListObj`.
#' @export
grade.wmfmModel = function(
    x,
    explanation,
    modelAnswer = NULL,
    method = c("deterministic", "llm", "both"),
    autoScore = TRUE,
    scoreScale = 10,
    nLlm = 1L,
    confirmLargeLlmJob = FALSE,
    maxLlmJobsWithoutConfirmation = 20L,
    ...
) {

  dots = list(...)

  if ("score" %in% names(dots)) {
    legacyScore = dots[["score"]]

    if (!is.logical(legacyScore) || length(legacyScore) != 1 || is.na(legacyScore)) {
      stop(
        "`score` must be TRUE or FALSE when used as a legacy alias for `autoScore`.",
        call. = FALSE
      )
    }

    autoScore = legacyScore
    dots[["score"]] = NULL
  }

  if (!inherits(x, "wmfmModel")) {
    stop("`x` must inherit from `wmfmModel`.", call. = FALSE)
  }

  if (!is.null(modelAnswer) &&
      (!is.character(modelAnswer) || length(modelAnswer) != 1 || is.na(modelAnswer))) {
    stop("`modelAnswer` must be NULL or a single non-missing character string.", call. = FALSE)
  }

  if (!is.logical(autoScore) || length(autoScore) != 1 || is.na(autoScore)) {
    stop("`autoScore` must be TRUE or FALSE.", call. = FALSE)
  }

  method = match.arg(method)
  nLlm = as.integer(nLlm)[1]

  if (is.na(nLlm) || nLlm < 1L) {
    stop("`nLlm` must be an integer greater than or equal to 1.", call. = FALSE)
  }

  explanationVec = normalizeWmfmExplanations(explanation)
  nExplanations = length(explanationVec)

  totalLlmCalls = computeWmfmLlmJobCount(
    nExplanations = nExplanations,
    method = method,
    nLlm = nLlm
  )

  enforceWmfmLlmJobGuard(
    totalLlmCalls = totalLlmCalls,
    nExplanations = nExplanations,
    nLlm = nLlm,
    confirmLargeLlmJob = confirmLargeLlmJob,
    maxLlmJobsWithoutConfirmation = maxLlmJobsWithoutConfirmation
  )

  buildOneGrade = function(explanationText, explanationName, runId) {
    studentRecord = buildWmfmGradeRunRecord(
      x = x,
      explanation = explanationText,
      runId = runId,
      answerRole = "student"
    )

    modelAnswerRecord = NULL

    if (!is.null(modelAnswer)) {
      modelAnswerRecord = buildWmfmGradeRunRecord(
        x = x,
        explanation = modelAnswer,
        runId = runId + 100000L,
        answerRole = "modelAnswer"
      )
    }

    newWmfmGrade(
      x = x,
      explanation = explanationText,
      modelAnswer = modelAnswer,
      scoreScale = scoreScale,
      records = list(
        student = studentRecord,
        modelAnswer = modelAnswerRecord
      ),
      meta = list(
        explanationName = explanationName
      )
    )
  }

  gradeList = lapply(seq_along(explanationVec), function(i) {
    buildOneGrade(
      explanationText = explanationVec[[i]],
      explanationName = names(explanationVec)[i],
      runId = i
    )
  })
  names(gradeList) = names(explanationVec)

  if (length(gradeList) == 1L) {
    out = gradeList[[1]]

    if (isTRUE(autoScore)) {
      if (identical(method, "both")) {
        out = do.call(score, c(list(x = out, method = "deterministic"), dots))
        out = do.call(score, c(list(x = out, method = "llm", nLlm = nLlm), dots))
      } else {
        out = do.call(score, c(list(x = out, method = method, nLlm = nLlm), dots))
      }
    }

    return(out)
  }

  out = newWmfmGradeListObj(
    grades = gradeList,
    model = x,
    inputs = list(
      explanations = explanationVec,
      modelAnswer = modelAnswer,
      names = names(explanationVec)
    ),
    meta = list(
      method = method,
      nLlm = nLlm,
      totalLlmCalls = totalLlmCalls,
      confirmLargeLlmJob = confirmLargeLlmJob,
      maxLlmJobsWithoutConfirmation = maxLlmJobsWithoutConfirmation
    )
  )

  if (isTRUE(autoScore)) {
    out = do.call(
      score,
      c(
        list(
          x = out,
          method = method,
          nLlm = nLlm,
          confirmLargeLlmJob = confirmLargeLlmJob,
          maxLlmJobsWithoutConfirmation = maxLlmJobsWithoutConfirmation
        ),
        dots
      )
    )
  }

  out
}
