#' Summarise a WMFM grade list object
#'
#' @param object A `wmfmGradeListObj` object.
#' @param ... Unused.
#'
#' @return An object of class `summary.wmfmGradeListObj`.
#' @export
summary.wmfmGradeListObj = function(
    object,
    ...
) {

  if (!inherits(object, "wmfmGradeListObj")) {
    stop("`object` must inherit from `wmfmGradeListObj`.", call. = FALSE)
  }

  extractMark = function(gradeObj, method) {
    methods = names(gradeObj$scores$byMethod %||% list())

    if (!method %in% methods) {
      return(NA_real_)
    }

    suppressWarnings(as.numeric(gradeObj$scores$byMethod[[method]]$mark)[1])
  }

  deterministicMarks = vapply(object$grades, extractMark, numeric(1), method = "deterministic")
  llmMarks = vapply(object$grades, extractMark, numeric(1), method = "llm")

  summariseVec = function(values) {
    values = values[!is.na(values)]

    if (length(values) < 1) {
      return(NULL)
    }

    list(
      mean = mean(values),
      median = stats::median(values),
      min = min(values),
      max = max(values)
    )
  }

  out = list(
    nExplanations = length(object$grades),
    method = object$meta$method,
    nLlm = object$meta$nLlm,
    totalLlmCalls = object$meta$totalLlmCalls,
    elapsedSeconds = object$meta$elapsedSeconds,
    meanSecondsPerExplanation = object$meta$meanSecondsPerExplanation,
    meanSecondsPerLlmCall = object$meta$meanSecondsPerLlmCall,
    deterministicMark = summariseVec(deterministicMarks),
    llmMark = summariseVec(llmMarks),
    meanDifference = if (all(is.na(deterministicMarks)) || all(is.na(llmMarks))) {
      NA_real_
    } else {
      mean(llmMarks - deterministicMarks, na.rm = TRUE)
    },
    source = object
  )

  class(out) = c("summary.wmfmGradeListObj", "list")
  out
}
