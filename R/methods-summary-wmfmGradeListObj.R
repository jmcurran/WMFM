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

  gradeNames = names(object$grades)

  extractMark = function(gradeObj, method) {
    methods = names(gradeObj$scores$byMethod %||% list())

    if (!method %in% methods) {
      return(NA_real_)
    }

    suppressWarnings(as.numeric(gradeObj$scores$byMethod[[method]]$mark)[1])
  }

  extractLastMethod = function(gradeObj) {
    method = gradeObj$meta$lastScoredMethod %||% NA_character_

    if (length(method) != 1 || is.na(method) || !nzchar(method)) {
      methods = names(gradeObj$scores$byMethod %||% list())
      if (length(methods) < 1) {
        return(NA_character_)
      }
      method = methods[length(methods)]
    }

    method
  }

  summariseVec = function(values) {
    values = values[!is.na(values)]

    if (length(values) < 1) {
      return(NULL)
    }

    list(
      mean = mean(values),
      median = stats::median(values),
      min = min(values),
      max = max(values),
      n = length(values)
    )
  }

  deterministicMarks = vapply(object$grades, extractMark, numeric(1), method = "deterministic")
  llmMarks = vapply(object$grades, extractMark, numeric(1), method = "llm")
  lastMethods = vapply(object$grades, extractLastMethod, character(1))

  latestMarks = vapply(seq_along(object$grades), function(i) {
    if (is.na(lastMethods[i])) {
      return(NA_real_)
    }
    extractMark(object$grades[[i]], lastMethods[i])
  }, numeric(1))

  availableMethods = unique(unlist(lapply(object$grades, function(gradeObj) {
    names(gradeObj$scores$byMethod %||% list())
  })))

  scoredByMethod = stats::setNames(
    lapply(c("deterministic", "llm"), function(method) {
      values = vapply(object$grades, function(gradeObj) {
        method %in% names(gradeObj$scores$byMethod %||% list())
      }, logical(1))

      list(
        n = sum(values),
        complete = all(values)
      )
    }),
    c("deterministic", "llm")
  )

  markByExplanation = data.frame(
    explanation = gradeNames,
    lastMethod = lastMethods,
    latestMark = latestMarks,
    deterministicMark = deterministicMarks,
    llmMark = llmMarks,
    scoreScale = vapply(object$grades, function(gradeObj) {
      suppressWarnings(as.numeric(gradeObj$scoreScale)[1])
    }, numeric(1)),
    stringsAsFactors = FALSE
  )

  out = list(
    nExplanations = length(object$grades),
    method = object$meta$method,
    nLlm = object$meta$nLlm,
    totalLlmCalls = object$meta$totalLlmCalls,
    elapsedSeconds = object$meta$elapsedSeconds,
    meanSecondsPerExplanation = object$meta$meanSecondsPerExplanation,
    meanSecondsPerLlmCall = object$meta$meanSecondsPerLlmCall,
    availableMethods = availableMethods,
    scoredByMethod = scoredByMethod,
    markByExplanation = markByExplanation,
    latestMark = summariseVec(latestMarks),
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
