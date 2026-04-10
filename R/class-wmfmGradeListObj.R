#' Create a WMFM grade list object
#'
#' Creates a classed `wmfmGradeListObj` object that stores multiple
#' `wmfmGrade` objects together with batch-level metadata.
#'
#' @param grades Named list of `wmfmGrade` objects.
#' @param model A `wmfmModel` object.
#' @param inputs Named list describing the supplied explanations.
#' @param meta Optional named list of metadata.
#'
#' @return An object of class `wmfmGradeListObj`.
#' @export
newWmfmGradeListObj = function(
    grades,
    model,
    inputs,
    meta = list()
) {

  if (!inherits(model, "wmfmModel")) {
    stop("`model` must inherit from `wmfmModel`.", call. = FALSE)
  }

  if (!is.list(grades) || length(grades) < 1) {
    stop("`grades` must be a non-empty list.", call. = FALSE)
  }

  if (!all(vapply(grades, inherits, logical(1), what = "wmfmGrade"))) {
    stop("All elements of `grades` must inherit from `wmfmGrade`.", call. = FALSE)
  }

  if (is.null(names(grades)) || any(!nzchar(names(grades)))) {
    stop("`grades` must be a named list.", call. = FALSE)
  }

  if (!is.list(inputs)) {
    stop("`inputs` must be a list.", call. = FALSE)
  }

  if (!is.list(meta)) {
    stop("`meta` must be a list.", call. = FALSE)
  }

  out = list(
    model = model,
    inputs = inputs,
    grades = grades,
    meta = utils::modifyList(
      list(
        createdAt = as.character(Sys.time()),
        nExplanations = length(grades),
        method = NA_character_,
        nLlm = NA_integer_,
        totalLlmCalls = NA_integer_,
        elapsedSeconds = NA_real_,
        meanSecondsPerExplanation = NA_real_,
        meanSecondsPerLlmCall = NA_real_,
        confirmLargeLlmJob = FALSE,
        maxLlmJobsWithoutConfirmation = 20L,
        scored = FALSE,
        scoredMethods = character(0),
        lastScoredMethod = NA_character_
      ),
      meta
    )
  )

  class(out) = c("wmfmGradeListObj", "list")
  out
}
