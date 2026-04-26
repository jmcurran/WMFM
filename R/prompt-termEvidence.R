#' Build optional term-evidence prompt block
#'
#' The explanation prompt now uses deterministic formatted quantities for
#' student-facing model evidence. Raw coefficient and confidence-interval
#' tables are retained in the explanation audit rather than being inserted into
#' the language-model prompt. This helper preserves the prompt-construction
#' contract for callers that still assemble an optional term-evidence block.
#'
#' @param model A fitted model object.
#' @param mf Optional model frame.
#'
#' @return An empty character scalar.
#' @keywords internal
buildLmTermEvidencePromptBlock = function(model, mf = NULL) {
  ""
}
