#' Compute the number of WMFM LLM grading jobs
#'
#' Internal helper for estimating how many LLM grading calls a request would
#' make.
#'
#' @param nExplanations Integer. Number of explanations.
#' @param method Character. One of `"deterministic"`, `"llm"`, or `"both"`.
#' @param nLlm Integer. Number of repeated LLM gradings per explanation.
#'
#' @return Integer scalar.
#' @keywords internal
computeWmfmLlmJobCount = function(
    nExplanations,
    method,
    nLlm = 1L
) {

  nExplanations = as.integer(nExplanations)[1]
  nLlm = as.integer(nLlm)[1]
  method = match.arg(method, c("deterministic", "llm", "both"))

  if (is.na(nExplanations) || nExplanations < 0L) {
    stop("`nExplanations` must be a non-negative integer.", call. = FALSE)
  }

  if (is.na(nLlm) || nLlm < 1L) {
    stop("`nLlm` must be an integer greater than or equal to 1.", call. = FALSE)
  }

  if (identical(method, "deterministic")) {
    return(0L)
  }

  as.integer(nExplanations * nLlm)
}
