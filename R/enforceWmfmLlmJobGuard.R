#' Enforce the WMFM LLM grading job guard
#'
#' Internal helper that prevents unexpectedly large LLM grading requests unless
#' the user explicitly opts in.
#'
#' @param totalLlmCalls Integer. Total planned LLM calls.
#' @param nExplanations Integer. Number of explanations.
#' @param nLlm Integer. Number of repeated LLM gradings per explanation.
#' @param confirmLargeLlmJob Logical. Whether to allow large requests.
#' @param maxLlmJobsWithoutConfirmation Integer. Maximum number of LLM calls
#'   allowed without explicit confirmation.
#'
#' @return Invisibly returns `TRUE` when the request is allowed.
#' @keywords internal
enforceWmfmLlmJobGuard = function(
    totalLlmCalls,
    nExplanations,
    nLlm,
    confirmLargeLlmJob = FALSE,
    maxLlmJobsWithoutConfirmation = 20L
) {

  totalLlmCalls = as.integer(totalLlmCalls)[1]
  nExplanations = as.integer(nExplanations)[1]
  nLlm = as.integer(nLlm)[1]
  maxLlmJobsWithoutConfirmation = as.integer(maxLlmJobsWithoutConfirmation)[1]

  if (is.na(totalLlmCalls) || totalLlmCalls < 0L) {
    stop("`totalLlmCalls` must be a non-negative integer.", call. = FALSE)
  }

  if (!is.logical(confirmLargeLlmJob) || length(confirmLargeLlmJob) != 1 || is.na(confirmLargeLlmJob)) {
    stop("`confirmLargeLlmJob` must be TRUE or FALSE.", call. = FALSE)
  }

  if (is.na(maxLlmJobsWithoutConfirmation) || maxLlmJobsWithoutConfirmation < 0L) {
    stop(
      "`maxLlmJobsWithoutConfirmation` must be a non-negative integer.",
      call. = FALSE
    )
  }

  if (totalLlmCalls > maxLlmJobsWithoutConfirmation && !isTRUE(confirmLargeLlmJob)) {
    stop(
      "This grading request would make ", totalLlmCalls, " LLM calls (",
      nExplanations, " explanation", if (nExplanations == 1L) "" else "s",
      " x ", nLlm, " repeated LLM grading",
      if (nLlm == 1L) "" else "s",
      "). Set `confirmLargeLlmJob = TRUE` to proceed.",
      call. = FALSE
    )
  }

  invisible(TRUE)
}
