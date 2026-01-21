#' Assemble a WMFM prompt from common pieces
#'
#' @param context "summary" or "contrast".
#' @param contextPayload Character scalar giving the context-specific data block
#'   (model summary payload or contrast payload).
#' @param scaleRules Optional character scalar with additional scale-specific
#'   rules (e.g., additive vs multiplicative vs odds multipliers).
#'
#' @return A character scalar prompt.
#' @keywords internal
composeWmfmPrompt = function(context = c("summary", "contrast"),
                             contextPayload,
                             scaleRules = NULL) {

  context = match.arg(context)

  languageContract = buildWmfmLanguageContractText(context = context)

  parts = c(
    languageContract,
    if (!is.null(scaleRules) && nzchar(scaleRules)) scaleRules else NULL,
    "Context:",
    contextPayload,
    "",
    "Write the explanation now."
  )

  paste(parts, collapse = "\n")
}
