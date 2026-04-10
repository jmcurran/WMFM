#' Generate language rules for interpreting a linear contrast
#'
#' This helper constructs a short, deterministic set of language constraints
#' to guide natural-language interpretation of a contrast. The rules are
#' designed to enforce wording that is consistent with the model scale
#' (identity, log, logit, etc.) and any transformation applied to the response
#' in an \code{lm()} model.
#'
#' The returned text is intended to be embedded verbatim in an LLM prompt to
#' prevent inconsistent phrasing (e.g., additive vs multiplicative language).
#'
#' @param isGlm Logical. \code{TRUE} if the fitted model inherits from
#'   \code{"glm"}, \code{FALSE} for \code{lm()} models.
#'
#' @param effectiveScale Character string giving the interpretation scale.
#'   For GLMs this should typically be the model link (e.g. \code{"identity"},
#'   \code{"log"}, \code{"logit"}). For \code{lm()} models this is usually
#'   \code{"identity"} or \code{"other"}.
#'
#' @param respTransform Character string describing the detected response
#'   transformation for \code{lm()} models. Expected values include
#'   \code{"none"}, \code{"log"}, \code{"log10"}, \code{"log1p"},
#'   \code{"sqrt"}, \code{"inverse"}, or \code{"unknown"}.
#'
#' @param nounPhrase Optional character string naming the outcome in
#'   student-facing language (e.g. \code{"oyster count"}). If \code{NULL},
#'   a generic phrase ("the outcome") is used.
#'
#' @return A single character string containing bullet-point language rules.
#'   The text is suitable for direct inclusion in an LLM prompt.
#'
#' @examples
#' buildScalePhrasingRules(
#'   isGlm = FALSE,
#'   effectiveScale = "identity",
#'   respTransform = "none"
#' )
#'
#' buildScalePhrasingRules(
#'   isGlm = TRUE,
#'   effectiveScale = "logit",
#'   respTransform = "none",
#'   nounPhrase = "disease status"
#' )
#' @export
buildScalePhrasingRules = function(
    isGlm,
    effectiveScale,
    respTransform,
    nounPhrase = NULL
) {

  nounPhrase = nounPhrase %||% "the outcome"

  # ---- GLM cases: use the link function ----
  if (isGlm) {

    if (identical(effectiveScale, "log")) {
      return(paste0(
        "Language rules:\n",
        "- This contrast represents a multiplicative effect.\n",
        "- Use ONLY 'times higher' or 'times lower' wording.\n",
        "- Do NOT describe this as an additive difference.\n"
      ))
    }

    if (identical(effectiveScale, "logit")) {
      return(paste0(
        "Language rules:\n",
        "- This contrast describes a multiplicative change in the odds.\n",
        "- Use ONLY wording of the form \"the odds are ... times ...\".\n",
        "- Do NOT use the term \"odds ratio\".\n",
        "- Do NOT talk about probabilities, percentages, or percentage points.\n"
      ))
    }

    # Fallback for other GLM links
    return(paste0(
      "Language rules:\n",
      "- Use higher/lower wording appropriate to the model scale.\n",
      "- Do NOT invent a back-transformation.\n"
    ))
  }

  # ---- lm() cases: use detected response transformation ----
  if (identical(respTransform, "none")) {
    return(paste0(
      "Language rules:\n",
      "- Treat this as an additive difference in means.\n",
      "- Use 'higher' or 'lower by' wording (NOT 'times').\n"
    ))
  }

  if (respTransform %in% c("log", "log10")) {
    return(paste0(
      "Language rules:\n",
      "- Interpret the contrast on the original scale as a ratio.\n",
      "- Use ONLY 'times higher' or 'times lower' wording.\n",
      "- You may refer to typical means of ", nounPhrase, ".\n"
    ))
  }

  if (identical(respTransform, "log1p")) {
    return(paste0(
      "Language rules:\n",
      "- This is an additive difference on the log(1 + y) scale.\n",
      "- Use 'higher' or 'lower on the log(1 + y) scale' wording.\n",
      "- Do NOT use 'times' language.\n"
    ))
  }

  if (identical(respTransform, "sqrt")) {
    return(paste0(
      "Language rules:\n",
      "- This is an additive difference on the square-root scale.\n",
      "- Use 'higher' or 'lower on the square-root scale' wording.\n"
    ))
  }

  if (identical(respTransform, "inverse")) {
    return(paste0(
      "Language rules:\n",
      "- This is an additive difference on an inverse (1/y) scale.\n",
      "- Use 'higher' or 'lower on the inverse scale' wording.\n"
    ))
  }

  # ---- Fallback ----
  paste0(
    "Language rules:\n",
    "- This contrast is on a transformed scale.\n",
    "- Use 'higher' or 'lower on the transformed scale' wording.\n",
    "- Do NOT invent a back-transformation.\n"
  )
}
