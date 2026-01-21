#' Build the shared language contract for WMFM explanations
#'
#' Central shared rules used by both overall model summaries and contrasts.
#' Only the opening context lines vary; the core rules stay identical.
#'
#' @param context One of "summary" or "contrast".
#'
#' @return A character scalar containing the shared language contract text.
#' @keywords internal
buildWmfmLanguageContractText = function(context = c("summary", "contrast")) {

  context = match.arg(context)

  header = if (identical(context, "summary")) {
    paste(
      "You are writing an overall explanation of a fitted model in plain language.",
      "Keep the explanation consistent with the rules below.",
      sep = "\n"
    )
  } else {
    paste(
      "You are writing an explanation of a model-based contrast in plain language.",
      "Treat the contrast as a zoomed-in view of the same fitted model, using the same language rules.",
      sep = "\n"
    )
  }

  contractBody = paste(
    "Guidelines:",
    "- If dataset documentation is provided, summarise it in at most one short clause.",
    "- Briefly explain what the response represents and what the predictors represent, defining predictors in-line without bullet points.",
    "",
    "- Start the explanation by describing the outcome and the main comparison in plain language.",
    "- Do NOT describe what the model does (e.g. avoid phrases like \"the model links\" or \"the model estimates\").",
    "- Do NOT begin with experimental design or data-collection mechanics unless they are essential for understanding the outcome.",
    "- If design details are mentioned, include them only briefly and after the outcome has been introduced.",
    "",
    "- Describe the direction of each important effect (positive/negative).",
    "- Use the rounded coefficients to give an intuitive sense of size.",
    "- Avoid unnecessary numerical precision; round and summarise numbers where possible.",
    "",
    "- Do not mention standard errors, t-values, z-values, or p-values explicitly.",
    "",
    "- Do not describe the statistical model, link function, or transformations.",
    "- Do not name the type of regression or statistical model.",
    "",
    "- Effects and confidence intervals are on the scale of the fitted model.",
    "- Never report effects or confidence intervals on more than one scale.",
    "",
    "- When confidence intervals are provided, include them for each key effect.",
    "- Present the point estimate first, then describe uncertainty using the confidence interval.",
    "- Explain confidence intervals in plain language without assigning probability to the parameter (e.g. \"values between L and U are consistent with the data\" or \"effects in this range cannot be ruled out by the data\").",
    "- If a confidence interval includes 0, say there is weak or uncertain evidence for a clear effect, because the data are consistent with anything from a small decrease to a small increase.",
    "- Do NOT replace \"95% confidence interval\" with phrases like \"plausible range\".",
    "- Do NOT do extra arithmetic in the explanation (e.g. do not multiply effects together or compute implied predicted counts like \"2.9 × 32.5 ≈ 94\").",
    "- Avoid mathematical operator symbols such as \"×\", \"≈\", \"+\", \"*\", or \"/\" in the narrative.",
    "- Do not introduce abbreviations for groups (e.g. \"GR\", \"PS1\") unless those abbreviations were explicitly provided in the context.",
    "- Do not repeat the full phrase \"95% confidence interval\" multiple times; you may say \"95% confidence interval\" once and then refer to \"the interval\" afterwards (but do not invent alternative terms like \"plausible range\").",
    "",
    "- For binomial models with a logit link:",
    "  Describe effects as multipliers of the odds.",
    "  Do not talk about probabilities.",
    "  Do not use the term \"odds ratio\".",
    "",
    "- For count outcomes:",
    "  Describe effects as expected counts being higher or lower by a certain factor.",
    "",
    "- Only discuss overall model fit when a standard R-squared is provided (linear models only).",
    "- If overall model fit is described, mention it briefly at the end.",
    "",
    "- Do not use section headings or bold labels.",
    "- Write as a single short paragraph (or at most two), not a bulleted list.",
    sep = "\n"
  )

  paste(header, "", contractBody, sep = "\n")
}
