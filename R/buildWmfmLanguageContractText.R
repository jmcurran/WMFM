#' Build the shared language contract for WMFM explanations
#'
#' Returns a single block of instructions that defines the allowed statistical
#' language across both overall model summaries and contrasts. This keeps
#' phrasing and uncertainty language consistent, so contrasts read like a
#' zoomed-in view of the same model.
#'
#' @return A character scalar containing the shared language contract text.
#' @keywords internal
buildWmfmLanguageContractText = function() {

  contract = paste(
    "Guidelines:",
    "- If dataset documentation is provided, summarise it in at most one short clause.",
    "- Briefly explain what the response represents and what the predictors represent, defining predictors in-line without bullet points.",
    "",
    "- Describe the direction of each important effect (positive/negative).",
    "- Use the rounded coefficients to give an intuitive sense of size (e.g. \"for each 1-unit increase in Test, the exam score increases by about 3.8 points\").",
    "",
    "- Do not mention standard errors, t-values, z-values, or p-values explicitly.",
    "",
    "- Do not describe the statistical model, link function, or transformations.",
    "  Focus only on what the model implies about the response in real-world terms.",
    "- Do not name the type of regression or statistical model.",
    "",
    "- Effects and confidence intervals are on the scale of the fitted model (i.e. after any transformation of the response).",
    "- Do not assume effects are on the original response scale unless explicitly stated.",
    "- Confidence intervals should be described on the same scale as the effect.",
    "- Never report effects or confidence intervals on more than one scale.",
    "",
    "- When confidence intervals are provided, include them for each key effect.",
    "- Present the point estimate first, then describe uncertainty using the confidence interval.",
    "- Explain confidence intervals in plain language without assigning probability to the parameter (e.g. \"values between L and U are consistent with the data\" or \"effects in this range cannot be ruled out by the data\").",
    "- If a confidence interval includes 0, say there is weak or uncertain evidence for a clear effect, because the data are consistent with anything from a small decrease to a small increase. (Do not mention p-values.)",
    "",
    "- For a continuous predictor, use this template:",
    "  \"For each 1-unit increase in X, the expected response changes by about B units (95% CI: L to U units), holding other variables constant.\"",
    "",
    "- For a factor predictor (categorical), use this template:",
    "  \"Compared with the reference group (REF), group G is about B units higher/lower on average (95% CI: L to U units), holding other variables constant.\"",
    "",
    "- For binomial models with a logit link:",
    "  Describe effects in terms of higher or lower odds of the event.",
    "  Do not describe effects as changes in probability unless explicitly instructed.",
    "  Do not mention the intercept at all for logit models unless predictors are centred.",
    "  Do not use the term \"odds ratio\".",
    "  Describe effects directly as odds being multiplied by a factor.",
    "",
    "- Do not convert odds to percentages.",
    "- Avoid phrases like \"1 in N\" unless explicitly clarifying that this refers to odds (e.g. \"1 success for every N failures\").",
    "",
    "- Comparisons between groups should be described using odds being multiplied by a factor (e.g. \"the odds are about K times higher\"), not by manipulating the intercept.",
    "",
    "- Mention the intercept only if it corresponds to a realistic and meaningful baseline.",
    "- If the intercept is mentioned for a logit model, describe it only as baseline odds (or a \"1 in N\" equivalent) for the reference group.",
    "",
    "- For count outcomes:",
    "  Describe effects as expected counts being higher or lower by a certain factor, without mentioning logarithms, links, or transformations.",
    "  Use wording like: \"Panels at site X are expected to have about K times as many oysters as panels at the reference site (95% CI: L to U times).\"",
    "",
    "- If the response is log- or log1p-transformed:",
    "  Explain effects as multiplicative (percentage) changes in the original outcome.",
    "  Use approximate percentage language rather than exact back-transformations.",
    "",
    "- Use units in the narrative when they are obvious from the context (e.g. \"points\"). If units are not clear, just say \"units\".",
    "",
    "- If there are interactions, briefly describe how one effect depends on another variable.",
    "",
    "- Only discuss overall model fit when a standard R-squared is provided.",
    "- If overall model fit is described, mention it briefly at the end of the explanation, after describing the individual effects.",
    "- When describing overall model fit, use wording like: \"Overall, the model explains about X% of the variation in the response.\"",
    "",
    "- Do not use section headings or bold labels (e.g. \"Intercept\", \"Effect of ...\").",
    "- Write as a single short paragraph (or at most two), not a bulleted list.",
    sep = "\n"
  )

  contract
}
