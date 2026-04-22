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
    "- Use rounded, interpreted effect sizes to give an intuitive sense of size.",
    "- Avoid unnecessary numerical precision; round and summarise numbers where possible.",
    "- Use words for integer counts from zero to ten (e.g. \"three\", \"eight\").",
    "",
    "- STRICT NUMERIC FORMATTING RULES (MANDATORY):",
    "- All decimal values MUST be written using numerals.",
    "  - Correct: \"3.5\"",
    "  - Incorrect: \"three and a half\", \"three-point-five\"",
    "- Percentages MUST always be written using numerals.",
    "  - Correct: \"62%\" or \"62 percent\"",
    "  - Incorrect: \"sixty-two percent\"",
    "- These rules override all other stylistic preferences.",
    "- Never spell out numbers when:",
    "  - the value is not an integer, OR",
    "  - the value represents a percentage.",
    "- Use numerals for all non-integer quantities, including decimals, percentages, confidence-interval bounds, effect sizes, and non-integer proportions.",
    "- When a quantity includes a decimal point, always use numerals, even if the value is less than ten.",
    "- Do not express decimal quantities in words or mixed verbal forms (e.g. do not write \"three-point-five\" or \"three and a half\"); always use numerals such as \"3.5\".",
    "- Keep numeric expressions plain, natural, and statistically conventional.",
    "- Do not use markdown emphasis for numbers (e.g. do not use bold or italics).",
    "- Prefer percentages when describing proportional changes.",
    "- Always express percentages using numerals (e.g. \"62%\" or \"62 percent\"), even for values below ten.",
    "- Write percentages without a space before the percent sign (e.g. \"62%\", not \"62 %\").",
    "- Do not use verbal fractions or fraction-like wording for non-integer proportions (e.g. \"one-fifth\", \"one fifth\", \"three quarters\").",
    "- Express non-integer proportions numerically, preferably as percentages (e.g. \"20%\", \"75%\").",
    "- If a number violates these rules, rewrite it using numerals.",
    "- Do not create hybrid number forms such as \"twenty-1\", \"one-five\", or \"four-2\"; rewrite them as plain numerals such as \"21\", \"15\", or \"42\".",
    "",
    "- Do not mention standard errors, t-values, z-values, or p-values explicitly.",
    "",
    "- Do not name the model family, regression type, link function, or transformation in the final explanation unless this is necessary to explain the interpretation scale.",
    "- Do not refer to model terms such as \"interaction\", \"main effect\", \"coefficient\", or \"parameter\".",
    "- When effects differ between groups, describe the difference directly in plain language (e.g. \"the decline is steeper in Washington\") rather than referring to model structure.",
    "- Never write raw transformation expressions such as exp(-1.56), exp(0.8), or log-scale coefficients in the final explanation.",
    "- Always convert them to the interpreted scale before writing.",
    "",
    "- Report effects and confidence intervals on a single interpretation scale only.",
    "- For linear models, report effects on the response scale.",
    "- For binomial models with a logit link, convert coefficients and confidence intervals to odds multipliers before explaining them.",
    "- For count models with a log link, convert coefficients and confidence intervals to expected-count multipliers before explaining them.",
    "- Do not leave transformed quantities as expressions such as exp(x); always evaluate them numerically.",
    "- Round interpreted effects to about two or three significant digits.",
    "- When describing expected values, always include a clear noun such as \"expected number\", \"expected count\", or \"expected value\" (e.g. \"expected number of oysters\").",
    "",
    "- When confidence intervals are provided, use them to support a clear conclusion about the effect.",
    "- Do not just report the interval; explain what the range implies about the size and direction of the effect.",
    "- Where possible, interpret the lower and/or upper end of the interval to reinforce the conclusion (e.g. \"even at the lower end, the increase is still substantial\").",
    "- Each key effect should end with a short concluding statement about what the data suggest or are consistent with, stated in plain language.",
    "- Keep the inferential register cautious and evidence-based.",
    "- You may use plain-language evidence phrases such as \"the data indicate\" or \"the results suggest\".",
    "- Do not say that the data \"confirm\", \"prove\", \"establish\", or \"demonstrate\" an effect.",
    "- Avoid phrasing that mimics hypothesis testing (e.g. \"significantly different\", \"reject\", \"different from zero\", or \"provides strong evidence for\").",
    "- Present the point estimate first, then describe uncertainty using the confidence interval.",
    "- Explain confidence intervals in plain language without assigning probability to the parameter (e.g. \"values between L and U are consistent with the data\" or \"effects in this range cannot be ruled out by the data\").",
    "- Do not interpret confidence intervals as hypothesis tests and do not describe them in terms of \"difference from zero\".",
    "- If a confidence interval includes 0, say there is weak or uncertain evidence for a clear effect, because the data are consistent with anything from a small decrease to a small increase.",
    "- For multiplicative effects on counts or odds, do not say a confidence interval lies below zero.",
    "- For multiplicative effects, describe the interval relative to the no-change value of 1, or say it corresponds to a decrease throughout the interval.",
    "- Do NOT replace \"95% confidence interval\" with phrases like \"plausible range\".",
    "- Do NOT do extra arithmetic in the explanation (e.g. do not multiply effects together or compute implied predicted counts like \"2.9 * 32.5 ~= 94\").",
    "- Avoid mathematical operator symbols such as \"*\", \"~=\", \"+\", or \"/\" in the narrative.",
    "- Do not introduce abbreviations for groups (e.g. \"GR\", \"PS1\") unless those abbreviations were explicitly provided in the context.",
    "- Do not repeat the full phrase \"95% confidence interval\" multiple times; you may say \"95% confidence interval\" once and then refer to \"the interval\" afterwards (but do not invent alternative terms like \"plausible range\").",
    "",
    "- For binomial models with a logit link:",
    "  Describe effects as multipliers of the odds.",
    "  Do not talk about probabilities.",
    "  Do not use the term \"odds ratio\".",
    "  When interpreting confidence intervals, describe them relative to the no-change value of 1 rather than saying they are above or below zero.",
    "",
    "- For count outcomes with a log link:",
    "  Convert coefficients and confidence intervals to multiplicative effects on the expected count.",
    "",
    "  When describing the size of the effect:",
    "  - If the multiplier is less than 1, express it as a percentage of the previous level.",
    "    Example: 0.21 -> \"about 21% of its previous level (a 79% decrease)\".",
    "  - If the multiplier is greater than 1, express it as a percentage increase.",
    "    Example: 1.35 -> \"about 35% higher\".",
    "",
    "  Prefer percentage descriptions rather than raw multiplicative factors.",
    "  Do not report expressions such as exp(x), and avoid writing raw factors like 0.21 unless necessary.",
    "  Do not describe non-integer multiplicative effects using verbal fractions such as \"one-fifth\"; use numeric percentage wording instead.",
    "  When interpreting confidence intervals, describe them as showing a decrease throughout the interval, or as staying below the no-change value of 1.",
    "",
    "- Only discuss overall model fit when a standard R-squared is provided (linear models only).",
    "- If overall model fit is described, mention it briefly at the end.",
    "",
    "- Do not use section headings or bold labels.",
    "- Write as a single short paragraph (or at most two), not a bulleted list.",
    "- Do not insert hyphens into ordinary noun phrases (e.g. write \"final exam\", \"summer school\", \"midterm test\" unless a hyphen is standard and necessary).",
    "",
    "- FINAL CHECK (MANDATORY):",
    "- Before finishing, scan the explanation and ensure:",
    "  - No decimal numbers are written in words.",
    "  - All percentages are written using numerals.",
    "  - No verbal fractions are used for non-integer proportions.",
    "  - Multiplicative confidence intervals are not described as being below zero.",
    "- If any violations are found, correct them.",
    sep = "\n"
  )

  paste(header, "", contractBody, sep = "\n")
}

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
