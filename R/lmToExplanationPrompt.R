#' Build a prompt asking for a plain-language model explanation
#'
#' Given a fitted linear or generalised linear model, construct a prompt that
#' asks a language model to explain the results in clear, non-technical
#' language. The prompt includes basic model information, an R-squared or
#' pseudo R-squared, and the coefficient table.
#'
#' @param model A fitted model object, typically of class \code{"lm"} or
#'   \code{"glm"}.
#'
#' @return A character scalar containing the prompt text to send to the
#'   language model for a narrative explanation.
#' @keywords internal
lmToExplanationPrompt = function(model) {
  modelFrame = model.frame(model)
  response = names(modelFrame)[1]
  coefTable = coef(summary(model))
  coefText = paste(capture.output(print(round(coefTable, 4))), collapse = "\n")

  ci = confint(model)
  ci = round(ci, 4)
  ciText = paste(capture.output(print(ci)), collapse = "\n")

  n = nrow(modelFrame)
  if (inherits(model, "glm")) {
    fam = model$family$family
    link = model$family$link
    modelSummary = summary(model)
    r2Text = "" ## R^2 meaningless here. Pseudo R^2 is unhelpful
    modelDesc = glue(
      "This is a generalised linear model with {fam} family and {link} link."
    )
  } else {
    modelSummary = summary(model)
    r2 = round(modelSummary$r.squared, 3)
    r2Text = glue("Approximate proportion of variation explained by the model: {r2}")
    modelDesc = "This is a linear regression model with Gaussian errors and identity link."
    fam = "gaussian"
    link = "identity"
  }

  outcomeDesc = if (inherits(model, "glm") && fam == "binomial") {
    "The outcome is binary (0/1), so the model describes how the probability of '1' changes."
  } else if (inherits(model, "glm") && fam == "poisson") {
    "The outcome is a non-negative count, so the model describes how the expected count changes."
  } else {
    "The outcome is continuous, so the model describes how the mean response changes."
  }

  # Optional: extra context from dataset documentation (e.g. s20x)
  dsDoc = attr(model, "wmfm_dataset_doc", exact = TRUE)
  dsName = attr(model, "wmfm_dataset_name", exact = TRUE)

  if (!is.null(dsDoc)) {
    # Truncate to something reasonable so we do not blow the context window
    dsLines = strsplit(dsDoc, "\n", fixed = TRUE)[[1]]
    dsLines = dsLines[seq_len(min(length(dsLines), 40L))]  # first ~40 lines
    dsDocShort = paste(dsLines, collapse = "\n")

    dataset_block = glue::glue("
Additional information about the data set (from its R documentation):

Data set name: {dsName}

{dsDocShort}

Use this information when explaining what the variables represent.
Do not repeat the documentation verbatim; instead, summarise it briefly
and connect it to the model results.
")
  } else {
    dataset_block = ""
  }


  glue("
You are a friendly statistics tutor.
Explain the model summary below (including the estimated effects and their uncertainty)
in clear, non-technical language.

{modelDesc}
{outcomeDesc}
{dataset_block}

Response variable: {response}
Number of observations: {n}
{r2Text}
If an R-squared value is shown above, briefly explain what it says about how well
the model explains variation in the response.

Coefficient table:
{coefText}

Confidence intervals (95%):
{ciText}

Guidelines:
- If dataset documentation is provided, summarise it in at most one short clause.
- Briefly explain what the response represents and what the predictors represent,
  defining predictors in-line without bullet points.

- Describe the direction of each important effect (positive/negative).
- Use the rounded coefficients to give an intuitive sense of size
  (e.g. \"for each 1-unit increase in Test, the exam score increases by about 3.8 points\").

- Do not mention standard errors, t-values, z-values, or p-values explicitly.

- Do not describe the statistical model, link function, or transformations.
  Focus only on what the model implies about the response in real-world terms.
- Do not name the type of regression or statistical model.

- Effects and confidence intervals are on the scale of the fitted model
  (i.e. after any transformation of the response).
- Do not assume effects are on the original response scale unless explicitly stated.
- Confidence intervals should be described on the same scale as the effect.
- Do not mix scales (e.g. coefficient on log scale with CI on original scale).
- Never report effects or confidence intervals on more than one scale.
- Do not mix scales (e.g. coefficient on log scale with CI on original scale).

- When confidence intervals are provided, include them for each key effect.
- Present the point estimate first, then describe uncertainty using the confidence interval.
- Explain confidence intervals in plain language without assigning probability
  to the parameter (e.g. \"values between L and U are consistent with the data\"
  or \"effects in this range cannot be ruled out by the data\").
- If a confidence interval includes 0, say there is weak or uncertain evidence for a clear effect,
  because the data are consistent with anything from a small decrease to a small increase.
  (Do not mention p-values.)

- For a continuous predictor, use this template:
  \"For each 1-unit increase in X, the expected response changes by about B units
   (95% CI: L to U units), holding other variables constant.\"

- For a factor predictor (categorical), use this template:
  \"Compared with the reference group (REF), group G is about B units higher/lower
   on average (95% CI: L to U units), holding other variables constant.\"

- For binomial models with a logit link:
  Describe effects in terms of higher or lower odds of the event.
  Do not describe effects as changes in probability unless explicitly instructed.
  Do not mention the intercept at all for logit models unless predictors are centred.
  Do not use the term \"odds ratio\".
  Describe effects directly as odds being multiplied by a factor.

- Do not convert odds to percentages.
  If it is helpful to convey very small or very large odds, use a simple
  \"1 in N\" description instead.
- When describing odds, avoid phrases like \"1 in N\" unless explicitly clarifying
  that this refers to odds (e.g. \"1 success for every N failures\").

- Comparisons between groups should be described using odds ratios
  (e.g. \"the odds are about K times higher\"), not by manipulating the intercept.

- Mention the intercept only if it corresponds to a realistic and meaningful
  baseline.
  For logit models, this will usually not be the case unless predictors are
  centred or otherwise have a natural zero.

- If the intercept is mentioned for a logit model, describe it only as baseline
  odds (or a \"1 in N\" equivalent) for the reference group.
  Do not compare the intercept-based baseline across groups.

- For count outcomes:
  Describe effects as expected counts being higher or lower by a certain factor,
  without mentioning logarithms, links, or transformations.
- Use wording like:
  \"Panels at site X are expected to have about K times as many oysters as panels
       at the reference site (95% CI: L to U times).\"

- If the response is log- or log1p-transformed:
  Explain effects as multiplicative (percentage) changes in the original outcome.
  Use approximate percentage language rather than exact back-transformations.
  Use language like:
  \"This corresponds to roughly a X% increase/decrease in the original outcome, on average.\"

- Use units in the narrative when they are obvious from the context (e.g. \"points\").
  If units are not clear, just say \"units\".

- Mention the intercept only if it corresponds to a meaningful baseline.
  If you mention it, explain it as the expected response for the reference group
  when numeric predictors are at zero or their baseline, and include its CI.
  For transformed responses, mention the intercept only if it has a clear
  interpretation on the transformed scale.
  Do not comment on whether the interval implies negative values.
- For count outcomes, do not describe the intercept unless it corresponds
  to a directly meaningful expected count for a clear reference group.
- If mentioned, describe it only as an expected count (with CI),
  without referring to any transformation or model scale.

- If there are interactions, briefly describe how one effect depends on another variable.

- Only discuss overall model fit when a standard R-squared is provided.

- If overall model fit is described, mention it briefly at the end of the explanation,
  after describing the individual effects.

- When describing overall model fit, use wording like:
  \"Overall, the model explains about X% of the variation in the response.\"

- You may optionally add a short qualitative phrase (e.g. \"indicating a reasonable fit\"),
  but do not elaborate further.

- Do not use section headings or bold labels (e.g. \"Intercept\", \"Effect of ...\").
- Write as a single short paragraph (or at most two), not a bulleted list.
")
}
