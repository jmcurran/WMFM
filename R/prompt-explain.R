#' Build a prompt asking for a plain-language model explanation
#'
#' Given a fitted linear or generalised linear model, construct a prompt that
#' asks a language model to explain the results in clear, non-technical
#' language. The prompt includes basic model information, an R-squared or
#' pseudo R-squared, and the coefficient table.
#'
#' @param model A fitted model object, typically of class \code{\"lm\"} or
#'   \code{\"glm\"}.
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
    modelDesc = glue::glue(
      "This is a generalised linear model with {fam} family and {link} link."
    )
  } else {
    modelSummary = summary(model)
    r2 = round(modelSummary$r.squared, 3)
    r2Text = glue::glue("Approximate proportion of variation explained by the model: {r2}")
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

  predictors = names(modelFrame)[-1]
  numericAnchorInfo = buildModelNumericAnchorInfo(
    model = model,
    mf = modelFrame,
    predictorNames = predictors
  )
  anchoredBaselineBlock = buildAnchoredBaselinePromptBlock(
    model = model,
    mf = modelFrame,
    predictorNames = predictors
  )

  dsDoc = attr(model, "wmfm_dataset_doc", exact = TRUE)
  dsName = attr(model, "wmfm_dataset_name", exact = TRUE)

  if (!is.null(dsDoc)) {
    dsLines = strsplit(dsDoc, "\n", fixed = TRUE)[[1]]
    dsLines = dsLines[seq_len(min(length(dsLines), 40L))]
    dsDocShort = paste(dsLines, collapse = "\n")

    datasetBlock = glue::glue("
Additional information about the data set (from its R documentation):

Data set name: {dsName}

{dsDocShort}

Use this information when explaining what the variables represent.
Do not repeat the documentation verbatim; instead, summarise it briefly
and connect it to the model results.
")
  } else {
    datasetBlock = ""
  }

  contextPayload = glue::glue("
You are a friendly statistics tutor.
Explain the model summary below (including the estimated effects and their uncertainty)
in clear, non-technical language.

{modelDesc}
{outcomeDesc}
{datasetBlock}

Response variable: {response}
Number of observations: {n}
{r2Text}
If an R-squared value is shown above, briefly explain what it says about how well
the model explains variation in the response.

{numericAnchorInfo$promptText}

Interpretation rules for numeric predictors:
- When discussing a baseline fitted value, expected count, mean response, odds, or probability, use the chosen anchor value for each numeric predictor rather than automatically using 0.
- If 0 lies outside the observed range for a numeric predictor, do not describe the intercept as if it were directly meaningful at 0.
- For interaction models, explain conditional comparisons at the chosen anchor value unless another value is explicitly being discussed.
- It is fine to mention that the formal fitted equation is a function of the numeric predictor, but the substantive interpretation should be anchored at the chosen value above.
- For multiplicative effects on counts or odds, describe confidence intervals relative to the no-change value of 1, or say they correspond to a decrease throughout the interval.
- Do not say that a multiplicative confidence interval lies below zero.

{anchoredBaselineBlock}

Coefficient table:
{coefText}

Confidence intervals (95%):
{ciText}
")

  prompt = composeWmfmPrompt(
    context = "summary",
    contextPayload = contextPayload
  )

  prompt
}
