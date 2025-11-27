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

  n = nrow(modelFrame)
  if (inherits(model, "glm")) {
    fam = model$family$family
    link = model$family$link
    modelSummary = summary(model)
    if (!is.null(modelSummary$null.deviance) && modelSummary$null.deviance > 0) {
      pseudoR2 = 1 - modelSummary$deviance / modelSummary$null.deviance
      r2Text = glue("Pseudo R-squared (1 - dev/null): {round(pseudoR2, 3)}")
    } else {
      r2Text = "Pseudo R-squared not available."
    }
    modelDesc = glue(
      "This is a generalised linear model with {fam} family and {link} link."
    )
  } else {
    modelSummary = summary(model)
    r2 = round(modelSummary$r.squared, 3)
    r2Text = glue("R-squared: {r2}")
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
Explain the results of this regression model in clear, non-technical language.

{modelDesc}
{outcomeDesc}
{dataset_block}

Response variable: {response}
Number of observations: {n}
{r2Text}

Coefficient table:
{coefText}

Guidelines:
- Briefly explain what the response represents and what the predictors represent.
- Describe the direction of each important effect (positive/negative).
- Use the rounded coefficients to give an intuitive sense of size
  (e.g. 'for each 1-unit increase in Test, the exam score increases by about 3.8 points').
- For binomial/logit models, talk about higher or lower probability of the event.
- For Poisson/log models, talk about higher or lower expected counts or rates.
- For factor predictors, explain differences between groups.
- If there are interactions, briefly describe how one effect depends on another variable.
- Do not mention standard errors, t-values, z-values, or p-values explicitly.
- Keep it to 1-3 short paragraphs.

Now provide the explanation.
")
}
