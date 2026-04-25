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
  formattedQuantityBlock = buildFormattedPromptQuantityBlock(
    model = model,
    mf = modelFrame,
    predictorNames = predictors
  )
  explanationSkeletonBlock = buildExplanationSkeletonPromptBlock(
    model = model,
    mf = modelFrame
  )
  responseScaleControlBlock = buildResponseScaleControlPromptBlock(
    model = model,
    mf = modelFrame
  )
  comparisonControlBlock = buildComparisonControlPromptBlock(
    model = model,
    mf = modelFrame
  )
  promptValidationGuardBlock = buildPromptValidationGuardBlock(
    model = model,
    mf = modelFrame
  )

  dsDoc = attr(model, "wmfm_dataset_doc", exact = TRUE)
  dsName = attr(model, "wmfm_dataset_name", exact = TRUE)
  researchQuestion = attr(model, "wmfm_research_question", exact = TRUE)

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

  researchQuestionBlock = ""

  if (!is.null(researchQuestion)) {
    researchQuestion = trimws(researchQuestion)

    if (nzchar(researchQuestion)) {
      researchQuestionBlock = glue::glue("
Research question supplied by the user:
{researchQuestion}

Use the research question to structure the explanation.
- Start with a short opening paragraph that briefly restates the research question in clear, natural language.
- Do not skip that opening restatement when a research question is present.
- Then explain the model results as usual in a separate middle paragraph or paragraphs.
- End with a short final paragraph that directly answers the research question in plain language.
- The final paragraph should summarise the answer in one or two sentences rather than repeating the full explanation.
- In the final paragraph, make it clear that the conclusion is about average or expected outcomes rather than certain outcomes for each individual case.
- Prefer wording such as \"on average\", \"tend to\", \"is associated with\", or \"is consistent with\" when answering the research question.
- Ground that answer in the fitted model results and their uncertainty.
- Stay cautious: avoid causal claims unless the model and study design justify them.
- Avoid phrasing that sounds fully predictive, deterministic, or guaranteed for individual cases.
- If the model does not fully answer the question, say what the model does and does not support.
")
    }
  }

  contextPayload = glue::glue("
You are a friendly statistics tutor.
Explain the model summary below (including the estimated effects and their uncertainty)
in clear, non-technical language.

{modelDesc}
{outcomeDesc}
{datasetBlock}
{researchQuestionBlock}

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

{explanationSkeletonBlock}

{formattedQuantityBlock}

{responseScaleControlBlock}

{comparisonControlBlock}

{promptValidationGuardBlock}
")

  prompt = composeWmfmPrompt(
    context = "summary",
    contextPayload = contextPayload
  )

  prompt
}
