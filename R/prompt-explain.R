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
  predictors = names(modelFrame)[-1]
  isInterceptOnlyModel = length(predictors) == 0

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
    if (isInterceptOnlyModel) {
      r2Text = ""
    } else {
      r2 = paste0(round(modelSummary$r.squared * 100), "%")
      r2Text = glue::glue("Approximate proportion of variation explained by the model: {r2}")
    }
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
  termEvidenceBlock = buildLmTermEvidencePromptBlock(
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

  if (isInterceptOnlyModel && !is.null(researchQuestion)) {
    researchQuestion = normaliseInterceptOnlyResearchQuestion(
      researchQuestion = researchQuestion
    )
  }

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
      researchQuestionGuidanceText = paste(
        getResearchQuestionGuidanceLines(context = "explanationBlock"),
        collapse = "\n"
      )
      researchQuestionBlock = glue::glue("
Research question supplied by the user:
{researchQuestion}

{researchQuestionGuidanceText}
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
For intercept-only models, do not discuss R-squared, variation explained, model fit, or the absence of predictors unless the user specifically asks about them.
For intercept-only models, answer using the supplied formatted estimate and confidence interval; do not give generic statements about constants or intervals without the numbers.
For intercept-only models with a confidence interval, frame the estimate as uncertainty about an underlying average, probability, or expected count for the relevant setting, rather than only as the observed sample mean.
For intercept-only models, Restate the research question inferentially; do not describe it as only the average in this data set, dataset, course data, or sample.
For intercept-only models, prefer wording such as: We estimate this value to be X, with a 95% confidence interval of L to U.
For intercept-only models, it is acceptable to say we can be 95% confident that the true value lies within this interval; do not describe this as a probability and do not say there is a 95% chance.
For intercept-only models, prefer we can be 95% confident over the true value is likely.
For intercept-only models, write a maximum of two short paragraphs: one sentence restating the inferential research question, then one answer sentence using the estimate and confidence interval.
For intercept-only models, keep exactly one sentence that contains the estimate and confidence interval, and do not repeat the same estimate in a separate final sentence; this sentence is both the estimate, the uncertainty statement, and the final answer.
For intercept-only models, the sentence containing the estimate and confidence interval is the final answer; Do not add a separate range explanation, a second overall sentence, or a generic closing sentence after it.
For intercept-only models, one concise answer with the estimate and confidence interval is enough.
For intercept-only models, do not include standalone sentences explaining what a confidence interval is, and do not include filler such as the range shows, the data allow, this gives a reasonable sense, typical performance, or the analysis combined all observations.

{numericAnchorInfo$promptText}

Interpretation rules for numeric predictors:
- When discussing a baseline fitted value, expected count, mean response, odds, or probability, use the chosen anchor value for each numeric predictor rather than automatically using 0.
- If 0 lies outside the observed range for a numeric predictor, do not describe the intercept as if it were directly meaningful at 0.
- For interaction models, explain conditional comparisons at the chosen anchor value unless another value is explicitly being discussed.
- It is fine to mention that the formal fitted equation is a function of the numeric predictor, but the substantive interpretation should be anchored at the chosen value above.
- For multiplicative effects on counts or odds, describe confidence intervals relative to the no-change value of 1, or say they correspond to a decrease throughout the interval.
- If a multiplicative confidence interval includes 1, say the evidence for a clear multiplicative change is weak or uncertain; report the interval if useful, but do not present the point estimate as a clearly supported difference or effect.
- Do not say that a multiplicative confidence interval lies below zero.

{anchoredBaselineBlock}

{explanationSkeletonBlock}

{termEvidenceBlock}

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

#' Normalise intercept-only research-question wording for prompt use
#'
#' Intercept-only explanations should frame the question as inference about an
#' underlying average, probability, or expected count, not as a descriptive
#' question about the observed data set. This helper removes common user-entered
#' data-set phrases before the question is inserted into the prompt.
#'
#' @param researchQuestion Character scalar containing the supplied research
#'   question.
#'
#' @return A character scalar with sample/data wording replaced by setting-level
#'   wording where possible.
#' @keywords internal
#' @noRd
normaliseInterceptOnlyResearchQuestion = function(researchQuestion) {
  if (!is.character(researchQuestion) || length(researchQuestion) != 1 || is.na(researchQuestion)) {
    return("")
  }

  out = trimws(researchQuestion)

  out = gsub(
    "\\b(in|from)\\s+(the|this|these|our)?\\s*course\\s+(data|dataset|data set|sample)\\b",
    "for this course",
    out,
    ignore.case = TRUE,
    perl = TRUE
  )

  out = gsub(
    "\\b(in|from)\\s+(the|this|these|our)?\\s*(data|dataset|data set|sample)\\b",
    "for the relevant setting",
    out,
    ignore.case = TRUE,
    perl = TRUE
  )

  out = gsub("\\s+", " ", out, perl = TRUE)
  trimws(out)
}
