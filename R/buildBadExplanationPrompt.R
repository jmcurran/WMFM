#' Build an LLM prompt for generating bad explanations
#'
#' @param x A `wmfmModel` object.
#' @param baseExplanation Character scalar giving the good explanation.
#' @param plan A plan object produced by `buildBadExplanationPlan()`.
#'
#' @return A character scalar containing the prompt text.
#' @keywords internal
buildBadExplanationPrompt = function(
    x,
    baseExplanation,
    plan
) {

  modelSummaryBlock = buildBadExplanationModelContext(x)
  typeGuideBlock = buildBadExplanationTypeGuide()
  instructionBlock = buildBadExplanationInstructionBlock(plan)

  paste(
    "You are helping generate deliberately flawed student-style explanations",
    "for testing a grading system.",
    "",
    "Your task is to rewrite a good explanation into one or more bad",
    "explanations that contain realistic statistical mistakes.",
    "",
    "Important constraints:",
    "- Keep each explanation coherent and plausible.",
    "- Do not produce nonsense, parody, or obviously fake writing.",
    "- Preserve the same model topic, variables, and overall style as the original explanation.",
    "- Introduce the requested mistake types intentionally.",
    "- Do not mention that the explanation is intentionally wrong.",
    "- Return valid JSON only.",
    "",
    "Model context:",
    modelSummaryBlock,
    "",
    "Original good explanation:",
    baseExplanation,
    "",
    "Bad explanation type guide:",
    typeGuideBlock,
    "",
    "Generation instructions:",
    instructionBlock,
    "",
    "Return a JSON array. Each array element must contain exactly these fields:",
    '- "name"',
    '- "text"',
    '- "errorTypes"',
    '- "severity"',
    "",
    "The `text` field must be plain text only.",
    "Do not include markdown code fences.",
    sep = "\n"
  )
}
