#' Build model-aware explanation skeleton text for prompt input
#'
#' Builds a deterministic prompt block from the Stage 9 model profile and rule
#' profile. The block gives the language model an ordered explanation structure
#' while leaving the wording of the student-facing explanation to the model.
#'
#' @param model A fitted model object.
#' @param mf Optional model frame.
#'
#' @return A character scalar containing model-aware skeleton instructions, or
#'   an empty string if a skeleton cannot be built.
#' @keywords internal
buildExplanationSkeletonPromptBlock = function(model, mf = NULL) {

  if (is.null(mf)) {
    mf = tryCatch(
      stats::model.frame(model),
      error = function(e) {
        NULL
      }
    )
  }

  modelProfile = tryCatch(
    buildExplanationModelProfile(model = model, data = mf),
    error = function(e) {
      NULL
    }
  )

  if (is.null(modelProfile)) {
    return("")
  }

  ruleProfile = tryCatch(
    buildExplanationRuleProfile(modelProfile = modelProfile),
    error = function(e) {
      NULL
    }
  )

  if (is.null(ruleProfile) || is.null(ruleProfile$skeletonSteps)) {
    return("")
  }

  skeletonSteps = ruleProfile$skeletonSteps

  if (!is.data.frame(skeletonSteps) || nrow(skeletonSteps) == 0) {
    return("")
  }

  lines = c(
    "Deterministic explanation skeleton:",
    "Use this ordered structure when writing the explanation, but write the sentences naturally for students.",
    "The structure is deterministic; do not replace it with coefficient-by-coefficient commentary.",
    paste0("Skeleton id: ", ruleProfile$skeletonId),
    paste0("Scale guidance: ", ruleProfile$scaleGuidance),
    paste0("Effect language: ", ruleProfile$effectLanguage),
    paste0("Comparison guidance: ", ruleProfile$comparisonGuidance),
    "Skeleton steps:"
  )

  for (i in seq_len(nrow(skeletonSteps))) {
    step = skeletonSteps[i, , drop = FALSE]
    lines = c(
      lines,
      paste0(
        "- ",
        step$stepId[[1]],
        ". ",
        step$stepRole[[1]],
        ": ",
        step$instruction[[1]]
      )
    )
  }

  avoidTerms = ruleProfile$avoidTerms

  if (length(avoidTerms) > 0) {
    lines = c(
      lines,
      paste0("Avoid these terms unless they are needed for a very brief clarification: ", paste(avoidTerms, collapse = ", "), ".")
    )
  }

  if (identical(ruleProfile$modelStructure, "interaction") || grepl("_interaction$", ruleProfile$skeletonId)) {
    lines = c(
      lines,
      "Interaction explanation control:",
      "- Use a within-group-then-compare structure.",
      "- First describe the effect within one relevant group or selected value.",
      "- Then describe the same kind of effect within the other relevant group or selected value.",
      "- Then compare those within-group effects directly in plain language.",
      "- Do not explain by adding, subtracting, or decomposing model coefficients.",
      "- Do not use the phrase interaction term in the student-facing explanation."
    )
  }

  paste(lines, collapse = "\n")
}
