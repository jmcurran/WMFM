#' Build validation-guard prompt guidance
#'
#' Builds deterministic prompt guidance from the Stage 9 validation targets. The
#' guard is deliberately prompt-side and diagnostic only: it asks the language
#' model to avoid known failure modes, while leaving automatic regeneration for a
#' later stage.
#'
#' @param model A fitted model object.
#' @param mf Optional model frame.
#'
#' @return A character scalar containing validation-guard prompt guidance.
#' @keywords internal
buildPromptValidationGuardBlock = function(model, mf = NULL) {

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

  ruleProfile = if (!is.null(modelProfile)) {
    tryCatch(
      buildExplanationRuleProfile(modelProfile = modelProfile),
      error = function(e) {
        NULL
      }
    )
  } else {
    NULL
  }

  guardTargets = buildPromptValidationGuardTargets(
    modelProfile = modelProfile,
    ruleProfile = ruleProfile
  )

  lines = c(
    "Prompt validation guard:",
    "- Before finalising the explanation, check the draft against these deterministic guard targets.",
    "- These guards are diagnostic and prompt-side only; do not regenerate automatically.",
    "- Keep the student-facing explanation clean and surface any failures through developer diagnostics.",
    "- Guard targets:"
  )

  lines = c(
    lines,
    paste0("  - ", guardTargets$label, " [", guardTargets$flag, "]")
  )

  paste(lines, collapse = "\n")
}

#' Build validation-guard target rows
#'
#' @param modelProfile Explanation model profile, or `NULL`.
#' @param ruleProfile Explanation rule profile, or `NULL`.
#'
#' @return A data frame with `flag` and `label` columns.
#' @keywords internal
buildPromptValidationGuardTargets = function(modelProfile = NULL, ruleProfile = NULL) {

  modelFamily = modelProfile$modelFamily %||% "unknown"
  hasInteraction = isTRUE(modelProfile$hasInteraction %||% FALSE)
  comparisonScope = ruleProfile$comparisonScope %||% "minimal"

  rows = data.frame(
    flag = c(
      "technicalScaleLeakage",
      "rawCoefficientShown",
      "oddsShownAsDecimal",
      "effectWithoutChangeLanguage",
      "missingAnswer",
      "missingUncertainty"
    ),
    label = c(
      "avoid technical scale leakage such as log-odds, log counts, link scale, or linear predictor language",
      "avoid raw coefficients, intercepts, beta notation, and coefficient-scale interpretations",
      "avoid displaying odds as ordinary decimals when odds-format quantities are available",
      "use explicit change language for numeric-predictor effects",
      "include a short answer sentence that directly addresses the research question or main model question",
      "keep uncertainty with the estimate whenever confidence intervals are provided"
    ),
    stringsAsFactors = FALSE
  )

  if (!identical(modelFamily, "logistic")) {
    rows = rows[rows$flag != "oddsShownAsDecimal", , drop = FALSE]
  }

  if (!identical(comparisonScope, "none")) {
    rows = rbind(
      rows,
      data.frame(
        flag = "excessiveComparisons",
        label = "avoid unnecessary exhaustive pairwise comparisons unless the research question asks for them",
        stringsAsFactors = FALSE
      )
    )
  }

  if (hasInteraction) {
    rows = rbind(
      rows,
      data.frame(
        flag = "interactionNotCompared",
        label = "for interactions, compare the within-group effects after describing them",
        stringsAsFactors = FALSE
      )
    )
  }

  rownames(rows) = NULL
  rows
}
