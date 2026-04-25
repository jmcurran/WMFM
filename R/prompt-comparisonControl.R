#' Build comparison-control prompt text
#'
#' Builds deterministic prompt guidance that limits group or treatment
#' comparisons to the scope implied by the model profile and the user-supplied
#' research question. This keeps explanations from enumerating every pairwise
#' comparison when a concise global summary is more appropriate.
#'
#' @param model A fitted model object.
#' @param mf Optional model frame.
#'
#' @return A character scalar containing comparison-control guidance, or an
#'   empty string if no comparison-control guidance is needed.
#' @keywords internal
buildComparisonControlPromptBlock = function(model, mf = NULL) {

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

  if (is.null(ruleProfile)) {
    return("")
  }

  comparisonScope = ruleProfile$comparisonScope %||% "minimal"

  if (identical(comparisonScope, "none")) {
    return("")
  }

  factorSummary = buildComparisonControlFactorSummary(
    mf = mf,
    predictorTypes = modelProfile$predictorTypes
  )
  researchQuestion = trimws(attr(model, "wmfm_research_question", exact = TRUE) %||% "")
  hasResearchQuestion = nzchar(researchQuestion)

  lines = c(
    "Comparison control:",
    paste0("- Comparison scope: ", comparisonScope, "."),
    paste0("- Research question supplied: ", if (hasResearchQuestion) "yes" else "no", "."),
    paste0("- ", ruleProfile$comparisonGuidance),
    "- Do not enumerate all treatment, group, or factor-level comparisons unless the research question explicitly asks for that level of detail.",
    "- Prefer a concise global summary when the model only needs to establish that groups differ.",
    "- Use targeted comparisons only when they are relevant to the research question, the explanation skeleton, or an interaction pattern.",
    "- Keep uncertainty attached to the estimate or comparison being discussed."
  )

  if (nrow(factorSummary) > 0) {
    lines = c(
      lines,
      "Factor comparison structure:"
    )

    for (i in seq_len(nrow(factorSummary))) {
      row = factorSummary[i, , drop = FALSE]
      lines = c(
        lines,
        paste0(
          "- ",
          row$predictor[[1]],
          ": ",
          row$nLevels[[1]],
          " levels; ",
          row$nPairwiseComparisons[[1]],
          " possible pairwise comparisons."
        )
      )
    }
  }

  if (!hasResearchQuestion && identical(comparisonScope, "minimal")) {
    lines = c(
      lines,
      "- With no research question asking for specific pairs, summarise the factor effect rather than listing pairwise contrasts."
    )
  }

  paste(lines, collapse = "\n")
}

#' Build factor-summary rows for comparison control
#'
#' @param mf Model frame.
#' @param predictorTypes Predictor-type metadata from
#'   `buildExplanationModelProfile()`.
#'
#' @return A data frame with one row per factor predictor.
#' @keywords internal
buildComparisonControlFactorSummary = function(mf, predictorTypes) {

  if (!is.data.frame(mf) || !is.list(predictorTypes)) {
    return(buildEmptyComparisonControlFactorSummary())
  }

  factorPredictors = predictorTypes$factor %||% character(0)
  factorPredictors = intersect(as.character(factorPredictors), names(mf))

  if (length(factorPredictors) == 0) {
    return(buildEmptyComparisonControlFactorSummary())
  }

  rows = lapply(factorPredictors, function(predictor) {
    x = mf[[predictor]]
    levelsX = if (is.factor(x)) {
      levels(x)
    } else {
      sort(unique(as.character(x)))
    }
    nLevels = length(levelsX)

    data.frame(
      predictor = predictor,
      nLevels = nLevels,
      nPairwiseComparisons = if (nLevels >= 2) choose(nLevels, 2) else 0,
      stringsAsFactors = FALSE
    )
  })

  do.call(rbind, rows)
}

buildEmptyComparisonControlFactorSummary = function() {

  data.frame(
    predictor = character(0),
    nLevels = integer(0),
    nPairwiseComparisons = numeric(0),
    stringsAsFactors = FALSE
  )
}
