#' Build term-evidence guidance for explanation prompts
#'
#' Uses the linear-model ANOVA table as internal guidance for which additive or
#' interaction terms deserve emphasis in the student-facing explanation. The
#' block is prompt-side guidance only: explanations should not report p-values
#' or ANOVA mechanics directly to students.
#'
#' @param model A fitted model object.
#' @param mf Optional model frame.
#' @param alpha Numeric threshold for treating a term as clearly supported.
#'
#' @return A character scalar containing prompt guidance, or an empty string
#'   when term-level evidence is unavailable or not applicable.
#' @keywords internal
#'
#' @importFrom stats anova
buildLmTermEvidencePromptBlock = function(model, mf = NULL, alpha = 0.05) {

  if (!inherits(model, "lm") || inherits(model, "glm")) {
    return("")
  }

  if (!is.numeric(alpha) || length(alpha) != 1 || is.na(alpha) || alpha <= 0 || alpha >= 1) {
    alpha = 0.05
  }

  if (is.null(mf)) {
    mf = tryCatch(
      stats::model.frame(model),
      error = function(e) {
        NULL
      }
    )
  }

  predictorNames = if (!is.null(mf) && is.data.frame(mf)) {
    names(mf)[-1]
  } else {
    character(0)
  }

  if (length(predictorNames) == 0) {
    return("")
  }

  anovaTable = tryCatch(
    stats::anova(model),
    error = function(e) {
      NULL
    }
  )

  if (is.null(anovaTable) || !is.data.frame(anovaTable) || nrow(anovaTable) == 0) {
    return("")
  }

  termRows = rownames(anovaTable)
  pColumn = grep("Pr", names(anovaTable), fixed = TRUE, value = TRUE)

  if (length(termRows) == 0 || length(pColumn) == 0) {
    return("")
  }

  pValues = anovaTable[[pColumn[[1]]]]
  keep = !is.na(termRows) & nzchar(termRows) & !is.na(pValues)
  termRows = termRows[keep]
  pValues = pValues[keep]

  if (length(termRows) == 0) {
    return("")
  }

  termLabels = attr(stats::terms(model), "term.labels") %||% character(0)
  interactionTerms = termLabels[grepl(":", termLabels, fixed = TRUE)]
  hasInteraction = length(interactionTerms) > 0

  lines = c(
    "Term-level evidence guidance for the explanation:",
    "Use this block only to decide emphasis; do not mention ANOVA, F-tests, or p-values in the student-facing explanation.",
    "Terms marked as clear can receive primary emphasis; terms marked as weak should be described cautiously and briefly, not as substantive findings.",
    "Term evidence summary:"
  )

  for (i in seq_along(termRows)) {
    evidenceLabel = if (isTRUE(pValues[[i]] < alpha)) {
      "clear"
    } else {
      "weak"
    }

    termType = if (grepl(":", termRows[[i]], fixed = TRUE)) {
      "interaction"
    } else {
      "main effect"
    }

    lines = c(
      lines,
      paste0("- ", termRows[[i]], ": ", evidenceLabel, " term-level evidence (", termType, ").")
    )
  }

  weakMainTerms = termRows[!grepl(":", termRows, fixed = TRUE) & !(pValues < alpha)]
  weakInteractionTerms = termRows[grepl(":", termRows, fixed = TRUE) & !(pValues < alpha)]

  if (length(weakMainTerms) > 0) {
    lines = c(
      lines,
      paste0(
        "For weak additive terms (",
        paste(weakMainTerms, collapse = ", "),
        "), say that the model does not show a clear difference or effect rather than presenting the estimate as an important substantive pattern."
      )
    )
  }

  if (length(weakInteractionTerms) > 0) {
    lines = c(
      lines,
      paste0(
        "For weak interaction terms (",
        paste(weakInteractionTerms, collapse = ", "),
        "), say that there is no clear evidence that the effect of one predictor differs across the other predictor."
      ),
      "When a weak interaction is present, still use the supplied simple comparisons to explain the fitted pattern, but avoid implying that the difference between those simple comparisons is meaningful."
    )
  } else if (hasInteraction) {
    lines = c(
      lines,
      "When an interaction term has clear term-level evidence, explain the within-group effects first and then compare those effects directly."
    )
  }

  paste(lines, collapse = "\n")
}
