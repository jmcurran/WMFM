#' Create a WMFM model object
#'
#' Creates a classed `wmfmModel` object that stores a fitted model together
#' with the additional context and generated outputs used by the WMFM
#' command-line workflow.
#'
#' @param model A fitted model object.
#' @param formula A model formula.
#' @param modelType Character string giving the model family.
#' @param data A `data.frame` used to fit the model.
#' @param dataContext Optional character string giving dataset context.
#' @param researchQuestion Optional character string giving the research
#'   question associated with the fitted model.
#' @param equations Generated equations object, or `NULL`.
#' @param explanation Generated explanation text, or `NULL`.
#' @param explanationAudit Deterministic explanation-audit object, or `NULL`.
#'   When present, this should inherit from `wmfmExplanationAudit` and follow
#'   the same top-level contract produced by `buildModelExplanationAudit()`.
#' @param explanationClaimEvidenceMap Deterministic claim-to-evidence map, or `NULL`.
#' @param interactionTerms Character vector of fitted interaction-term names.
#' @param interactionMinPValue Minimum p-value across fitted interaction terms,
#'   or `NA_real_`.
#' @param meta Optional named list of metadata.
#'
#' @return An object of class `wmfmModel`.
#' @export
newWmfmModel = function(
    model,
    formula,
    modelType,
    data,
    dataContext = NULL,
    researchQuestion = NULL,
    equations = NULL,
    explanation = NULL,
    explanationAudit = NULL,
    explanationClaimEvidenceMap = NULL,
    interactionTerms = character(0),
    interactionMinPValue = NA_real_,
    meta = list()
) {

  if (missing(model) || is.null(model)) {
    stop("`model` must be supplied.", call. = FALSE)
  }

  if (!inherits(formula, "formula")) {
    stop("`formula` must inherit from `formula`.", call. = FALSE)
  }

  if (!is.character(modelType) || length(modelType) != 1 || is.na(modelType)) {
    stop("`modelType` must be a single character string.", call. = FALSE)
  }

  if (!is.data.frame(data)) {
    stop("`data` must be a data.frame.", call. = FALSE)
  }

  if (is.null(dataContext)) {
    dataContext = NA_character_
  }

  if (is.null(researchQuestion)) {
    researchQuestion = NA_character_
  }

  if (!is.character(researchQuestion) || length(researchQuestion) != 1) {
    stop("`researchQuestion` must be a single character string or NULL.", call. = FALSE)
  }

  if (!is.character(interactionTerms)) {
    stop("`interactionTerms` must be a character vector.", call. = FALSE)
  }

  if (!is.numeric(interactionMinPValue) || length(interactionMinPValue) != 1) {
    stop(
      "`interactionMinPValue` must be a single numeric value.",
      call. = FALSE
    )
  }

  if (!is.list(meta)) {
    stop("`meta` must be a list.", call. = FALSE)
  }

  validateWmfmExplanationAudit(
    x = explanationAudit,
    allowNull = TRUE
  )

  out = list(
    model = model,
    formula = formula,
    modelType = modelType,
    data = data,
    dataContext = dataContext,
    researchQuestion = researchQuestion,
    equations = equations,
    explanation = explanation,
    explanationAudit = explanationAudit,
    explanationClaimEvidenceMap = explanationClaimEvidenceMap,
    interactionTerms = interactionTerms,
    interactionMinPValue = interactionMinPValue,
    meta = utils::modifyList(
      list(
        createdAt = as.character(Sys.time())
      ),
      meta
    )
  )

  class(out) = c("wmfmModel", class(out))
  out
}
