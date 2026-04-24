#' Build a deterministic equation table from a fitted model
#'
#' Runs the deterministic equation pipeline by building an equation
#' specification, enumerating teaching cases, and rendering those cases into a
#' tabular output suitable for printing or downstream storage.
#'
#' The returned table includes a compatibility `equation` column containing the
#' rendered scales collapsed into one text field, along with separate scale
#' columns for later UI use.
#'
#' @param model A fitted model object, typically of class `lm` or `glm`.
#' @param digits Integer number of decimal places for displayed coefficients.
#'   Defaults to `2`.
#'
#' @return A `data.frame` with one row per displayed teaching case.
#' @keywords internal
buildDeterministicEquationTable = function(model, digits = 2) {

  spec = buildEquationSpec(model)
  cases = buildEquationCases(spec)
  rendered = renderEquationCases(spec = spec, cases = cases, digits = digits)

  collapseEquationText = function(x) {
    pieces = c(x$linearPredictor, x$oddsScale, x$responseScale)
    pieces = pieces[!vapply(pieces, is.null, logical(1))]
    pieces = pieces[!is.na(pieces)]
    paste(pieces, collapse = "\n")
  }

  out = data.frame(
    condition = vapply(rendered, function(x) x$label, character(1)),
    equation = vapply(rendered, collapseEquationText, character(1)),
    linearPredictor = vapply(rendered, function(x) x$linearPredictor, character(1)),
    oddsScale = vapply(
      rendered,
      function(x) {
        if (is.null(x$oddsScale)) {
          return(NA_character_)
        }

        x$oddsScale
      },
      character(1)
    ),
    responseScale = vapply(
      rendered,
      function(x) {
        if (is.null(x$responseScale)) {
          return(NA_character_)
        }

        x$responseScale
      },
      character(1)
    ),
    stringsAsFactors = FALSE
  )

  class(out) = c("wmfmEquationTable", class(out))
  out
}

#' Get fitted-model equations using the selected equation engine
#'
#' Provides a single entry point for equation generation. Deterministic
#' equations are now the default. The older LLM equation path remains available
#' as an opt-in compatibility route during the transition.
#'
#' @param model A fitted model object, typically of class `lm` or `glm`.
#' @param method Character string giving the equation engine. Must be one of
#'   `"deterministic"` or `"llm"`.
#' @param chat Optional chat provider object. Required when `method = "llm"`.
#' @param digits Integer number of decimal places for displayed coefficients in
#'   the deterministic path. Defaults to `2`.
#'
#' @return Either a deterministic equation table or the existing language-model
#'   equation object.
#' @export
getModelEquations = function(
    model,
    method = c("deterministic", "llm"),
    chat = NULL,
    digits = 2
) {

  method = match.arg(method)

  lmEquations(
    model = model,
    chat = chat,
    method = method,
    digits = digits
  )
}
