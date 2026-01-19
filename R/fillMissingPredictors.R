#' Fill missing predictor columns in new data using the model's training frame
#'
#' @param model A fitted model object with a model frame (e.g., lm/glm).
#' @param newData A data frame intended for prediction/design-matrix construction.
#'
#' @return newData with any missing predictor columns added, using values from
#'   the first row of the model frame and preserving factor levels.
#'
#' @importFrom stats model.frame delete.response terms
fillMissingPredictors = function(model, newData) {

  mf = model.frame(model)
  tt = delete.response(terms(model))

  # Variables referenced by the RHS (includes main effects used in interactions)
  vars = all.vars(tt)

  if (length(vars) == 0) {
    return(newData)
  }

  baseRow = mf[1, vars, drop = FALSE]

  # Add any missing columns
  missingVars = setdiff(vars, names(newData))
  if (length(missingVars) > 0) {
    for (v in missingVars) {
      newData[[v]] = rep(baseRow[[v]], nrow(newData))
    }
  }

  # Make sure factor levels match training data
  for (v in intersect(vars, names(newData))) {
    if (is.factor(baseRow[[v]])) {
      newData[[v]] = factor(newData[[v]], levels = levels(baseRow[[v]]))
    }
  }

  newData
}
