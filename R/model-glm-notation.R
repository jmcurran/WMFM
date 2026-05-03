#' Build student-facing notation for GLM teaching output
#'
#' Creates a small bundle of notation strings used in equations and confidence
#' interval output for common GLM families.
#'
#' For binomial models, the helper extracts the response-variable name and the
#' two observed outcome levels from the model response and returns labels such
#' as \code{"Pr(Pass = Yes)"} and \code{"Odds(Pass = Yes)"}. The second level
#' is treated as the success level to match R's usual treatment coding for a
#' two-level factor response.
#'
#' For Poisson models, the helper returns labels based on the response-variable
#' name, such as \code{"E[Freq]"}.
#'
#' @param model A fitted model object, typically of class \code{"glm"}.
#'
#' @return A named list of notation strings.
#'
#' @keywords internal
buildGlmTeachingNotation = function(model) {

  responseName = tryCatch(
    names(stats::model.frame(model))[1],
    error = function(e) {
      "Y"
    }
  )

  if (!inherits(model, "glm")) {
    return(list(
      response = responseName,
      mean = paste0("E[", responseName, "]"),
      logMean = paste0("log(E[", responseName, "])")
    ))
  }

  fam = stats::family(model)$family
  link = stats::family(model)$link

  if (identical(fam, "binomial") && identical(link, "logit")) {
    mf = stats::model.frame(model)
    responseName = names(mf)[1]
    response = mf[[1]]

    if (is.factor(response) && nlevels(response) == 2) {
      failureLevel = as.character(levels(response)[1])
      successLevel = as.character(levels(response)[2])
    } else {
      observed = unique(stats::na.omit(response))
      observed = as.character(observed)

      if (length(observed) >= 2) {
        failureLevel = observed[1]
        successLevel = observed[2]
      } else {
        failureLevel = "0"
        successLevel = "1"
      }
    }

    return(list(
      response = responseName,
      successLevel = successLevel,
      failureLevel = failureLevel,
      probabilitySuccess = paste0("Pr(", responseName, " = \"", successLevel, "\")"),
      probabilityFailure = paste0("Pr(", responseName, " = \"", failureLevel, "\")"),
      oddsSuccess = paste0("Odds(", responseName, " = \"", successLevel, "\" vs ", responseName, " = \"", failureLevel, "\")"),
      oddsFailure = paste0("Odds(", responseName, " = \"", failureLevel, "\" vs ", responseName, " = \"", successLevel, "\")"),
      logitSuccess = paste0("logit(Pr(", responseName, " = \"", successLevel, "\"))")
    ))
  }

  if (identical(fam, "poisson") && identical(link, "log")) {
    return(list(
      response = responseName,
      mean = paste0("E[", responseName, "]"),
      logMean = paste0("log(E[", responseName, "])"),
      meanMultiplier = paste0("E[", responseName, "] multiplier")
    ))
  }

  list(
    response = responseName,
    mean = paste0("E[", responseName, "]"),
    logMean = paste0("log(E[", responseName, "])")
  )
}
