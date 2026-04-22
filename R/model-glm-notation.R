#' Build student-facing notation for GLM teaching output
#'
#' Creates a small bundle of notation strings used in equations and confidence
#' interval output for common GLM families.
#'
#' For binomial models, the helper extracts the two observed outcome levels from
#' the model response and returns labels such as
#' \code{"Pr(Y = Pass)"} and \code{"Odds(Y = Pass)"}. The second level is
#' treated as the success level to match R's usual treatment coding for a
#' two-level factor response.
#'
#' For Poisson models, the helper returns labels based on \code{"E(Y)"}.
#'
#' @param model A fitted model object, typically of class \code{"glm"}.
#'
#' @return A named list of notation strings.
#'
#' @keywords internal
buildGlmTeachingNotation = function(model) {

  if (!inherits(model, "glm")) {
    return(list(
      response = "Y",
      mean = "E(Y)",
      logMean = "log(E(Y))"
    ))
  }

  fam = stats::family(model)$family
  link = stats::family(model)$link

  if (identical(fam, "binomial") && identical(link, "logit")) {
    mf = stats::model.frame(model)
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
      response = "Y",
      successLevel = successLevel,
      failureLevel = failureLevel,
      probabilitySuccess = paste0("Pr(Y = ", successLevel, ")"),
      probabilityFailure = paste0("Pr(Y = ", failureLevel, ")"),
      oddsSuccess = paste0("Odds(Y = ", successLevel, ")"),
      oddsFailure = paste0("Odds(Y = ", failureLevel, ")"),
      logitSuccess = paste0("logit(Pr(Y = ", successLevel, "))")
    ))
  }

  if (identical(fam, "poisson") && identical(link, "log")) {
    return(list(
      response = "Y",
      mean = "E(Y)",
      logMean = "log(E(Y))",
      meanMultiplier = "E(Y) multiplier"
    ))
  }

  list(
    response = "Y",
    mean = "E(Y)",
    logMean = "log(E(Y))"
  )
}
