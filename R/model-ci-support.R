#' Compute confidence intervals for fitted mean responses
#'
#' Computes pointwise confidence intervals for the fitted mean response
#' evaluated at \code{newData}. These are confidence intervals for the
#' conditional mean (i.e., the fitted line), not prediction intervals.
#'
#' For generalized linear models, intervals are computed on the link scale
#' and transformed back to the response scale.
#'
#' Robust (sandwich) confidence intervals are supported via \code{vcovHC()}.
#'
#' @param model A fitted \code{lm} or \code{glm} object.
#' @param newData A data frame of covariate values at which to evaluate the
#'   fitted mean.
#' @param ciType Character string: \code{"standard"} or \code{"sandwich"}.
#' @param hcType Character string specifying the HC estimator
#'   (e.g. \code{"HC0"}, \code{"HC3"}). Used only when
#'   \code{ciType = "sandwich"}.
#' @param level Confidence level for the intervals.
#'
#' @return A data frame containing \code{newData} plus columns
#'   \code{fit}, \code{lower}, and \code{upper}.
#'
#' @importFrom stats predict qnorm model.matrix terms coef delete.response
#'
#' @export

computeMeanCi = function(model, newData, ciType = "standard", hcType = "HC0", level = 0.95) {

  stopifnot(!is.null(model))
  stopifnot(is.data.frame(newData))

  isGlm = inherits(model, "glm")

  alpha = 1 - level
  z = qnorm(1 - alpha / 2)

  if (identical(ciType, "standard")) {

    if (!isGlm) {
      pr = predict(model, newdata = newData, se.fit = TRUE)
      fit = as.numeric(pr$fit)
      se  = as.numeric(pr$se.fit)

      out = newData
      out$fit   = fit
      out$lower = fit - z * se
      out$upper = fit + z * se
      return(out)
    }

    pr = predict(model, newdata = newData, type = "link", se.fit = TRUE)
    eta = as.numeric(pr$fit)
    se  = as.numeric(pr$se.fit)

    linkInv = model$family$linkinv
    out = newData
    out$fit   = linkInv(eta)
    out$lower = linkInv(eta - z * se)
    out$upper = linkInv(eta + z * se)
    return(out)
  }

  if (!identical(ciType, "sandwich")) {
    stop("ciType must be 'standard' or 'sandwich'.")
  }

  newData = fillMissingPredictors(model, newData)

  tt = delete.response(terms(model))
  X  = model.matrix(tt, data = newData)
  V = vcovHC(model, type = hcType)

  # diag(X %*% V %*% t(X)) without constructing the full matrix
  varEta = rowSums((X %*% V) * X)
  seEta  = sqrt(pmax(varEta, 0))

  if (!isGlm) {
    fit = as.numeric(X %*% coef(model))

    out = newData
    out$fit   = fit
    out$lower = fit - z * seEta
    out$upper = fit + z * seEta
    return(out)
  }

  eta = as.numeric(X %*% coef(model))
  linkInv = model$family$linkinv

  out = newData
  out$fit   = linkInv(eta)
  out$lower = linkInv(eta - z * seEta)
  out$upper = linkInv(eta + z * seEta)
  out
}


#' Render confidence-interval derivation details as UI
#'
#' Converts the derivation records produced by
#' \code{buildModelConfidenceIntervalData()} into a compact accordion so that
#' students can inspect how a displayed confidence interval is constructed
#' without overwhelming the main table.
#'
#' @param details A list of derivation records.
#'
#' @return A Shiny UI object.
#' @keywords internal
#'
#' @importFrom bslib accordion accordion_panel
#' @importFrom htmltools HTML tagList tags
renderModelConfidenceIntervalDetailsUi = function(details) {

  if (length(details) == 0) {
    return(NULL)
  }

  panels = lapply(details, function(oneDetail) {
    accordion_panel(
      title = oneDetail$label,
      tagList(
        tags$p(tags$strong("Context"), tags$br(), oneDetail$context),
        tags$p(tags$strong("Linear combination"), tags$br(), HTML(oneDetail$combination)),
        tags$p(tags$strong("Variance rule"), tags$br(), HTML(oneDetail$variance)),
        tags$p(tags$strong("Why this matters"), tags$br(), oneDetail$note)
      )
    )
  })

  do.call(
    accordion,
    c(
      list(
        id = "model_confint_details",
        multiple = TRUE,
        open = FALSE
      ),
      panels
    )
  )
}
