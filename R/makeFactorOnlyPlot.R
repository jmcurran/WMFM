#' Plot response by factor predictors for factor-only models
#'
#' Produces a grouped plot for models with only factor predictors.
#'
#' * Uses boxplots when all groups have at least 10 observations
#' * Uses jittered point plots when any group has fewer than 10 observations
#' * Adds fitted means and 95% confidence intervals alongside each group
#' * Supports standard (model-based) and robust (sandwich) confidence intervals
#' * Applies a log(1 + y) scale when the model is Poisson
#'
#' For multiple factor predictors, groups are defined by their interaction.
#'
#' Robust confidence intervals are computed on the linear predictor scale using
#' X V X^T, where V is either vcov(model) or sandwich::vcovHC(model, type = ...).
#' For GLMs, intervals are then transformed back to the response scale using the
#' inverse link function.
#'
#' @param model A fitted model object (e.g. \code{lm}, \code{glm}).
#' @param data A data frame containing the variables used to fit the model.
#' @param ciType Confidence-interval type. One of \code{"standard"} (model-based)
#'   or \code{"sandwich"} (robust).
#' @param hcType Heteroskedasticity-consistent estimator type for robust CIs.
#'   One of \code{"HC0"} or \code{"HC3"}. Only used when \code{ciType="sandwich"}.
#'
#' @return A \code{ggplot} object.
#'
#' @importFrom stats coef vcov qnorm family model.matrix terms delete.response
#' @importFrom ggplot2 ggplot aes labs theme_minimal theme element_text
#' @importFrom ggplot2 geom_boxplot stat_boxplot geom_point geom_errorbar
#' @importFrom ggplot2 position_jitter position_nudge
#' @importFrom ggplot2 scale_y_continuous scale_x_discrete expansion
#' @importFrom rlang .data
#'
#' @export
makeFactorOnlyPlot = function(
    model,
    data,
    ciType = c("standard", "sandwich"),
    hcType = c("HC0", "HC3")
) {
  ciType = match.arg(ciType)
  hcType = match.arg(hcType)

  responseVar = all.vars(formula(model))[1]
  factorPreds = getFactorPredictors(model, data)

  if (length(factorPreds) == 0) {
    stop("makeFactorOnlyPlot() requires at least one factor predictor.")
  }

  if (length(factorPreds) == 1) {
    group = data[[factorPreds[1]]]
    xLabel = factorPreds[1]
  } else {
    group = interaction(
      data[, factorPreds, drop = FALSE],
      drop = TRUE,
      sep = " : "
    )
    xLabel = "Group"
  }

  plotDf = data.frame(
    y = data[[responseVar]],
    group = droplevels(group)
  )

  levs = levels(plotDf$group)

  groupSizes = table(plotDf$group)
  minGroupSize = min(as.integer(groupSizes))

  isGlm = inherits(model, "glm")
  isPoisson = isGlm && identical(family(model)$family, "poisson")

  # --- newdata for fitted mean/CI (one row per group) ---
  if (length(factorPreds) == 1) {
    newData = data.frame(tmp = factor(levs, levels = levs))
    names(newData) = factorPreds[1]
  } else {
    parts = strsplit(levs, " : ", fixed = TRUE)
    partsMat = do.call(rbind, parts)

    newData = as.data.frame(partsMat, stringsAsFactors = FALSE)
    names(newData) = factorPreds

    for (j in seq_along(factorPreds)) {
      newData[[factorPreds[j]]] =
        factor(newData[[factorPreds[j]]], levels = levels(data[[factorPreds[j]]]))
    }
  }

  # --- fitted mean + 95% CI (standard or sandwich) via X V X' ---
  xTerms = delete.response(terms(model))
  X = model.matrix(xTerms, newData)
  beta = coef(model)

  if (ciType == "sandwich") {
    if (!requireNamespace("sandwich", quietly = TRUE)) {
      stop("Package 'sandwich' is required for robust CIs. Please install it.")
    }
    V = sandwich::vcovHC(model, type = hcType)
  } else {
    V = vcov(model)
  }

  eta = as.numeric(X %*% beta)
  seEta = sqrt(as.numeric(diag(X %*% V %*% t(X))))

  crit = qnorm(0.975)

  lowerEta = eta - crit * seEta
  upperEta = eta + crit * seEta

  if (isGlm) {
    invLink = family(model)$linkinv
    fit = invLink(eta)
    lower = invLink(lowerEta)
    upper = invLink(upperEta)
  } else {
    fit = eta
    lower = lowerEta
    upper = upperEta
  }

  fitDf = data.frame(
    group = factor(levs, levels = levs),
    fit = fit,
    lower = lower,
    upper = upper
  )

  # --- plot ---
  p = ggplot(plotDf, aes(x = .data$group, y = .data$y)) +
    labs(x = xLabel, y = responseVar) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 30, hjust = 1)) +
    scale_x_discrete(expand = expansion(mult = c(0.04, 0.30)))

  boxWidth = 0.45
  whiskerCapWidth = 0.22

  if (minGroupSize < 10) {
    p = p + geom_point(
      position = position_jitter(width = 0.15, height = 0),
      alpha = 0.7
    )
  } else {
    p = p +
      geom_boxplot(
        width = boxWidth,
        fill = "lightblue",
        alpha = 0.45,
        outlier.alpha = 0.35,
        linewidth = 0.8
      ) +
      stat_boxplot(
        geom = "errorbar",
        width = whiskerCapWidth,
        linewidth = 0.8
      )
  }

  nudge = position_nudge(x = 0.38)

  p = p +
    geom_errorbar(
      data = fitDf,
      mapping = aes(x = group, ymin = lower, ymax = upper),
      inherit.aes = FALSE,
      position = nudge,
      width = 0.14,
      linewidth = 0.9,
      colour = "firebrick"
    ) +
    geom_point(
      data = fitDf,
      mapping = aes(x = group, y = fit),
      inherit.aes = FALSE,
      position = nudge,
      size = 2.8,
      colour = "firebrick"
    )

  ciLabel =
    if (ciType == "sandwich") {
      paste0("Robust (sandwich) ", hcType, " 95% CI")
    } else {
      "Standard (model-based) 95% CI"
    }

  if (isPoisson) {
    p = p +
      scale_y_continuous(trans = "log1p") +
      labs(subtitle = paste(ciLabel, " | Poisson: log(1 + response) scale", sep = ""))
  } else {
    p = p + labs(subtitle = ciLabel)
  }

  p
}
