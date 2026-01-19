#' Draw the fitted-model plot (data + fitted line / fitted means)
#'
#' Extracts the plotting logic from the Shiny \code{renderPlot()} block and
#' returns either:
#' \itemize{
#'   \item a \code{ggplot} object (most models), or
#'   \item the result of \code{makeFactorOnlyPlot()} for factor-only models, or
#'   \item draws a base-graphics message and returns \code{invisible()} when a plot is not applicable.
#' }
#'
#' This function expects the helper functions \code{isFactorOnlyModel()} and
#' \code{makeFactorOnlyPlot()} to exist in your app codebase (as they do in your
#' current server implementation).
#'
#' If the model contains exactly one numeric predictor, the function draws the
#' raw data and a fitted line. Optionally, it can add confidence bands around the
#' fitted line(s). Bands can be computed using either the model-based standard
#' errors or sandwich/robust standard errors.
#'
#' @param model A fitted \code{lm} or \code{glm} object.
#' @param ciType Confidence interval type. Typically \code{"standard"} or
#'   \code{"sandwich"}. For factor-only models, this is passed through to
#'   \code{makeFactorOnlyPlot()}.
#' @param hcType Robust (sandwich) type used when \code{ciType = "sandwich"}.
#'   Common values include \code{"HC0"} and \code{"HC3"}.
#' @param showCi Logical; if \code{TRUE}, draw confidence bands around fitted
#'   line(s) when a numeric-predictor plot is available.
#' @param level Confidence level for intervals. Default is \code{0.95}.
#'
#' @return A \code{ggplot} object when a numeric-predictor plot is available, or
#'   whatever \code{makeFactorOnlyPlot()} returns for factor-only models. In cases
#'   where plotting is not applicable, the function draws an informative message
#'   using base graphics and returns \code{invisible()}.
#'
#' @examples
#' \dontrun{
#' m = lm(mpg ~ wt, data = mtcars)
#' p = drawModelPlot(m, showCi = TRUE)
#' print(p)
#'
#' m2 = glm(vs ~ wt, data = mtcars, family = binomial())
#' p2 = drawModelPlot(m2, showCi = TRUE, ciType = "sandwich", hcType = "HC3")
#' print(p2)
#' }
#'
#' @importFrom stats model.frame predict na.omit
#' @importFrom graphics plot.new text
#' @importFrom ggplot2 ggplot geom_point geom_line geom_ribbon labs aes vars facet_wrap
#' @importFrom ggplot2 scale_y_continuous
#' @importFrom rlang .data
#'
#' @keywords internal
drawModelPlot = function(model, ciType = "standard", hcType = "HC0", showCi = FALSE, level = 0.95) {

  stopifnot(!is.null(model))

  m = model

  modelFrame = model.frame(m)
  response   = names(modelFrame)[1]
  predictors = names(modelFrame)[-1]

  # Identify numeric predictors
  numericMask  = sapply(modelFrame[predictors], is.numeric)
  numericPreds = predictors[numericMask]

  # If zero numeric predictors:
  # - If predictors are factors, show grouped plot (boxplot or jitter)
  # - Otherwise, show a message
  if (length(numericPreds) == 0) {
    if (isFactorOnlyModel(m, modelFrame)) {
      return(makeFactorOnlyPlot(
        model  = m,
        data   = modelFrame,
        ciType = ciType,
        hcType = hcType
      ))
    }

    plot.new()
    text(
      0.5, 0.5,
      "No numeric predictors available to plot.",
      cex = 1.2
    )
    return(invisible())
  }

  # If more than one numeric predictor: do not plot, show message
  if (length(numericPreds) > 1) {
    plot.new()
    text(
      0.5, 0.5,
      "Plot is only available when there is a single numeric predictor in the model.",
      cex = 1.2
    )
    return(invisible())
  }

  xVar = numericPreds[1]

  # Factor predictors: used for colour / faceting (for non-binomial)
  factorMask  = sapply(modelFrame[predictors], is.factor)
  factorPreds = predictors[factorMask]

  facetVar  = NULL
  colourVar = NULL

  if (length(factorPreds) == 1) {
    colourVar = factorPreds[1]
  } else if (length(factorPreds) == 2) {
    levCounts = vapply(
      factorPreds,
      function(v) nlevels(modelFrame[[v]]),
      FUN.VALUE = integer(1)
    )

    if (levCounts[1] < levCounts[2]) {
      facetVar  = factorPreds[1]
      colourVar = factorPreds[2]
    } else if (levCounts[2] < levCounts[1]) {
      facetVar  = factorPreds[2]
      colourVar = factorPreds[1]
    } else {
      facetVar  = factorPreds[2]
      colourVar = factorPreds[1]
    }
  }

  # --- Set up y for plotting (special handling for binomial glm) ---
  y       = modelFrame[[response]]
  isGlm   = inherits(m, "glm")
  isBinom = isGlm && identical(m$family$family, "binomial")
  yPlot   = y
  yBreaks = NULL
  yLabels = NULL

  # For binomial models, we build:
  #  - .yPlot      : numeric 0/1 on the y-axis
  #  - .respFactor : factor used for point colours
  if (isBinom) {
    respFactor = NULL

    if (is.factor(y)) {
      levs = levels(y)
      if (length(levs) == 2) {
        eventLevel = levs[2]
        yPlot      = as.numeric(y == eventLevel)
        yBreaks    = c(0, 1)
        yLabels    = levs
        respFactor = y
      } else {
        yPlot      = as.numeric(y)
        respFactor = factor(y)
      }
    } else if (is.logical(y)) {
      yPlot      = as.numeric(y)
      yBreaks    = c(0, 1)
      yLabels    = c("FALSE", "TRUE")
      respFactor = factor(y, levels = c(FALSE, TRUE))
    } else if (is.numeric(y)) {
      yPlot = y
      uy    = sort(unique(na.omit(yPlot)))
      if (identical(uy, c(0, 1))) {
        yBreaks    = c(0, 1)
        yLabels    = c("0", "1")
        respFactor = factor(yPlot, levels = c(0, 1), labels = yLabels)
      } else {
        respFactor = factor(yPlot)
      }
    } else {
      fac  = factor(y)
      levs = levels(fac)
      if (length(levs) == 2) {
        eventLevel = levs[2]
        yPlot      = as.numeric(fac == eventLevel)
        yBreaks    = c(0, 1)
        yLabels    = levs
        respFactor = fac
      } else {
        yPlot      = as.numeric(fac)
        respFactor = fac
      }
    }

    modelFrame$.yPlot      = yPlot
    modelFrame$.respFactor = respFactor
  }

  # Build grid for fitted lines
  xSeq = seq(
    min(modelFrame[[xVar]], na.rm = TRUE),
    max(modelFrame[[xVar]], na.rm = TRUE),
    length.out = 100
  )

  gridList = list()
  gridList[[xVar]] = xSeq

  # Use all factor predictors in the grid (if any)
  if (length(factorPreds) > 0) {
    for (v in factorPreds) {
      gridList[[v]] = levels(modelFrame[[v]])
    }
  }

  # For any other predictors, hold them at a typical value
  otherPreds = setdiff(predictors, c(xVar, factorPreds))
  for (v in otherPreds) {
    x = modelFrame[[v]]
    if (is.numeric(x)) {
      gridList[[v]] = mean(x, na.rm = TRUE)
    } else if (is.factor(x)) {
      gridList[[v]] = levels(x)[1]
    }
  }

  newData = expand.grid(gridList, stringsAsFactors = FALSE)

  # Predictions (and optional confidence intervals)
  if (isTRUE(showCi)) {
    newData = computeMeanCi(
      model   = m,
      newData = newData,
      ciType  = ciType,
      hcType  = hcType,
      level   = level
    )
  } else {
    if (isGlm) {
      newData$fit = predict(m, newdata = newData, type = "response")
    } else {
      newData$fit = predict(m, newdata = newData)
    }
  }

  # ----- Build plot -----

  addCiRibbon = function(p, hasGroups = FALSE) {
    if (!isTRUE(showCi)) {
      return(p)
    }
    if (hasGroups) {
      return(p + geom_ribbon(
        data = newData,
        mapping = aes(
          x = .data[[xVar]],
          ymin = .data[["lower"]],
          ymax = .data[["upper"]],
          group = .data[[colourVar]]
        ),
        alpha = 0.2,
        inherit.aes = FALSE
      ))
    }

    p + geom_ribbon(
      data = newData,
      mapping = aes(
        x = .data[[xVar]],
        ymin = .data[["lower"]],
        ymax = .data[["upper"]]
      ),
      alpha = 0.2,
      inherit.aes = FALSE
    )
  }

  if (isBinom) {
    if (is.null(colourVar)) {
      p = ggplot(
        data    = modelFrame,
        mapping = aes(
          x      = .data[[xVar]],
          y      = .data[[".yPlot"]],
          colour = .data[[".respFactor"]]
        )
      ) +
        geom_point(alpha = 0.6)

      p = addCiRibbon(p, hasGroups = FALSE)

      p = p +
        geom_line(
          data    = newData,
          mapping = aes(
            x = .data[[xVar]],
            y = .data[["fit"]]
          ),
          linewidth = 1
        ) +
        labs(
          x      = xVar,
          y      = response,
          colour = response
        )
    } else {
      p = ggplot(
        data    = modelFrame,
        mapping = aes(
          x      = .data[[xVar]],
          y      = .data[[".yPlot"]],
          colour = .data[[".respFactor"]],
          shape  = .data[[colourVar]]
        )
      ) +
        geom_point(alpha = 0.6)

      p = addCiRibbon(p, hasGroups = TRUE)

      p = p +
        geom_line(
          data    = newData,
          mapping = aes(
            x     = .data[[xVar]],
            y     = .data[["fit"]],
            group = .data[[colourVar]]
          ),
          linewidth = 1
        ) +
        labs(
          x      = xVar,
          y      = response,
          colour = response,
          shape  = colourVar
        )
    }

    if (!is.null(facetVar)) {
      p = p + facet_wrap(vars(.data[[facetVar]]))
    }

  } else {

    if (is.null(colourVar)) {
      p = ggplot(
        data    = modelFrame,
        mapping = aes(
          x = .data[[xVar]],
          y = .data[[response]]
        )
      ) +
        geom_point(alpha = 0.6)

      p = addCiRibbon(p, hasGroups = FALSE)

      p = p +
        geom_line(
          data    = newData,
          mapping = aes(
            x = .data[[xVar]],
            y = .data[["fit"]]
          ),
          linewidth = 1
        ) +
        labs(x = xVar, y = response)

    } else {
      p = ggplot(
        data    = modelFrame,
        mapping = aes(
          x      = .data[[xVar]],
          y      = .data[[response]],
          colour = .data[[colourVar]]
        )
      ) +
        geom_point(alpha = 0.6)

      p = addCiRibbon(p, hasGroups = TRUE)

      p = p +
        geom_line(
          data    = newData,
          mapping = aes(
            x      = .data[[xVar]],
            y      = .data[["fit"]],
            colour = .data[[colourVar]]
          ),
          linewidth = 1
        ) +
        labs(
          x      = xVar,
          y      = response,
          colour = colourVar
        )

      if (!is.null(facetVar)) {
        p = p + facet_wrap(vars(.data[[facetVar]]))
      }
    }
  }

  # For binomial models, force y-scale to 0-1 with nice labels
  if (isBinom && !is.null(yBreaks) && !is.null(yLabels)) {
    p = p + scale_y_continuous(
      breaks = yBreaks,
      labels = yLabels,
      limits = c(0, 1)
    )
  }

  p
}
