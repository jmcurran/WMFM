#' Resolve a fitted model object for model-plot helpers
#'
#' @param model A fitted model object or a \code{wmfmModel} object.
#'
#' @return A fitted model object.
#' @keywords internal
resolveModelPlotModel = function(model) {

  if (inherits(model, "wmfmModel")) {
    return(model$model)
  }

  model
}

#' Classify the fitted model for model plots
#'
#' @param model A fitted model object or a \code{wmfmModel} object.
#'
#' @return A single character string describing the supported plot family.
#' @keywords internal
classifyModelPlotFamily = function(model) {

  model = resolveModelPlotModel(model)

  if (inherits(model, "lm") && !inherits(model, "glm")) {
    return("lm")
  }

  if (inherits(model, "glm")) {
    familyName = model$family$family %||% ""

    if (identical(familyName, "binomial")) {
      return("binomial")
    }

    if (identical(familyName, "poisson")) {
      return("poisson")
    }
  }

  "unsupported"
}

#' Build available model-plot choices
#'
#' @param model A fitted model object or a \code{wmfmModel} object.
#'
#' @return A named character vector suitable for \code{selectInput()}.
#' @keywords internal
buildModelPlotTypeChoices = function(model) {

  plotFamily = classifyModelPlotFamily(model)

  if (identical(plotFamily, "lm")) {
    return(c(
      "Observed vs fitted" = "observedFitted",
      "Residuals vs fitted" = "residualFitted"
    ))
  }

  if (identical(plotFamily, "binomial")) {
    return(c(
      "Observed outcome vs predicted probability" = "observedFitted",
      "Deviance residuals vs fitted probability" = "residualFitted"
    ))
  }

  if (identical(plotFamily, "poisson")) {
    return(c(
      "Observed count vs fitted count" = "observedFitted",
      "Deviance residuals vs fitted count" = "residualFitted"
    ))
  }

  c("No model plots available" = "unsupported")
}

#' Convert a model response to a plotting response
#'
#' @param response Model response vector.
#' @param plotFamily Model-plot family from \code{classifyModelPlotFamily()}.
#'
#' @return A list with \code{values}, \code{breaks}, and \code{labels}.
#' @keywords internal
convertModelPlotResponse = function(response, plotFamily) {

  if (!identical(plotFamily, "binomial")) {
    return(list(
      values = as.numeric(response),
      breaks = NULL,
      labels = NULL
    ))
  }

  if (is.factor(response)) {
    levels = levels(response)

    if (length(levels) == 2) {
      return(list(
        values = as.numeric(response == levels[2]),
        breaks = c(0, 1),
        labels = levels
      ))
    }
  }

  if (is.logical(response)) {
    return(list(
      values = as.numeric(response),
      breaks = c(0, 1),
      labels = c("FALSE", "TRUE")
    ))
  }

  values = as.numeric(response)
  uniqueValues = sort(unique(na.omit(values)))

  if (identical(uniqueValues, c(0, 1))) {
    return(list(
      values = values,
      breaks = c(0, 1),
      labels = c("0", "1")
    ))
  }

  list(
    values = values,
    breaks = NULL,
    labels = NULL
  )
}

#' Build deterministic model-plot data
#'
#' @param model A fitted \code{lm}, binomial \code{glm}, poisson \code{glm}, or
#'   \code{wmfmModel} object containing one of those fitted model objects.
#' @param plotType Plot type. Supported values are \code{"observedFitted"} and
#'   \code{"residualFitted"}.
#'
#' @return A list containing plot metadata and a data frame.
#'
#' @importFrom stats complete.cases fitted model.frame model.response na.omit residuals
#'
#' @export
buildModelPlotData = function(model, plotType = c("observedFitted", "residualFitted")) {

  plotType = match.arg(plotType)
  model = resolveModelPlotModel(model)
  plotFamily = classifyModelPlotFamily(model)

  if (identical(plotFamily, "unsupported")) {
    return(list(
      available = FALSE,
      plotFamily = plotFamily,
      plotType = plotType,
      residualType = NA_character_,
      fittedScale = NA_character_,
      data = data.frame()
    ))
  }

  modelFrame = model.frame(model)
  response = model.response(modelFrame)
  responseValues = convertModelPlotResponse(
    response = response,
    plotFamily = plotFamily
  )

  residualType = if (identical(plotFamily, "lm")) {
    "ordinary"
  } else {
    "deviance"
  }

  fittedScale = switch(
    plotFamily,
    lm = "response",
    binomial = "predicted probability",
    poisson = "fitted count",
    "response"
  )

  residualValues = if (identical(residualType, "ordinary")) {
    residuals(model)
  } else {
    residuals(model, type = "deviance")
  }

  data = data.frame(
    observation = seq_along(responseValues$values),
    observed = as.numeric(responseValues$values),
    fitted = as.numeric(fitted(model)),
    residual = as.numeric(residualValues),
    stringsAsFactors = FALSE
  )

  data = data[complete.cases(data), , drop = FALSE]

  list(
    available = TRUE,
    plotFamily = plotFamily,
    plotType = plotType,
    residualType = residualType,
    fittedScale = fittedScale,
    yBreaks = responseValues$breaks,
    yLabels = responseValues$labels,
    data = data
  )
}

#' Build a short teaching note for a model plot
#'
#' @param model A fitted model object or \code{wmfmModel} object.
#' @param plotType Plot type.
#'
#' @return A named list with short explanatory strings.
#' @keywords internal
buildModelPlotTeachingNote = function(model, plotType = c("observedFitted", "residualFitted")) {

  plotType = match.arg(plotType)
  plotFamily = classifyModelPlotFamily(model)

  if (identical(plotFamily, "unsupported")) {
    return(list(
      title = "Model plots are not available for this fitted model.",
      shows = "This fitted model is not one of the model types currently supported by the Model plots tab.",
      lookFor = "Use the other model output sections to inspect the fitted model.",
      cannotProve = "A plot cannot prove that a model is correct or incorrect."
    ))
  }

  if (identical(plotType, "observedFitted")) {
    if (identical(plotFamily, "binomial")) {
      return(list(
        title = "Observed outcome vs predicted probability",
        shows = "This plot shows each observed binary outcome against the fitted probability from the model.",
        lookFor = "Look for whether larger fitted probabilities tend to line up with more observations at the event level.",
        cannotProve = "Binary outcomes naturally sit at the two outcome levels, so this plot cannot prove that the model is correct."
      ))
    }

    if (identical(plotFamily, "poisson")) {
      return(list(
        title = "Observed count vs fitted count",
        shows = "This plot shows the observed counts against the fitted counts from the model.",
        lookFor = "Look for whether larger fitted counts broadly correspond to larger observed counts, while remembering that counts can be noisy.",
        cannotProve = "This plot can reveal obvious structure the fitted model may be missing, but it cannot certify the model."
      ))
    }

    return(list(
      title = "Observed vs fitted",
      shows = "This plot shows how closely the fitted values track the observed responses.",
      lookFor = "Points close to the reference line have fitted values close to the observed values; strong patterns away from the line may suggest missing structure.",
      cannotProve = "This plot does not prove that the model is correct."
    ))
  }

  if (identical(plotFamily, "binomial")) {
    return(list(
      title = "Deviance residuals vs fitted probability",
      shows = "This plot shows model-based residuals against the fitted probabilities.",
      lookFor = "Look for obvious leftover patterns, clusters, or unusual observations after the model has been fitted.",
      cannotProve = "These residuals are on a model-based scale, not simply observed minus fitted probability, and the plot does not identify the correct alternative model by itself."
    ))
  }

  if (identical(plotFamily, "poisson")) {
    return(list(
      title = "Deviance residuals vs fitted count",
      shows = "This plot shows model-based residuals against the fitted counts.",
      lookFor = "Look for obvious leftover patterns, changes in spread, clusters, or unusual observations.",
      cannotProve = "The plot is intended to reveal obvious leftover patterns, not to certify the model."
    ))
  }

  list(
    title = "Residuals vs fitted",
    shows = "This plot shows what is left over after the fitted model has been used.",
    lookFor = "Residuals scattered around zero with no obvious pattern are generally easier to reconcile with the fitted model; curves, funnels, or clusters may suggest missing structure.",
    cannotProve = "This plot does not identify the correct alternative model by itself."
  )
}

#' Draw a student-facing model plot
#'
#' @param model A fitted model object or \code{wmfmModel} object.
#' @param plotType Plot type.
#'
#' @return A \code{ggplot} object, or \code{NULL} for unsupported models.
#'
#' @importFrom ggplot2 aes geom_abline geom_hline geom_jitter geom_point ggplot labs scale_y_continuous theme_minimal
#' @importFrom rlang .data
#'
#' @export
plotModelPlot = function(model, plotType = c("observedFitted", "residualFitted")) {

  plotType = match.arg(plotType)
  plotData = buildModelPlotData(model = model, plotType = plotType)

  if (!isTRUE(plotData$available)) {
    return(NULL)
  }

  data = plotData$data

  if (identical(plotType, "observedFitted")) {
    yLabel = if (identical(plotData$plotFamily, "binomial")) {
      "Observed outcome"
    } else {
      "Observed response"
    }

    xLabel = switch(
      plotData$plotFamily,
      binomial = "Fitted probability",
      poisson = "Fitted count",
      "Fitted value"
    )

    plot = ggplot(
      data,
      aes(x = .data$fitted, y = .data$observed)
    ) +
      geom_point(alpha = 0.75) +
      geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
      labs(x = xLabel, y = yLabel) +
      theme_minimal()

    if (identical(plotData$plotFamily, "binomial")) {
      plot = ggplot(
        data,
        aes(x = .data$fitted, y = .data$observed)
      ) +
        geom_jitter(height = 0.04, width = 0, alpha = 0.75) +
        labs(x = xLabel, y = yLabel) +
        theme_minimal()

      if (!is.null(plotData$yBreaks) && !is.null(plotData$yLabels)) {
        plot = plot + scale_y_continuous(
          breaks = plotData$yBreaks,
          labels = plotData$yLabels
        )
      }
    }

    return(plot)
  }

  xLabel = switch(
    plotData$plotFamily,
    binomial = "Fitted probability",
    poisson = "Fitted count",
    "Fitted value"
  )

  yLabel = if (identical(plotData$residualType, "deviance")) {
    "Deviance residual"
  } else {
    "Residual"
  }

  ggplot(
    data,
    aes(x = .data$fitted, y = .data$residual)
  ) +
    geom_point(alpha = 0.75) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    labs(x = xLabel, y = yLabel) +
    theme_minimal()
}
