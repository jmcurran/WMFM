#' Describe the fitted model for student explanation insertions
#'
#' @param model A fitted model object.
#'
#' @return A named list describing the available scales.
#'
#' @keywords internal
studentExplanationModelContext = function(model) {
  context = list(
    family = "linear",
    coefficientScales = c("Coefficient" = "coefficient"),
    meanScales = c("Fitted mean" = "response"),
    predictionScales = c("Predicted individual value" = "response"),
    differenceScales = c("Difference in fitted means" = "responseDifference")
  )

  if (!inherits(model, "glm")) {
    return(context)
  }

  familyName = tryCatch(model$family$family, error = function(e) "")
  linkName = tryCatch(model$family$link, error = function(e) "")

  if (identical(familyName, "binomial") && identical(linkName, "logit")) {
    context$family = "binomial"
    context$coefficientScales = c(
      "Log-odds coefficient" = "link",
      "Odds ratio" = "ratio"
    )
    context$meanScales = c(
      "Probability" = "response",
      "Odds" = "odds",
      "Log odds" = "link"
    )
    context$predictionScales = c(
      "Predicted probability" = "response",
      "Predicted odds" = "odds",
      "Predicted log odds" = "link"
    )
    context$differenceScales = c(
      "Probability difference" = "responseDifference",
      "Odds ratio" = "ratio",
      "Log-odds difference" = "linkDifference"
    )
  } else if (identical(familyName, "poisson") && identical(linkName, "log")) {
    context$family = "poisson"
    context$coefficientScales = c(
      "Log-count coefficient" = "link",
      "Expected-count ratio" = "ratio"
    )
    context$meanScales = c(
      "Expected count" = "response",
      "Log expected count" = "link"
    )
    context$predictionScales = c(
      "Predicted count" = "response",
      "Predicted log count" = "link"
    )
    context$differenceScales = c(
      "Expected-count difference" = "responseDifference",
      "Expected-count ratio" = "ratio",
      "Log expected-count difference" = "linkDifference"
    )
  }

  context
}

#' Format a model term for student-facing controls
#'
#' @param term Character vector of model term labels.
#'
#' @return A character vector of readable labels.
#'
#' @keywords internal
formatStudentExplanationTerm = function(term) {
  term = as.character(term)
  term = gsub("`", "", term, fixed = TRUE)
  term[term == "(Intercept)"] = "the intercept"
  term = gsub("log1p\\(([^)]+)\\)", "log-one-plus \\1", term)
  term = gsub("log\\(([^)]+)\\)", "log-transformed \\1", term)
  term = gsub("sqrt\\(([^)]+)\\)", "square-root transformed \\1", term)
  term = gsub("I\\(([^)]+)\\^2\\)", "\\1 squared", term)
  term = gsub(":", " interacting with ", term, fixed = TRUE)
  term
}

#' Format a number for the student explanation editor
#'
#' @param value Numeric vector.
#'
#' @return A character vector.
#'
#' @keywords internal
formatStudentExplanationNumber = function(value) {
  formatC(value, digits = 4, format = "fg", flag = "#")
}

#' Build labels for analysed observations
#'
#' @param model A fitted model object.
#' @param maxRows Maximum number of rows shown in a selector.
#'
#' @return A named integer vector.
#'
#' @keywords internal
buildStudentExplanationObservationChoices = function(model, maxRows = 100L) {
  modelData = tryCatch(stats::model.frame(model), error = function(e) NULL)
  if (is.null(modelData) || nrow(modelData) == 0) {
    return(integer(0))
  }

  rowIndices = seq_len(nrow(modelData))
  if (length(rowIndices) > maxRows) {
    rowIndices = unique(round(seq(1, length(rowIndices), length.out = maxRows)))
  }

  predictorNames = names(modelData)[-1]
  labels = vapply(
    rowIndices,
    function(index) {
      details = vapply(
        utils::head(predictorNames, 3),
        function(variableName) {
          paste0(variableName, " = ", as.character(modelData[[variableName]][[index]]))
        },
        character(1)
      )
      paste0("Observation ", index, if (length(details) > 0) paste0(": ", paste(details, collapse = ", ")) else "")
    },
    character(1)
  )
  stats::setNames(rowIndices, labels)
}

studentExplanationConfidenceLabel = function(level) {
  paste0(round(100 * level), "%")
}

studentExplanationInsert = function(session, text) {
  if (!is.null(text) && length(text) > 0 && !is.na(text[[1]]) && nzchar(text[[1]])) {
    session$sendCustomMessage(
      type = "wmfmInsertStudentExplanation",
      message = list(text = text[[1]])
    )
  }
  invisible(NULL)
}

studentExplanationCoefficientFragment = function(model, term, scale, includeInterval, level) {
  estimates = stats::coef(model)
  if (!(term %in% names(estimates))) {
    stop("The selected coefficient is no longer available.", call. = FALSE)
  }

  estimate = estimates[[term]]
  interval = tryCatch(stats::confint.default(model, level = level)[term, ], error = function(e) NULL)
  label = formatStudentExplanationTerm(term)
  context = studentExplanationModelContext(model)

  if (identical(scale, "ratio")) {
    estimate = exp(estimate)
    if (!is.null(interval)) {
      interval = exp(interval)
    }
    measure = if (identical(context$family, "binomial")) "odds ratio" else "expected-count ratio"
  } else {
    measure = if (identical(context$family, "linear")) "coefficient" else names(context$coefficientScales)[match(scale, context$coefficientScales)]
    measure = tolower(measure)
  }

  text = paste0("the estimated ", measure, " for ", label, " is ", formatStudentExplanationNumber(estimate))
  if (isTRUE(includeInterval) && !is.null(interval)) {
    text = paste0(
      text, ", with a ", studentExplanationConfidenceLabel(level),
      " confidence interval from ", formatStudentExplanationNumber(interval[[1]]),
      " to ", formatStudentExplanationNumber(interval[[2]])
    )
  }
  text
}

studentExplanationPredictionSummary = function(model, observation, level) {
  modelData = stats::model.frame(model)
  newData = modelData[observation, , drop = FALSE]
  responseName = names(modelData)[1]
  newData[[responseName]] = NULL
  prediction = stats::predict(model, newdata = newData, type = "link", se.fit = TRUE)
  criticalValue = stats::qnorm(1 - (1 - level) / 2)
  list(
    link = as.numeric(prediction$fit),
    linkLower = as.numeric(prediction$fit - criticalValue * prediction$se.fit),
    linkUpper = as.numeric(prediction$fit + criticalValue * prediction$se.fit)
  )
}

studentExplanationMeanFragment = function(model, newData, scale, includeInterval, level) {
  if (is.numeric(newData) && length(newData) == 1L) {
    modelData = stats::model.frame(model)
    newData = modelData[newData, , drop = FALSE]
    responseName = names(modelData)[1]
    newData[[responseName]] = NULL
  }

  if (!is.data.frame(newData) && !is.list(newData)) {
    stop("Prediction profiles must be supplied as a data frame or list.", call. = FALSE)
  }

  if (inherits(model, "glm")) {
    prediction = stats::predict(model, newdata = newData, type = "link", se.fit = TRUE)
    criticalValue = stats::qnorm(1 - (1 - level) / 2)
    link = as.numeric(prediction$fit)
    linkLower = link - criticalValue * as.numeric(prediction$se.fit)
    linkUpper = link + criticalValue * as.numeric(prediction$se.fit)
    inverseLink = model$family$linkinv
  } else {
    prediction = stats::predict(model, newdata = newData, interval = "confidence", level = level)
    link = as.numeric(prediction[, "fit"])
    linkLower = as.numeric(prediction[, "lwr"])
    linkUpper = as.numeric(prediction[, "upr"])
    inverseLink = identity
  }

  context = studentExplanationModelContext(model)

  if (identical(scale, "response")) {
    estimates = inverseLink(link)
    lower = inverseLink(linkLower)
    upper = inverseLink(linkUpper)
    measure = if (identical(context$family, "binomial")) "predicted probability" else if (identical(context$family, "poisson")) "expected count" else "expected value"
  } else if (identical(scale, "odds")) {
    estimates = exp(link)
    lower = exp(linkLower)
    upper = exp(linkUpper)
    measure = "fitted odds"
  } else {
    estimates = link
    lower = linkLower
    upper = linkUpper
    measure = if (identical(context$family, "binomial")) "fitted log odds" else if (identical(context$family, "poisson")) "fitted log expected count" else "expected value"
  }

  fragments = vapply(seq_along(estimates), function(index) {
    profile = formatStudentExplanationPredictionProfile(newData, index)
    text = paste0("for ", profile, ", the ", measure, " is ", formatStudentExplanationNumber(estimates[[index]]))
    if (isTRUE(includeInterval)) {
      text = paste0(text, ", with a ", studentExplanationConfidenceLabel(level), " confidence interval from ", formatStudentExplanationNumber(lower[[index]]), " to ", formatStudentExplanationNumber(upper[[index]]))
    }
    text
  }, character(1))

  paste(fragments, collapse = "; ")
}


getStudentExplanationPredictionVariables = function(model, inputPrefix = "studentPredictionValue") {
  predictorNames = all.vars(stats::delete.response(stats::terms(model)))
  sourceData = tryCatch(
    eval(model$call$data, envir = environment(stats::formula(model))),
    error = function(e) NULL
  )
  modelData = tryCatch(stats::model.frame(model), error = function(e) NULL)

  lapply(seq_along(predictorNames), function(index) {
    variableName = predictorNames[[index]]
    values = NULL
    if (is.data.frame(sourceData) && variableName %in% names(sourceData)) {
      values = sourceData[[variableName]]
    } else if (is.data.frame(modelData) && variableName %in% names(modelData)) {
      values = modelData[[variableName]]
    }

    factorLevels = model$xlevels[[variableName]] %||% character(0)
    isFactor = length(factorLevels) > 0 || is.factor(values) || is.character(values)
    if (length(factorLevels) == 0 && isFactor) {
      factorLevels = unique(as.character(values[!is.na(values)]))
    }

    defaultValue = if (isFactor) {
      if (length(factorLevels) == 0) "" else factorLevels[[1]]
    } else {
      numericValues = suppressWarnings(as.numeric(values))
      numericValues = numericValues[is.finite(numericValues)]
      if (length(numericValues) == 0) "" else formatStudentExplanationNumber(stats::median(numericValues))
    }

    list(
      name = variableName,
      inputId = paste0(inputPrefix, index),
      isFactor = isFactor,
      levels = factorLevels,
      default = defaultValue
    )
  })
}

parseStudentExplanationPredictionValues = function(model, inputValues, inputPrefix = "studentPredictionValue") {
  specifications = getStudentExplanationPredictionVariables(model, inputPrefix = inputPrefix)
  parsedValues = vector("list", length(specifications))
  names(parsedValues) = vapply(specifications, function(x) x$name, character(1))

  for (index in seq_along(specifications)) {
    specification = specifications[[index]]
    rawValue = inputValues[[specification$inputId]] %||% ""
    if (specification$isFactor) {
      values = as.character(rawValue)
      values = trimws(values[nzchar(trimws(values))])
      invalidValues = setdiff(values, specification$levels)
      if (length(invalidValues) > 0) {
        return(list(
          ok = FALSE,
          message = paste0(
            specification$name, " contains an unavailable level: ",
            paste(invalidValues, collapse = ", "), "."
          )
        ))
      }
    } else {
      characterValues = trimws(unlist(strsplit(as.character(rawValue), ",", fixed = TRUE)))
      characterValues = characterValues[nzchar(characterValues)]
      values = suppressWarnings(as.numeric(characterValues))
      if (length(characterValues) == 0 || any(!is.finite(values))) {
        return(list(
          ok = FALSE,
          message = paste0(
            "Enter one or more numeric values for ", specification$name,
            ", separated by commas."
          )
        ))
      }
    }

    if (length(values) == 0) {
      return(list(ok = FALSE, message = paste0("Supply at least one value for ", specification$name, ".")))
    }
    parsedValues[[specification$name]] = values
  }

  lengths = vapply(parsedValues, length, integer(1))
  predictionCount = max(lengths)
  invalidLengths = lengths[lengths != 1L & lengths != predictionCount]
  if (length(invalidLengths) > 0) {
    details = paste0(names(lengths), " has ", lengths, ifelse(lengths == 1L, " value", " values"))
    return(list(
      ok = FALSE,
      message = paste0(
        "Each covariate must contain either one value or ", predictionCount,
        " values. ", paste(details, collapse = "; "), "."
      )
    ))
  }

  broadcastValues = lapply(parsedValues, function(values) {
    if (length(values) == 1L) rep(values, predictionCount) else values
  })
  newData = as.data.frame(broadcastValues, stringsAsFactors = FALSE, check.names = FALSE)

  for (specification in specifications) {
    if (specification$isFactor) {
      newData[[specification$name]] = factor(
        newData[[specification$name]],
        levels = specification$levels
      )
    }
  }

  list(
    ok = TRUE,
    newData = newData,
    predictionCount = predictionCount,
    suppliedValues = parsedValues
  )
}

formatStudentExplanationPredictionProfile = function(newData, rowIndex) {
  details = vapply(
    names(newData),
    function(variableName) {
      paste0(variableName, " = ", as.character(newData[[variableName]][[rowIndex]]))
    },
    character(1)
  )
  paste(details, collapse = ", ")
}

studentExplanationPredictionFragment = function(
    model,
    newData,
    scale,
    includeInterval,
    level,
    wording = "prediction"
) {
  context = studentExplanationModelContext(model)

  if (identical(context$family, "linear")) {
    prediction = stats::predict(
      model,
      newdata = newData,
      interval = if (isTRUE(includeInterval)) "prediction" else "none",
      level = level
    )

    if (isTRUE(includeInterval)) {
      estimates = as.numeric(prediction[, "fit"])
      lower = as.numeric(prediction[, "lwr"])
      upper = as.numeric(prediction[, "upr"])
    } else {
      estimates = as.numeric(prediction)
      lower = upper = rep(NA_real_, length(estimates))
    }

    measure = if (identical(wording, "typical")) "typical value" else "predicted individual value"
  } else {
    linkPredictions = as.numeric(stats::predict(model, newdata = newData, type = "link"))
    if (identical(scale, "response")) {
      estimates = model$family$linkinv(linkPredictions)
      measure = if (identical(context$family, "binomial")) "predicted probability" else "predicted count"
    } else if (identical(scale, "odds")) {
      estimates = exp(linkPredictions)
      measure = "predicted odds"
    } else {
      estimates = linkPredictions
      measure = if (identical(context$family, "binomial")) "predicted log odds" else "predicted log count"
    }
    lower = upper = rep(NA_real_, length(estimates))
  }

  fragments = vapply(
    seq_along(estimates),
    function(index) {
      profile = formatStudentExplanationPredictionProfile(newData, index)
      text = paste0(
        "for ", profile, ", the ", measure, " is ",
        formatStudentExplanationNumber(estimates[[index]])
      )
      if (isTRUE(includeInterval) && is.finite(lower[[index]]) && is.finite(upper[[index]])) {
        text = paste0(
          text, ", with a ", studentExplanationConfidenceLabel(level),
          " prediction interval from ", formatStudentExplanationNumber(lower[[index]]),
          " to ", formatStudentExplanationNumber(upper[[index]])
        )
      }
      text
    },
    character(1)
  )

  paste(fragments, collapse = "; ")
}

studentExplanationDifferenceFragment = function(model, firstObservation, secondObservation, scale, includeInterval, level) {
  first = studentExplanationPredictionSummary(model, firstObservation, level)
  second = studentExplanationPredictionSummary(model, secondObservation, level)
  modelData = stats::model.frame(model)
  responseName = names(modelData)[1]
  newData = modelData[c(firstObservation, secondObservation), , drop = FALSE]
  newData[[responseName]] = NULL
  modelMatrix = stats::model.matrix(stats::delete.response(stats::terms(model)), newData)
  contrast = modelMatrix[2, , drop = TRUE] - modelMatrix[1, , drop = TRUE]
  linkDifference = sum(contrast * stats::coef(model))
  standardError = sqrt(drop(t(contrast) %*% stats::vcov(model) %*% contrast))
  criticalValue = stats::qnorm(1 - (1 - level) / 2)
  linkInterval = linkDifference + c(-1, 1) * criticalValue * standardError
  context = studentExplanationModelContext(model)

  if (identical(scale, "ratio")) {
    estimate = exp(linkDifference)
    interval = exp(linkInterval)
    measure = if (identical(context$family, "binomial")) "odds ratio" else "expected-count ratio"
  } else if (identical(scale, "linkDifference")) {
    estimate = linkDifference
    interval = linkInterval
    measure = if (identical(context$family, "binomial")) "log-odds difference" else "log expected-count difference"
  } else {
    inverseLink = if (inherits(model, "glm")) model$family$linkinv else identity
    estimate = inverseLink(second$link) - inverseLink(first$link)
    interval = NULL
    measure = if (identical(context$family, "binomial")) "probability difference" else if (identical(context$family, "poisson")) "expected-count difference" else "difference in fitted means"
  }

  text = paste0("the estimated ", measure, " comparing observation ", secondObservation, " with observation ", firstObservation, " is ", formatStudentExplanationNumber(estimate))
  if (isTRUE(includeInterval) && !is.null(interval)) {
    text = paste0(text, ", with a ", studentExplanationConfidenceLabel(level), " confidence interval from ", formatStudentExplanationNumber(interval[[1]]), " to ", formatStudentExplanationNumber(interval[[2]]))
  }
  text
}

studentExplanationResidualFragment = function(model, observation, residualType) {
  residualValues = stats::residuals(model, type = residualType)
  paste0(
    "the ", residualType, " residual for observation ", observation,
    " is ", formatStudentExplanationNumber(residualValues[[observation]])
  )
}

buildStudentExplanationToolbarUi = function(model) {
  disabled = is.null(model)
  buttonClass = "btn-default wmfm-statistical-insert-button"
  shiny::tags$div(
    class = "wmfm-student-explanation-toolbar",
    title = if (disabled) "Fit a model to enable statistical insertion tools." else NULL,
    shiny::actionButton("openStudentCoefficientDialog", shiny::HTML("&beta;&#770;"), class = buttonClass, disabled = disabled, title = "Insert a coefficient"),
    shiny::actionButton("openStudentMeanDialog", shiny::HTML("&mu;&#770;"), class = buttonClass, disabled = disabled, title = "Insert a fitted mean or fitted response"),
    shiny::actionButton("openStudentPredictionDialog", shiny::HTML("y&#770;"), class = buttonClass, disabled = disabled, title = "Insert an individual prediction"),
    shiny::actionButton("openStudentDifferenceDialog", shiny::HTML("&Delta;&#770;"), class = buttonClass, disabled = disabled, title = "Insert a pairwise difference or ratio"),
    shiny::actionButton("openStudentResidualDialog", shiny::HTML("&epsilon;&#770;"), class = buttonClass, disabled = disabled, title = "Insert a residual"),
    shiny::actionButton("openStudentOtherDialog", shiny::HTML("&hellip;"), class = buttonClass, disabled = disabled, title = "Insert another model statistic")
  )
}

buildStudentExplanationCoefficientDialog = function(model, confidenceLevel) {
  context = studentExplanationModelContext(model)
  shiny::modalDialog(
    title = "Insert a coefficient",
    shiny::selectInput("studentCoefficientTerm", "Coefficient", choices = stats::setNames(names(stats::coef(model)), formatStudentExplanationTerm(names(stats::coef(model))))),
    shiny::selectInput("studentCoefficientScale", "Scale", choices = context$coefficientScales),
    shiny::checkboxInput("studentCoefficientInterval", paste0("Include a ", studentExplanationConfidenceLabel(confidenceLevel), " confidence interval"), TRUE),
    footer = shiny::tagList(shiny::modalButton("Cancel"), shiny::actionButton("insertStudentCoefficient", "Insert", class = "btn-primary")),
    easyClose = TRUE
  )
}

buildStudentExplanationMeanDialog = function(model, confidenceLevel) {
  context = studentExplanationModelContext(model)
  specifications = getStudentExplanationPredictionVariables(model, inputPrefix = "studentMeanValue")
  predictorControls = lapply(specifications, function(specification) {
    if (specification$isFactor) {
      shiny::selectizeInput(
        specification$inputId,
        specification$name,
        choices = specification$levels,
        selected = specification$default,
        multiple = TRUE,
        options = list(plugins = list("remove_button"))
      )
    } else {
      shiny::textInput(
        specification$inputId,
        specification$name,
        value = specification$default,
        placeholder = "Enter one or more values separated by commas"
      )
    }
  })
  shiny::modalDialog(
    title = "Insert a fitted mean or fitted response",
    predictorControls,
    shiny::helpText("Supply one value or a common number of values for each covariate. Single values are repeated across the fitted-mean profiles."),
    shiny::uiOutput("studentMeanPreviewUi"),
    shiny::selectInput("studentMeanScale", "Scale", choices = context$meanScales),
    shiny::checkboxInput("studentMeanInterval", paste0("Include a ", studentExplanationConfidenceLabel(confidenceLevel), " confidence interval"), TRUE),
    footer = shiny::tagList(shiny::modalButton("Cancel"), shiny::actionButton("insertStudentMean", "Insert", class = "btn-primary")),
    easyClose = TRUE
  )
}


buildStudentExplanationPredictionDialog = function(model, confidenceLevel) {
  context = studentExplanationModelContext(model)
  isLinear = identical(context$family, "linear")
  specifications = getStudentExplanationPredictionVariables(model)

  predictorControls = lapply(specifications, function(specification) {
    if (specification$isFactor) {
      shiny::selectizeInput(
        specification$inputId,
        specification$name,
        choices = specification$levels,
        selected = specification$default,
        multiple = TRUE,
        options = list(plugins = list("remove_button"))
      )
    } else {
      shiny::textInput(
        specification$inputId,
        specification$name,
        value = specification$default,
        placeholder = "Enter one or more values separated by commas"
      )
    }
  })

  controls = c(
    predictorControls,
    list(
      shiny::helpText(
        "Supply one value or a common number of values for each covariate. Single values are repeated across the prediction profiles."
      ),
      shiny::uiOutput("studentPredictionPreviewUi"),
      shiny::selectInput(
        "studentPredictionScale",
        "Prediction scale",
        choices = context$predictionScales
      )
    )
  )

  if (isLinear) {
    controls = c(
      controls,
      list(
        shiny::selectInput(
          "studentPredictionWording",
          "Wording",
          choices = c(
            "Predicted individual value" = "prediction",
            "Typical value" = "typical"
          )
        ),
        shiny::checkboxInput(
          "studentPredictionInterval",
          paste0(
            "Include a ",
            studentExplanationConfidenceLabel(confidenceLevel),
            " prediction interval"
          ),
          TRUE
        )
      )
    )
  } else if (identical(context$family, "binomial")) {
    controls = c(
      controls,
      list(
        shiny::helpText(
          "A future binary outcome is zero or one, so WMFM inserts its predicted probability or odds rather than a prediction interval."
        )
      )
    )
  } else {
    controls = c(
      controls,
      list(
        shiny::helpText(
          "This stage inserts a predicted count but does not yet calculate a predictive interval for a future Poisson count."
        )
      )
    )
  }

  shiny::modalDialog(
    title = "Insert an individual prediction",
    controls,
    footer = shiny::tagList(
      shiny::modalButton("Cancel"),
      shiny::actionButton(
        "insertStudentPrediction",
        "Insert",
        class = "btn-primary"
      )
    ),
    easyClose = TRUE
  )
}

buildStudentExplanationDifferenceDialog = function(model, confidenceLevel) {
  context = studentExplanationModelContext(model)
  choices = buildStudentExplanationObservationChoices(model)
  shiny::modalDialog(
    title = "Insert a pairwise comparison",
    shiny::selectInput("studentDifferenceFirst", "Reference observation", choices = choices),
    shiny::selectInput("studentDifferenceSecond", "Comparison observation", choices = choices, selected = if (length(choices) > 1) choices[[2]] else choices[[1]]),
    shiny::selectInput("studentDifferenceScale", "Result scale", choices = context$differenceScales),
    shiny::checkboxInput("studentDifferenceInterval", paste0("Include a ", studentExplanationConfidenceLabel(confidenceLevel), " confidence interval when available"), TRUE),
    shiny::helpText("A response-scale difference currently omits an interval when the transformation makes a simple interval misleading."),
    footer = shiny::tagList(shiny::modalButton("Cancel"), shiny::actionButton("insertStudentDifference", "Insert", class = "btn-primary")),
    easyClose = TRUE
  )
}

buildStudentExplanationResidualDialog = function(model) {
  residualChoices = if (inherits(model, "glm")) c("Deviance residual" = "deviance", "Pearson residual" = "pearson", "Response residual" = "response") else c("Ordinary residual" = "response", "Pearson residual" = "pearson")
  shiny::modalDialog(
    title = "Insert a residual",
    shiny::selectInput("studentResidualObservation", "Observation", choices = buildStudentExplanationObservationChoices(model)),
    shiny::selectInput("studentResidualType", "Residual type", choices = residualChoices),
    footer = shiny::tagList(shiny::modalButton("Cancel"), shiny::actionButton("insertStudentResidual", "Insert", class = "btn-primary")),
    easyClose = TRUE
  )
}

buildStudentExplanationOtherDialog = function(model, confidenceLevel) {
  choices = c("Number of analysed observations" = "n")
  if (inherits(model, "lm") && !inherits(model, "glm")) {
    choices = c(choices, "R-squared" = "rSquared", "Adjusted R-squared" = "adjustedRSquared", "Residual standard deviation" = "residualSd")
  } else {
    choices = c(choices, "AIC" = "aic", "Residual deviance" = "deviance")
  }
  shiny::modalDialog(
    title = "Other model quantities",
    shiny::selectInput("studentOtherStatistic", "Quantity", choices = choices),
    shiny::selectInput("studentExplanationConfidenceLevel", "Confidence level used by insertion dialogs", choices = c("90%" = 0.90, "95%" = 0.95, "99%" = 0.99), selected = confidenceLevel),
    footer = shiny::tagList(shiny::modalButton("Cancel"), shiny::actionButton("insertStudentOther", "Insert", class = "btn-primary")),
    easyClose = TRUE
  )
}

studentExplanationOtherFragment = function(model, statistic) {
  modelSummary = summary(model)
  switch(
    statistic,
    n = paste0("the model was fitted using ", stats::nobs(model), " observations"),
    rSquared = paste0("the model R-squared is ", formatStudentExplanationNumber(modelSummary$r.squared)),
    adjustedRSquared = paste0("the adjusted R-squared is ", formatStudentExplanationNumber(modelSummary$adj.r.squared)),
    residualSd = paste0("the residual standard deviation is ", formatStudentExplanationNumber(modelSummary$sigma)),
    aic = paste0("the model AIC is ", formatStudentExplanationNumber(stats::AIC(model))),
    deviance = paste0("the residual deviance is ", formatStudentExplanationNumber(stats::deviance(model))),
    ""
  )
}
#' Build formative feedback for a student explanation
#'
#' Converts a scored `wmfmGrade` object into a compact feedback structure for
#' the student workspace. The result focuses on strengths and revision
#' priorities rather than exposing the full developer scoring record.
#'
#' @param gradeObj A scored `wmfmGrade` object.
#' @param method Character scalar naming the scoring method.
#'
#' @return A named list containing the score, strengths, and revision priorities.
#'
#' @keywords internal
buildStudentExplanationFeedback = function(
    gradeObj,
    method = "deterministic"
) {
  if (!inherits(gradeObj, "wmfmGrade")) {
    return(NULL)
  }

  methodScore = gradeObj$scores$byMethod[[method]]
  methodFeedback = gradeObj$feedback$byMethod[[method]]

  if (is.null(methodScore) || is.null(methodFeedback)) {
    return(NULL)
  }

  friendlyMessage = function(metric, defaultText, kind = c("strength", "priority")) {
    kind = match.arg(kind)

    strengthMessages = c(
      effectDirectionCorrect = "You described the direction of the relationship correctly.",
      effectScaleAppropriate = "You interpreted the effects on the correct scale.",
      mainEffectCoverageAdequate = "You explained the main relationships in the model.",
      numericExpressionAdequate = "You used the numerical results appropriately.",
      uncertaintyHandlingAppropriate = "You included and interpreted uncertainty appropriately.",
      referenceGroupHandledCorrectly = "You identified the comparison group clearly.",
      comparisonStructureClear = "You made the important comparison clear.",
      clarityAdequate = "Your explanation was clear and easy to follow.",
      calibrationScore = "Your conclusions were appropriately cautious.",
      inferentialRegisterAppropriate = "You matched your wording to what the model can support."
    )

    priorityMessages = c(
      calibrationScore = "Use cautious association wording and avoid suggesting that the model proves a cause-and-effect relationship.",
      inferentialRegisterAppropriate = "Replace strong causal claims with wording such as 'is associated with', 'is estimated to', or 'the results suggest'.",
      clarityAdequate = "Proofread the explanation and simplify any awkward sentences so that each idea is easy to follow.",
      clarityScore = "A careful proofread would make the explanation smoother and easier to read.",
      comparisonStructureClear = "State comparisons directly and name both groups or quantities being compared.",
      referenceGroupHandledCorrectly = "Name the reference group explicitly when describing a difference between groups.",
      referenceGroupCoverageAdequate = "Explain which group is the reference group so that the attendance comparison is unambiguous.",
      inferenceScore = "Check that each conclusion is no stronger than the evidence and uncertainty allow.",
      completenessScore = "Add the missing model result that is most relevant to the research question.",
      factualScore = "Check the numerical statements and make sure each one describes the correct model quantity."
    )

    messages = if (identical(kind, "strength")) strengthMessages else priorityMessages
    if (!is.na(metric) && metric %in% names(messages)) {
      return(unname(messages[[metric]]))
    }

    defaultText
  }

  extractFeedbackItems = function(x, kind = c("strength", "priority")) {
    kind = match.arg(kind)
    if (!is.data.frame(x) || nrow(x) == 0) {
      return(character(0))
    }

    metrics = if ("metric" %in% names(x)) as.character(x$metric) else rep(NA_character_, nrow(x))
    preferredColumns = c("comment", "reason", "message", "feedback", "detail", "label")
    textColumn = preferredColumns[preferredColumns %in% names(x)][1]
    defaults = if (length(textColumn) == 0 || is.na(textColumn)) {
      rep("", nrow(x))
    } else {
      trimws(as.character(x[[textColumn]]))
    }

    keep = !is.na(metrics) & metrics != "overallScore"
    if (all(is.na(metrics))) {
      keep = rep(TRUE, nrow(x))
    }

    items = vapply(
      which(keep),
      function(i) friendlyMessage(metrics[[i]], defaults[[i]], kind),
      character(1)
    )
    items = trimws(items)
    unique(items[nzchar(items)])
  }

  strengths = extractFeedbackItems(methodFeedback$strengths, "strength")
  priorities = extractFeedbackItems(methodFeedback$whereMarksLost, "priority")

  if (length(priorities) == 0) {
    priorities = extractFeedbackItems(methodFeedback$missingElements, "priority")
  }

  # Keep the student panel concise. Developer diagnostics retain the complete
  # scoring record when more detail is required.
  strengths = head(strengths, 4L)
  priorities = head(priorities, 3L)

  list(
    overallScore = suppressWarnings(as.numeric(methodScore$overallScore)[1]),
    mark = suppressWarnings(as.numeric(methodScore$mark)[1]),
    scoreScale = suppressWarnings(as.numeric(gradeObj$scoreScale)[1]),
    strengths = strengths,
    priorities = priorities
  )
}

#' Render formative student explanation feedback
#'
#' @param feedback A feedback structure returned by
#'   `buildStudentExplanationFeedback()`.
#'
#' @return A Shiny tag list, or `NULL` when no feedback is available.
#'
#' @keywords internal
renderStudentExplanationFeedbackUi = function(feedback) {
  if (is.null(feedback)) {
    return(NULL)
  }

  renderItems = function(items, emptyText) {
    if (length(items) == 0) {
      return(shiny::tags$p(emptyText))
    }

    shiny::tags$ul(
      lapply(items, shiny::tags$li)
    )
  }

  scoreText = ""
  if (is.finite(feedback$overallScore)) {
    scoreText = paste0(round(feedback$overallScore), "%")
  }

  shiny::tags$div(
    class = "wmfm-student-explanation-feedback",
    shiny::tags$h4("Feedback on your explanation"),
    if (nzchar(scoreText)) {
      shiny::tags$p(
        class = "wmfm-student-explanation-score",
        paste("Current rubric score:", scoreText)
      )
    },
    shiny::tags$h5("What you did well"),
    renderItems(
      feedback$strengths,
      "The current rubric has not identified a specific strength yet. Add a clear statement about what the model shows."
    ),
    shiny::tags$h5("What you need to revise/improve"),
    renderItems(
      feedback$priorities,
      "No important revisions were identified. A final proofread may still improve the writing."
    ),
    shiny::tags$p(
      class = "wmfm-student-explanation-feedback-note",
      "Revise your explanation and check it again. This feedback does not reveal WMFM's model explanation."
    )
  )
}


buildStudentExplanationProfilePreviewUi = function(model, input, inputPrefix, profileLabel) {
  specifications = getStudentExplanationPredictionVariables(model, inputPrefix = inputPrefix)
  inputValues = stats::setNames(
    lapply(specifications, function(specification) input[[specification$inputId]]),
    vapply(specifications, function(specification) specification$inputId, character(1))
  )
  parsed = parseStudentExplanationPredictionValues(model, inputValues, inputPrefix = inputPrefix)
  if (!isTRUE(parsed$ok)) {
    return(shiny::tags$p(class = "text-danger", parsed$message))
  }
  previewRows = lapply(seq_len(min(parsed$predictionCount, 6L)), function(index) {
    shiny::tags$li(formatStudentExplanationPredictionProfile(parsed$newData, index))
  })
  if (parsed$predictionCount > 6L) {
    previewRows = c(previewRows, list(shiny::tags$li(paste0("... and ", parsed$predictionCount - 6L, " more"))))
  }
  shiny::tags$div(
    class = "wmfm-student-prediction-preview",
    shiny::tags$strong(paste0(parsed$predictionCount, " ", if (parsed$predictionCount == 1L) profileLabel else paste0(profileLabel, "s"))),
    shiny::tags$ol(previewRows)
  )
}

buildStudentExplanationDiagnosticReport = function(model, rv, input, explanationText, gradeObj) {
  modelFrame = tryCatch(stats::model.frame(model), error = function(e) NULL)
  variableLines = if (is.data.frame(modelFrame)) {
    vapply(names(modelFrame), function(name) {
      value = modelFrame[[name]]
      details = if (is.factor(value)) {
        paste0("factor; levels: ", paste(levels(value), collapse = ", "))
      } else {
        class(value)[[1]]
      }
      paste0("- ", name, ": ", details)
    }, character(1))
  } else {
    "- unavailable"
  }

  markdownTable = function(x) {
    if (is.null(x) || !is.data.frame(x) || nrow(x) == 0L) {
      return("_None recorded._")
    }
    x = as.data.frame(x, stringsAsFactors = FALSE)
    x[] = lapply(x, function(value) {
      value = as.character(value)
      value[is.na(value)] = ""
      gsub("|", "\\|", value, fixed = TRUE)
    })
    header = paste0("| ", paste(names(x), collapse = " | "), " |")
    separator = paste0("| ", paste(rep("---", ncol(x)), collapse = " | "), " |")
    rows = apply(x, 1L, function(row) paste0("| ", paste(row, collapse = " | "), " |"))
    paste(c(header, separator, rows), collapse = "\n")
  }

  coefficientTable = tryCatch({
    matrix = summary(model)$coefficients
    data.frame(term = rownames(matrix), matrix, row.names = NULL, check.names = FALSE)
  }, error = function(e) NULL)

  metricTable = NULL
  lossTable = NULL
  semanticTable = NULL
  fatalLines = "- No fatal flaw was recorded."
  parsedLines = "_No grading result is currently available._"

  if (!is.null(gradeObj) && !inherits(gradeObj, "wmfmGrade")) {
    parsedLines = paste(capture.output(str(gradeObj, max.level = 3L, give.attr = FALSE)), collapse = "\n")
  }

  if (inherits(gradeObj, "wmfmGrade")) {
    metricTable = gradeObj$metricSummary
    lossTable = gradeObj$feedback$whereMarksLost
    semanticTable = gradeObj$feedback$semanticEvidence
    studentScore = gradeObj$student
    fatal = isTRUE(studentScore$fatalFlawDetected[[1]])
    overclaim = isTRUE(studentScore$overclaimDetected[[1]])
    fatalLines = c(
      paste0("- Fatal flaw detected: ", fatal),
      paste0("- Overclaim detected: ", overclaim),
      paste0("- Fatal-flaw cap: ", gradeObj$meta$fatalFlawCap %||% NA),
      paste0("- Overall score: ", gradeObj$overallScore %||% NA)
    )
    parsedFields = c(
      "overclaimDetected", "inferentialRegister", "uncertaintyTypeClaim",
      "followupPredictionTypeClaim", "followupIntervalTypeClaim",
      "comparisonLanguageMention", "referenceGroupMention", "wordCount"
    )
    availableFields = intersect(parsedFields, names(studentScore))
    parsedTable = data.frame(
      field = availableFields,
      value = vapply(availableFields, function(field) as.character(studentScore[[field]][[1]]), character(1)),
      stringsAsFactors = FALSE
    )
    parsedLines = markdownTable(parsedTable)
  }

  researchQuestion = attr(model, "wmfm_research_question", exact = TRUE)
  if (is.null(researchQuestion) && !is.null(rv)) {
    researchQuestion = rv$researchQuestion
  }
  researchQuestion = researchQuestion %||% "Unavailable"

  paste(
    "# WMFM student explanation diagnostic report",
    "",
    "This compact developer report omits the underlying data, fitted values, residual vectors, QR decomposition, and complete fitted-model object.",
    "",
    "## Analysis context",
    paste0("- Model class: ", paste(class(model), collapse = ", ")),
    paste0("- Formula: `", paste(deparse(stats::formula(model)), collapse = " "), "`"),
    paste0("- Analysed observations: ", stats::nobs(model)),
    paste0("- Research question: ", researchQuestion),
    "",
    "### Variables",
    paste(variableLines, collapse = "\n"),
    "",
    "### Coefficients supplied to grading",
    markdownTable(coefficientTable),
    "",
    "## Student explanation",
    explanationText,
    "",
    "## Fatal-flaw and cap trace",
    paste(fatalLines, collapse = "\n"),
    "",
    "## Parsed explanation fields",
    parsedLines,
    "",
    "## Metric scores",
    markdownTable(metricTable),
    "",
    "## Where marks were lost",
    markdownTable(lossTable),
    "",
    "## Semantic evidence",
    markdownTable(semanticTable),
    sep = "\n"
  )
}

#' Register student explanation workspace observers
#'
#' @param input Shiny input object.
#' @param output Shiny output object.
#' @param session Shiny session object.
#' @param rv App reactive values object.
#' @param modelFit Reactive fitted model holder.
#' @param developerModeUnlocked Reactive value indicating whether developer mode is unlocked.
#'
#' @return Invisible `NULL`.
#'
#' @keywords internal
registerStudentExplanationObservers = function(input, output, session, rv, modelFit, developerModeUnlocked) {
  studentExplanationGrade = shiny::reactiveVal(NULL)
  studentExplanationStatus = shiny::reactiveVal("")
  confidenceLevel = shiny::reactiveVal(0.95)

  output$studentExplanationToolbarUi = shiny::renderUI({
    buildStudentExplanationToolbarUi(modelFit())
  })
  output$studentExplanationFeedbackStatus = shiny::renderText(studentExplanationStatus())
  output$studentExplanationFeedbackUi = shiny::renderUI({
    renderStudentExplanationFeedbackUi(buildStudentExplanationFeedback(studentExplanationGrade()))
  })
  output$studentPredictionPreviewUi = shiny::renderUI({
    model = modelFit()
    if (is.null(model)) {
      return(NULL)
    }
    buildStudentExplanationProfilePreviewUi(model, input, "studentPredictionValue", "prediction profile")
  })
  output$studentMeanPreviewUi = shiny::renderUI({
    model = modelFit()
    if (is.null(model)) {
      return(NULL)
    }
    buildStudentExplanationProfilePreviewUi(model, input, "studentMeanValue", "fitted-mean profile")
  })
  output$studentExplanationDeveloperDiagnosticsUi = shiny::renderUI({
    if (!isTRUE(developerModeUnlocked())) {
      return(NULL)
    }
    shiny::tags$details(
      shiny::tags$summary("Developer explanation diagnostics"),
      shiny::helpText("Download a compact Markdown report containing the model context, exact student text, parsed evidence, score trace, and fatal-flaw information. The underlying data are omitted."),
      shiny::downloadButton("studentExplanationDiagnosticDownload", "Download diagnostic report")
    )
  })
  output$studentExplanationDiagnosticDownload = shiny::downloadHandler(
    filename = function() paste0("wmfm-explanation-diagnostic-", format(Sys.Date(), "%Y-%m-%d"), ".md"),
    content = function(file) {
      if (!isTRUE(developerModeUnlocked())) {
        stop("Developer mode is required.", call. = FALSE)
      }
      model = modelFit()
      shiny::req(model)
      explanationText = as.character(input$studentExplanationText %||% "")[[1]]
      report = buildStudentExplanationDiagnosticReport(model, rv, input, explanationText, studentExplanationGrade())
      writeLines(report, file, useBytes = TRUE)
    }
  )

  shiny::observeEvent(input$openStudentCoefficientDialog, {
    shiny::req(modelFit())
    shiny::showModal(buildStudentExplanationCoefficientDialog(modelFit(), confidenceLevel()))
  })
  shiny::observeEvent(input$openStudentMeanDialog, {
    shiny::req(modelFit())
    shiny::showModal(buildStudentExplanationMeanDialog(modelFit(), confidenceLevel()))
  })
  shiny::observeEvent(input$openStudentPredictionDialog, {
    shiny::req(modelFit())
    shiny::showModal(buildStudentExplanationPredictionDialog(modelFit(), confidenceLevel()))
  })
  shiny::observeEvent(input$openStudentDifferenceDialog, {
    shiny::req(modelFit())
    shiny::showModal(buildStudentExplanationDifferenceDialog(modelFit(), confidenceLevel()))
  })
  shiny::observeEvent(input$openStudentResidualDialog, {
    shiny::req(modelFit())
    shiny::showModal(buildStudentExplanationResidualDialog(modelFit()))
  })
  shiny::observeEvent(input$openStudentOtherDialog, {
    shiny::req(modelFit())
    shiny::showModal(buildStudentExplanationOtherDialog(modelFit(), confidenceLevel()))
  })

  shiny::observeEvent(input$insertStudentCoefficient, {
    text = studentExplanationCoefficientFragment(modelFit(), input$studentCoefficientTerm, input$studentCoefficientScale, isTRUE(input$studentCoefficientInterval), confidenceLevel())
    shiny::removeModal()
    studentExplanationInsert(session, text)
  })
  shiny::observeEvent(input$insertStudentMean, {
    model = modelFit()
    specifications = getStudentExplanationPredictionVariables(model, inputPrefix = "studentMeanValue")
    inputValues = stats::setNames(
      lapply(specifications, function(specification) input[[specification$inputId]]),
      vapply(specifications, function(specification) specification$inputId, character(1))
    )
    parsed = parseStudentExplanationPredictionValues(model, inputValues, inputPrefix = "studentMeanValue")
    if (!isTRUE(parsed$ok)) {
      shiny::showNotification(parsed$message, type = "error", duration = NULL)
      return(NULL)
    }
    text = tryCatch(
      studentExplanationMeanFragment(model, parsed$newData, input$studentMeanScale, isTRUE(input$studentMeanInterval), confidenceLevel()),
      error = function(e) {
        shiny::showNotification(paste("WMFM could not calculate these fitted means:", conditionMessage(e)), type = "error", duration = NULL)
        NULL
      }
    )
    if (is.null(text)) {
      return(NULL)
    }
    shiny::removeModal()
    studentExplanationInsert(session, text)
  })
  shiny::observeEvent(input$insertStudentPrediction, {
    model = modelFit()
    specifications = getStudentExplanationPredictionVariables(model)
    inputValues = stats::setNames(
      lapply(specifications, function(specification) input[[specification$inputId]]),
      vapply(specifications, function(specification) specification$inputId, character(1))
    )
    parsed = parseStudentExplanationPredictionValues(model, inputValues)
    if (!isTRUE(parsed$ok)) {
      shiny::showNotification(parsed$message, type = "error", duration = NULL)
      return(NULL)
    }
    text = tryCatch(
      studentExplanationPredictionFragment(
        model,
        parsed$newData,
        input$studentPredictionScale,
        isTRUE(input$studentPredictionInterval),
        confidenceLevel(),
        input$studentPredictionWording %||% "prediction"
      ),
      error = function(e) {
        shiny::showNotification(
          paste("WMFM could not calculate these predictions:", conditionMessage(e)),
          type = "error",
          duration = NULL
        )
        NULL
      }
    )
    if (is.null(text)) {
      return(NULL)
    }
    shiny::removeModal()
    studentExplanationInsert(session, text)
  })
  shiny::observeEvent(input$insertStudentDifference, {
    text = studentExplanationDifferenceFragment(modelFit(), as.integer(input$studentDifferenceFirst), as.integer(input$studentDifferenceSecond), input$studentDifferenceScale, isTRUE(input$studentDifferenceInterval), confidenceLevel())
    shiny::removeModal()
    studentExplanationInsert(session, text)
  })
  shiny::observeEvent(input$insertStudentResidual, {
    text = studentExplanationResidualFragment(modelFit(), as.integer(input$studentResidualObservation), input$studentResidualType)
    shiny::removeModal()
    studentExplanationInsert(session, text)
  })
  shiny::observeEvent(input$insertStudentOther, {
    selectedLevel = suppressWarnings(as.numeric(input$studentExplanationConfidenceLevel))
    if (is.finite(selectedLevel)) {
      confidenceLevel(selectedLevel)
    }
    text = studentExplanationOtherFragment(modelFit(), input$studentOtherStatistic)
    shiny::removeModal()
    studentExplanationInsert(session, text)
  })

  shiny::observeEvent(input$checkStudentExplanation, {
    model = modelFit()
    explanationText = trimws(as.character(input$studentExplanationText %||% ""))[1]
    if (is.null(model)) {
      studentExplanationGrade(NULL)
      studentExplanationStatus("Fit a model before checking your explanation.")
      return(NULL)
    }
    if (!nzchar(explanationText)) {
      studentExplanationGrade(NULL)
      studentExplanationStatus("Write an explanation before asking WMFM to check it.")
      return(NULL)
    }
    studentExplanationStatus("Checking your explanation...")
    scoredGrade = tryCatch(
      scoreDeveloperExplanation(model = model, rv = rv, input = input, explanationText = explanationText, method = "deterministic"),
      error = function(e) {
        studentExplanationStatus(paste("WMFM could not check the explanation:", conditionMessage(e)))
        NULL
      }
    )
    studentExplanationGrade(scoredGrade)
    if (!is.null(scoredGrade)) {
      studentExplanationStatus("Explanation checked. Revise it using the feedback below, then check it again.")
    }
  })

  shiny::observeEvent(input$studentExplanationText, {
    if (!is.null(studentExplanationGrade())) {
      studentExplanationGrade(NULL)
      studentExplanationStatus("Your explanation has changed. Check it again for updated feedback.")
    }
  }, ignoreInit = TRUE)

  shiny::observeEvent(modelFit(), {
    shiny::updateTextAreaInput(session, "studentExplanationText", value = "")
    studentExplanationGrade(NULL)
    studentExplanationStatus("")
  }, ignoreInit = TRUE)

  invisible(NULL)
}
