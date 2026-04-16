#' Fit a WMFM model and generate command-line outputs
#'
#' Fits a model using a supplied dataset and formula, optionally attaches
#' dataset context, and then attempts to generate fitted equations and a model
#' explanation using the same helper functions used by the app.
#'
#' Supported model types are linear regression, logistic regression, and
#' Poisson regression.
#'
#' Explanation caching can be controlled via `useExplanationCache`. For normal
#' usage this can remain `TRUE`, but when repeatedly querying the language
#' model for the same fitted model it is often useful to set it to `FALSE`
#' so that each run makes a fresh explanation request.
#'
#' The returned object also includes interaction-term names and the minimum
#' interaction-term p-value, which can be used later when evaluating whether
#' the explanation interpreted interaction evidence appropriately.
#'
#' @param data A `data.frame` containing the variables used in the model.
#' @param formula A model formula, either as a formula object or a character
#'   string that can be converted to a formula.
#' @param modelType A character string giving the model family. Must be one
#'   of `"lm"`, `"logistic"`, or `"poisson"`.
#' @param dataContext Optional character string giving additional context
#'   about the dataset, study, variables, coding, or research aim.
#' @param ollamaBaseUrl Optional character string giving the base URL for the
#'   language model service.
#' @param printOutput Logical. If `TRUE`, prints the model summary, fitted
#'   equations, and explanation to the console.
#' @param useExplanationCache Logical. Should cached explanation text be reused
#'   when the same fitted model is encountered? Defaults to `TRUE`.
#' @param equationMethod Character string giving the equation engine. Must be
#'   one of `"llm"` or `"deterministic"`. Defaults to `"llm"`.
#'
#' @return Invisibly returns an object of class `wmfmModel`.
#' @export
runModel = function(
    data,
    formula,
    modelType = c("lm", "logistic", "poisson"),
    dataContext = NULL,
    ollamaBaseUrl = NULL,
    printOutput = TRUE,
    useExplanationCache = TRUE,
    equationMethod = c("llm", "deterministic")
) {

  extractInteractionInfo = function(model) {
    out = list(
      interactionTerms = character(0),
      interactionMinPValue = NA_real_
    )

    coefficientTable = tryCatch(
      summary(model)$coefficients,
      error = function(e) {
        NULL
      }
    )

    if (is.null(coefficientTable) || !is.matrix(coefficientTable)) {
      return(out)
    }

    termNames = rownames(coefficientTable)

    if (is.null(termNames) || length(termNames) == 0) {
      return(out)
    }

    interactionIdx = grepl(":", termNames, fixed = TRUE)

    if (!any(interactionIdx)) {
      return(out)
    }

    interactionTerms = termNames[interactionIdx]
    pColumnIdx = grep("^Pr\\(", colnames(coefficientTable))

    interactionMinPValue = NA_real_

    if (length(pColumnIdx) >= 1) {
      interactionPValues = suppressWarnings(
        as.numeric(coefficientTable[interactionIdx, pColumnIdx[1]])
      )
      interactionPValues = interactionPValues[!is.na(interactionPValues)]

      if (length(interactionPValues) > 0) {
        interactionMinPValue = min(interactionPValues)
      }
    }

    list(
      interactionTerms = interactionTerms,
      interactionMinPValue = interactionMinPValue
    )
  }

  modelType = match.arg(modelType)
  equationMethod = match.arg(equationMethod)

  if (!is.data.frame(data)) {
    stop("`data` must be a data.frame.", call. = FALSE)
  }

  if (is.character(formula)) {
    formula = stats::as.formula(formula)
  }

  if (!inherits(formula, "formula")) {
    stop(
      "`formula` must be a formula or a character string that can be converted to one.",
      call. = FALSE
    )
  }

  if (!is.logical(printOutput) || length(printOutput) != 1 || is.na(printOutput)) {
    stop("`printOutput` must be TRUE or FALSE.", call. = FALSE)
  }

  if (!is.logical(useExplanationCache) || length(useExplanationCache) != 1 || is.na(useExplanationCache)) {
    stop("`useExplanationCache` must be TRUE or FALSE.", call. = FALSE)
  }

  allVars = all.vars(formula)
  missingVars = setdiff(allVars, names(data))

  if (length(missingVars) > 0) {
    stop(
      "Unknown variable(s) in formula: ",
      paste(missingVars, collapse = ", "),
      call. = FALSE
    )
  }

  responseName = allVars[1]
  predictorNames = unique(setdiff(allVars, responseName))

  if (length(predictorNames) > 3) {
    stop(
      "This app only allows models with at most 3 covariates. Your formula uses ",
      length(predictorNames),
      ": ",
      paste(predictorNames, collapse = ", "),
      call. = FALSE
    )
  }

  dataModel = data

  for (varName in predictorNames) {
    if (is.character(dataModel[[varName]])) {
      dataModel[[varName]] = factor(dataModel[[varName]])
    }
  }

  response = dataModel[[responseName]]

  model = switch(
    modelType,
    lm = {
      if (is.factor(response) && nlevels(response) == 2) {
        responseLevels = levels(response)
        dataModel[[responseName]] = as.numeric(response == responseLevels[2])
      }

      stats::lm(formula, data = dataModel)
    },
    logistic = {
      if (is.character(response)) {
        distinctValues = unique(stats::na.omit(response))

        if (length(distinctValues) == 2) {
          dataModel[[responseName]] = factor(response)
          response = dataModel[[responseName]]
        } else {
          stop(
            "Logistic regression requires a binary response. ",
            responseName,
            " has ",
            length(distinctValues),
            " distinct character values.",
            call. = FALSE
          )
        }
      }

      if (is.factor(response)) {
        if (nlevels(response) != 2) {
          stop(
            "Logistic regression requires a 2-level factor response. ",
            responseName,
            " has ",
            nlevels(response),
            " levels.",
            call. = FALSE
          )
        }
      } else if (is.numeric(response)) {
        distinctValues = unique(stats::na.omit(response))

        if (!all(distinctValues %in% c(0, 1))) {
          stop(
            "Numeric logistic responses must be coded 0/1. ",
            responseName,
            " has values outside {0, 1}.",
            call. = FALSE
          )
        }
      } else {
        stop(
          "Logistic regression requires a binary factor, numeric 0/1, or a 2-level character response.",
          call. = FALSE
        )
      }

      stats::glm(
        formula,
        data = dataModel,
        family = stats::binomial(link = "logit")
      )
    },
    poisson = {
      nonMissingResponse = stats::na.omit(response)

      if (any(nonMissingResponse < 0) || any(nonMissingResponse %% 1 != 0)) {
        warning(
          "Response has negative or non-integer values. Poisson regression expects non-negative counts.",
          call. = FALSE
        )
      }

      stats::glm(
        formula,
        data = dataModel,
        family = stats::poisson(link = "log")
      )
    }
  )

  if (!is.null(dataContext)) {
    dataContext = trimws(dataContext)

    if (nzchar(dataContext)) {
      dataContextEscaped = gsub("\"", "\\\\\"", dataContext, fixed = TRUE)
      attr(model, "wmfm_dataset_doc") = dataContextEscaped
      attr(model, "wmfm_dataset_name") = "Debug data"

      if (exists("resolveResponseNounPhrase", mode = "function")) {
        nounPhrase = resolveResponseNounPhrase(model, responseName)
        attr(model, "wmfm_response_noun_phrase") = nounPhrase
      }
    }
  }

  if (!is.null(ollamaBaseUrl)) {
    options(wmfm.ollama_base_url = ollamaBaseUrl)
  }

  interactionInfo = extractInteractionInfo(model)

  equations = NULL
  explanation = NULL

  equations = tryCatch(
    getModelEquations(
      model = model,
      method = equationMethod
    ),
    error = function(e) {
      if (identical(equationMethod, "deterministic")) {
        warning(
          "Deterministic equation generation failed: ",
          conditionMessage(e),
          call. = FALSE
        )
      }

      NULL
    }
  )

  chatProvider = tryCatch(
    getChatProvider(),
    error = function(e) {
      warning(
        "Could not connect to the language model server. Returning fitted model only. Details: ",
        conditionMessage(e),
        call. = FALSE
      )
      NULL
    }
  )

  if (!is.null(chatProvider)) {
    if (identical(equationMethod, "llm")) {
      equations = tryCatch(
        getModelEquations(
          model = model,
          method = "llm",
          chat = chatProvider
        ),
        error = function(e) {
          warning("Equation generation failed: ", conditionMessage(e), call. = FALSE)
          NULL
        }
      )
    }

    explanation = tryCatch(
      lmExplanation(
        model = model,
        chat = chatProvider,
        useCache = useExplanationCache
      ),
      error = function(e) {
        warning("Explanation generation failed: ", conditionMessage(e), call. = FALSE)
        NULL
      }
    )
  }

  output = newWmfmModel(
    model = model,
    formula = formula,
    modelType = modelType,
    data = dataModel,
    dataContext = attr(model, "wmfm_dataset_doc", exact = TRUE),
    equations = equations,
    explanation = explanation,
    interactionTerms = interactionInfo$interactionTerms,
    interactionMinPValue = interactionInfo$interactionMinPValue,
    meta = list(
      useExplanationCache = useExplanationCache,
      ollamaBaseUrl = ollamaBaseUrl,
      sourceFunction = "runModel",
      equationMethod = equationMethod
    )
  )

  if (isTRUE(printOutput)) {
    cat("\n====================\n")
    cat("Model summary\n")
    cat("====================\n\n")
    print(summary(model))

    cat("\n====================\n")
    cat("Equations\n")
    cat("====================\n\n")

    if (is.null(equations)) {
      cat("No equations generated.\n")
    } else {
      print(equations)
    }

    cat("\n====================\n")
    cat("Explanation\n")
    cat("====================\n\n")

    if (is.null(explanation)) {
      cat("No explanation generated.\n")
    } else {
      cat(explanation, "\n")
    }
  }

  invisible(output)
}
