#' Fit a WMFM model and generate console outputs without launching Shiny
#'
#' Fits a model using a supplied dataset and formula, optionally attaches
#' dataset context, and then attempts to generate fitted equations and a
#' model explanation using the same helper functions used by the app.
#'
#' Supported model types are linear regression, logistic regression, and
#' Poisson regression.
#'
#' Explanation caching can be controlled via `useExplanationCache`. For normal
#' usage this can remain `TRUE`, but when repeatedly querying the language
#' model for the same fitted model it is often useful to set it to `FALSE`
#' so that each run makes a fresh explanation request.
#'
#' @param data A `data.frame` containing the variables used in the model.
#' @param formula A model formula, either as a formula object or a character
#'   string that can be converted to a formula.
#' @param modelType A character string giving the model family. Must be one
#'   of `"lm"`, `"logistic"`, or `"poisson"`.
#' @param dataContext Optional character string giving additional context
#'   about the dataset, study, variables, coding, or research aim.
#' @param ollamaBaseUrl Optional character string giving the base URL for
#'   the language model service.
#' @param printOutput Logical. If `TRUE`, prints the model summary, fitted
#'   equations, and explanation to the console.
#' @param useExplanationCache Logical. Should cached explanation text be reused
#'   when the same fitted model is encountered? Defaults to `TRUE`.
#'
#' @return Invisibly returns a list with components:
#' \describe{
#'   \item{`model`}{The fitted model object.}
#'   \item{`equations`}{The result of `lmEquations()`, or `NULL` if equation
#'   generation failed or no chat provider was available.}
#'   \item{`explanation`}{The result of `lmExplanation()`, or `NULL` if
#'   explanation generation failed or no chat provider was available.}
#'   \item{`datasetContext`}{The dataset context attached to the model, or
#'   `NULL` if none was supplied.}
#' }
#'
#' @examples
#' \dontrun{
#' runWMFMModelDebug(
#'   data = course.df,
#'   formula = Exam ~ Attend + Test,
#'   modelType = "lm"
#' )
#'
#' runWMFMModelDebug(
#'   data = course.df,
#'   formula = Exam ~ Attend + Test,
#'   modelType = "lm",
#'   useExplanationCache = FALSE
#' )
#' }
#'
#' @export
runWMFMModelDebug = function(
    data,
    formula,
    modelType = c("lm", "logistic", "poisson"),
    dataContext = NULL,
    ollamaBaseUrl = NULL,
    printOutput = TRUE,
    useExplanationCache = TRUE
) {

  modelType = match.arg(modelType)

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

  equations = NULL
  explanation = NULL

  if (!is.null(chatProvider)) {
    equations = tryCatch(
      lmEquations(model, chatProvider),
      error = function(e) {
        warning("Equation generation failed: ", conditionMessage(e), call. = FALSE)
        NULL
      }
    )

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

  output = list(
    model = model,
    equations = equations,
    explanation = explanation,
    datasetContext = attr(model, "wmfm_dataset_doc", exact = TRUE)
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
