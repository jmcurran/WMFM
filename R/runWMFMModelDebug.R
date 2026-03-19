#' Fit a WMFM model and generate console outputs without launching Shiny
#'
#' @param data A data.frame.
#' @param formula A model formula, either as a formula object or a string.
#' @param modelType One of "lm", "logistic", or "poisson".
#' @param dataContext Optional character string describing the data, variables,
#'   study aim, units, coding, etc. This is passed to the language-model
#'   helpers in the same way as the app's uploaded-data context.
#' @param ollamaBaseUrl Optional base URL for Ollama, e.g. "http://localhost:11434".
#' @param printOutput Logical. If TRUE, print results to the console.
#'
#' @return A list with components:
#'   \item{model}{The fitted model object.}
#'   \item{equations}{Output from lmEquations(), or NULL if unavailable.}
#'   \item{explanation}{Output from lmExplanation(), or NULL if unavailable.}
#'   \item{datasetContext}{The dataset context actually attached to the model.}
#'
#' @export
runWMFMModelDebug = function(
    data,
    formula,
    modelType = c("lm", "logistic", "poisson"),
    dataContext = NULL,
    ollamaBaseUrl = NULL,
    printOutput = TRUE
) {
  modelType = match.arg(modelType)

  if (!is.data.frame(data)) {
    stop("`data` must be a data.frame.")
  }

  if (is.character(formula)) {
    formula = stats::as.formula(formula)
  }

  if (!inherits(formula, "formula")) {
    stop("`formula` must be a formula or a character string that can be converted to one.")
  }

  allVars = all.vars(formula)
  missingVars = setdiff(allVars, names(data))
  if (length(missingVars) > 0) {
    stop(
      "Unknown variable(s) in formula: ",
      paste(missingVars, collapse = ", ")
    )
  }

  respName = allVars[1]
  predNames = unique(setdiff(allVars, respName))

  if (length(predNames) > 3) {
    stop(
      "This app only allows models with at most 3 covariates. ",
      "Your formula uses ", length(predNames), ": ",
      paste(predNames, collapse = ", ")
    )
  }

  dfMod = data

  # Match the app's behaviour: character predictors become factors.
  for (v in predNames) {
    if (is.character(dfMod[[v]])) {
      dfMod[[v]] = factor(dfMod[[v]])
    }
  }

  y = dfMod[[respName]]

  model = switch(
    modelType,
    "lm" = {
      if (is.factor(y) && nlevels(y) == 2) {
        levs = levels(y)
        dfMod[[respName]] = as.numeric(y == levs[2])
      }
      stats::lm(formula, data = dfMod)
    },
    "logistic" = {
      if (is.character(y)) {
        u = unique(stats::na.omit(y))
        if (length(u) == 2) {
          dfMod[[respName]] = factor(y)
          y = dfMod[[respName]]
        } else {
          stop(
            "Logistic regression requires a binary response. ",
            respName, " has ", length(u), " distinct character values."
          )
        }
      }

      if (is.factor(y)) {
        if (nlevels(y) != 2) {
          stop(
            "Logistic regression requires a 2-level factor response. ",
            respName, " has ", nlevels(y), " levels."
          )
        }
      } else if (is.numeric(y)) {
        uy = unique(stats::na.omit(y))
        if (!all(uy %in% c(0, 1))) {
          stop(
            "Numeric logistic responses must be coded 0/1. ",
            respName, " has values outside {0, 1}."
          )
        }
      } else {
        stop(
          "Logistic regression requires a binary factor, numeric 0/1, ",
          "or a 2-level character response."
        )
      }

      stats::glm(
        formula,
        data = dfMod,
        family = stats::binomial(link = "logit")
      )
    },
    "poisson" = {
      if (any(stats::na.omit(y) < 0) || any(stats::na.omit(y) %% 1 != 0)) {
        warning(
          "Response has negative or non-integer values. ",
          "Poisson regression expects non-negative counts."
        )
      }

      stats::glm(
        formula,
        data = dfMod,
        family = stats::poisson(link = "log")
      )
    }
  )

  # Attach optional context in the same style as the app
  if (!is.null(dataContext)) {
    dataContext = trimws(dataContext)

    if (nzchar(dataContext)) {
      dataContextEscaped = gsub("\"", "\\\\\"", dataContext, fixed = TRUE)
      attr(model, "wmfm_dataset_doc") = dataContextEscaped
      attr(model, "wmfm_dataset_name") = "Debug data"

      if (exists("resolveResponseNounPhrase", mode = "function")) {
        nounPhrase = resolveResponseNounPhrase(model, respName)
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
        "Could not connect to the language model server. ",
        "Returning fitted model only. Details: ",
        conditionMessage(e)
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
        warning("Equation generation failed: ", conditionMessage(e))
        NULL
      }
    )

    explanation = tryCatch(
      lmExplanation(model, chatProvider),
      error = function(e) {
        warning("Explanation generation failed: ", conditionMessage(e))
        NULL
      }
    )
  }

  out = list(
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

  invisible(out)
}
