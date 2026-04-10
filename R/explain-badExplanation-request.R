#' Validate a bad explanation generation request
#'
#' @param x A `wmfmModel` object.
#' @param explanation Optional character scalar giving the base explanation.
#' @param type Character vector of bad explanation types, or `"auto"`.
#' @param severity Character scalar.
#' @param n Integer scalar.
#' @param mixTypes Logical scalar.
#' @param labelErrors Logical scalar.
#'
#' @return Invisibly returns `TRUE` when validation succeeds.
#' @keywords internal
validateBadExplanationRequest = function(
    x,
    explanation,
    type,
    severity,
    n,
    mixTypes,
    labelErrors
) {

  if (!inherits(x, "wmfmModel")) {
    stop("`x` must inherit from `wmfmModel`.", call. = FALSE)
  }

  if (!is.null(explanation)) {
    if (!is.character(explanation) || length(explanation) != 1L || is.na(explanation)) {
      stop(
        "`explanation` must be NULL or a single non-missing character string.",
        call. = FALSE
      )
    }
  }

  if (!is.character(type) || length(type) < 1L || anyNA(type)) {
    stop("`type` must be a non-empty character vector.", call. = FALSE)
  }

  if (!is.character(severity) || length(severity) != 1L || is.na(severity)) {
    stop("`severity` must be a single non-missing character string.", call. = FALSE)
  }

  if (!severity %in% c("subtle", "moderate", "severe")) {
    stop(
      "`severity` must be one of 'subtle', 'moderate', or 'severe'.",
      call. = FALSE
    )
  }

  n = as.integer(n)[1]

  if (is.na(n) || n < 1L) {
    stop("`n` must be an integer greater than or equal to 1.", call. = FALSE)
  }

  if (!is.logical(mixTypes) || length(mixTypes) != 1L || is.na(mixTypes)) {
    stop("`mixTypes` must be TRUE or FALSE.", call. = FALSE)
  }

  if (!is.logical(labelErrors) || length(labelErrors) != 1L || is.na(labelErrors)) {
    stop("`labelErrors` must be TRUE or FALSE.", call. = FALSE)
  }

  if (!identical(type, "auto")) {
    unsupportedTypes = setdiff(type, listBadExplanationTypes())

    if (length(unsupportedTypes) > 0L) {
      stop(
        paste0(
          "Unsupported bad explanation type(s): ",
          paste(unsupportedTypes, collapse = ", ")
        ),
        call. = FALSE
      )
    }
  }

  invisible(TRUE)
}

#' Resolve the source explanation for bad explanation generation
#'
#' @param x A `wmfmModel` object.
#' @param explanation Optional user-supplied explanation.
#'
#' @return A character scalar giving the base explanation.
#' @keywords internal
resolveBadExplanationSource = function(x, explanation = NULL) {

  if (!is.null(explanation)) {
    if (!nzchar(trimws(explanation))) {
      stop("`explanation` must not be empty.", call. = FALSE)
    }

    return(explanation)
  }

  objectExplanation = x$explanation

  if (!is.character(objectExplanation) ||
      length(objectExplanation) != 1L ||
      is.na(objectExplanation) ||
      !nzchar(trimws(objectExplanation))) {
    stop(
      paste(
        "`generateBadExplanation()` requires either a supplied `explanation`",
        "or a non-empty `x$explanation`."
      ),
      call. = FALSE
    )
  }

  objectExplanation
}

#' Build compact model context for bad explanation generation
#'
#' @param x A `wmfmModel` object.
#'
#' @return A character scalar.
#' @keywords internal
buildBadExplanationModelContext = function(x) {

  formulaText = paste(deparse(x$formula), collapse = "")
  responseName = tryCatch(
    all.vars(stats::formula(x$model))[1],
    error = function(e) {
      NA_character_
    }
  )

  factorPredictors = tryCatch(
    getFactorPredictors(x$model, x$data),
    error = function(e) {
      character(0)
    }
  )

  interactionTerms = x$interactionTerms %||% character(0)
  interactionTerms = interactionTerms[!is.na(interactionTerms) & nzchar(interactionTerms)]

  parts = c(
    paste0("Model type: ", safeWmfmScalar(x$modelType)),
    paste0("Formula: ", formulaText),
    paste0("Response: ", safeWmfmScalar(responseName)),
    if (length(factorPredictors) > 0L) {
      paste0("Factor predictors: ", paste(factorPredictors, collapse = ", "))
    } else {
      "Factor predictors: none"
    },
    if (length(interactionTerms) > 0L) {
      paste0("Interaction terms: ", paste(interactionTerms, collapse = ", "))
    } else {
      "Interaction terms: none"
    },
    buildBadExplanationReferenceLevels(x),
    if (!is.null(x$dataContext) && !is.na(x$dataContext) && nzchar(trimws(x$dataContext))) {
      paste0("Data context: ", x$dataContext)
    } else {
      NULL
    }
  )

  paste(parts, collapse = "\n")
}

#' Build reference-level context for factor predictors
#'
#' @param x A `wmfmModel` object.
#'
#' @return A character scalar describing factor predictors and their reference
#'   levels.
#' @keywords internal
buildBadExplanationReferenceLevels = function(x) {

  factorPredictors = tryCatch(
    getFactorPredictors(x$model, x$data),
    error = function(e) {
      character(0)
    }
  )

  if (length(factorPredictors) < 1L) {
    return("Factor reference levels: none")
  }

  details = vapply(factorPredictors, function(varName) {
    varData = x$data[[varName]]
    levelsVec = levels(varData)
    referenceLevel = if (length(levelsVec) > 0L) levelsVec[[1]] else "unknown"

    paste0(
      varName,
      ": reference level = ",
      referenceLevel,
      "; levels = ",
      paste(levelsVec, collapse = ", ")
    )
  }, character(1))

  paste(c("Factor reference levels:", details), collapse = "\n")
}

#' Build a bad explanation generation plan
#'
#' @param x A `wmfmModel` object.
#' @param type Character vector of bad explanation types, or `"auto"`.
#' @param severity Character scalar.
#' @param n Integer number of explanations to generate.
#' @param mixTypes Logical.
#'
#' @return A named list describing the requested generation plan.
#' @keywords internal
buildBadExplanationPlan = function(
    x,
    type = "auto",
    severity = "moderate",
    n = 1,
    mixTypes = FALSE
) {

  availableTypes = getAvailableBadExplanationTypes(x)

  if (identical(type, "auto")) {
    requestedTypes = availableTypes
  } else {
    unavailableTypes = setdiff(type, availableTypes)

    if (length(unavailableTypes) > 0L) {
      stop(
        paste0(
          "The following bad explanation type(s) are not compatible with this model: ",
          paste(unavailableTypes, collapse = ", ")
        ),
        call. = FALSE
      )
    }

    requestedTypes = type
  }

  if (length(requestedTypes) < 1L) {
    stop(
      "No compatible bad explanation types are available for this model.",
      call. = FALSE
    )
  }

  width = nchar(as.character(n))
  explanationNames = paste0(
    "explanation_",
    formatC(seq_len(n), width = width, flag = "0")
  )

  selectedTypes = lapply(seq_len(n), function(i) {
    if (isTRUE(mixTypes)) {
      nTypesThisExplanation = switch(
        severity,
        subtle = 1L,
        moderate = min(2L, length(requestedTypes)),
        severe = min(3L, length(requestedTypes))
      )

      sample(
        requestedTypes,
        size = nTypesThisExplanation,
        replace = FALSE
      )
    } else {
      sample(requestedTypes, size = 1L, replace = TRUE)
    }
  })
  names(selectedTypes) = explanationNames

  list(
    explanationNames = explanationNames,
    selectedTypes = selectedTypes,
    severity = stats::setNames(rep(severity, n), explanationNames),
    availableTypes = availableTypes,
    requestedTypes = requestedTypes,
    mixTypes = mixTypes
  )
}
