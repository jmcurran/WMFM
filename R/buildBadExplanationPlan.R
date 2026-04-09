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
