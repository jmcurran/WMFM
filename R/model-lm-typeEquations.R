#' Schema for LM equations returned by the LLM
#'
#' Defines the structured output schema for fitted regression model equations.
#' This is only used with providers that support structured outputs.
#'
#' @format An `ellmer::type_object()` schema with `condition` and `equation`
#'   fields for each equation.
#' @keywords internal
typeLmEquations = ellmer::type_object(
  "Equations describing a fitted regression model.",
  equations = ellmer::type_array(
    ellmer::type_object(
      "One equation under a particular condition.",
      condition = ellmer::type_string("When the equation applies."),
      equation = ellmer::type_string("The algebraic equation.")
    )
  )
)
