#' Schema for LM equations returned by the LLM
#'
#' Defines a structured schema for equations describing a fitted regression
#' model. This is only used when talking to providers that support
#' structured outputs (e.g. OpenAI via ellmer).
#'
#' @format An object created by \code{ellmer::type_object()} describing a
#'   list of equations, each with a \code{condition} and \code{equation}
#'   field.
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
