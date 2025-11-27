#' Build a prompt asking for fitted-model equations
#'
#' Given a fitted linear or generalised linear model, construct a detailed
#' prompt that asks a language model to write down the fitted-model
#' equations. The prompt includes a description of the model, the response
#' and predictors, and the coefficient table.
#'
#' The rules in the prompt are tailored for teaching: for additive models
#' with factors, equations are requested separately for each factor level,
#' and for interaction models, the arithmetic behind changes in intercept
#' and slope is emphasised.
#'
#' @param model A fitted model object, typically of class \code{"lm"} or
#'   \code{"glm"}.
#'
#' @return A character scalar containing the prompt text to send to the
#'   language model.
#' @keywords internal
lmToPrompt = function(model) {
  modelFrame = model.frame(model)
  response = names(modelFrame)[1]
  coefTable = coef(summary(model))
  coefText = paste(capture.output(print(round(coefTable, 4))), collapse = "\n")

  # Model type description
  if (inherits(model, "glm")) {
    fam = model$family$family
    link = model$family$link
    modelDesc = glue(
      "This is a generalised linear model with {fam} family and {link} link."
    )
  } else {
    modelDesc = "This is a linear regression model with Gaussian errors and identity link."
  }

  predictors = setdiff(names(modelFrame), response)
  predictorInfo = sapply(
    predictors,
    function(varName) {
      x = modelFrame[[varName]]
      if (is.factor(x)) {
        levelsX = levels(x)
        sprintf(
          "- %s: factor; levels = [%s]; reference = %s",
          varName,
          paste(levelsX, collapse = ", "),
          levelsX[1]
        )
      } else {
        sprintf("- %s: numeric", varName)
      }
    },
    USE.NAMES = FALSE
  )

  # Detect interactions from the terms object
  termsObj = terms(model)
  termLabels = attr(termsObj, "term.labels")
  hasInteractions = any(grepl(":", termLabels, fixed = TRUE))

  if (hasInteractions) {
    # Interaction models
    interactionRules = "
- The model includes interaction terms (see coefficient names containing ':').
- Use the coefficient table to express how BOTH the intercept and slopes change across
  levels of the factor involved in the interaction.

- When there is a single factor-by-numeric interaction such as Attend * Test:
  * Let the coefficients be (symbolically): (Intercept) = a, AttendYes = b,
    Test = c, AttendYes:Test = d.
  * First write the equation for the reference level of the factor, e.g.
      Exam = a + c × Test    (when Attend = \"No\")
  * Then, for the non-reference level (e.g. Attend = \"Yes\"), show BOTH the intercept
    and slope as sums BEFORE simplifying, for example:
      Exam = (a + b) + (c + d) × Test = 14.63 + 4.75 × Test    (when Attend = \"Yes\")
    where you plug in the actual numeric values for a, b, c, d from the coefficient table.

- In general, for each non-reference level L of a factor F that interacts with Test:
  * The intercept for level L is (Intercept + coefficient of F=L).
  * The slope for Test at level L is (coefficient of Test + coefficient of F=L:Test).
  * Show these arithmetic sums explicitly in brackets, THEN simplify them in the same line.

- If there are more complicated interactions (e.g. multiple factors or higher-order
  interactions), you may give either:
  * a set of equations by factor level using the same intercept/slope-decomposition idea, or
  * a single general equation with indicator functions, if that is clearer.
"
  } else {
    # Additive models (no interactions)
    interactionRules = "
- There are no interaction terms.
- For each factor predictor:
  * Use the reference level implied by the intercept.
  * Give one equation per factor level.

- For binary factors such as Attend with levels \"No\" (reference) and \"Yes\":
  * First show the baseline equation for the reference level, for example
      Exam = 6.62 + 3.52 × Test    (when Attend = \"No\")
  * Then, for the non-reference level, explicitly show how the intercept is obtained
    by adding the factor coefficient to the intercept, for example
      Exam = (6.62 + 8.01) + 3.52 × Test = 14.63 + 3.52 × Test    (when Attend = \"Yes\")
    (numbers here are just an illustration; use the actual coefficients from the table).

- Use this same pattern for any factor: show the baseline equation, then for each
  non-reference level show (intercept + factor coefficient) before simplifying.
"
  }

  glue("
You are given output from an R regression model.
{modelDesc}

Your task is to write fitted-model equations for teaching.

Response: {response}

Predictors:
{paste(predictorInfo, collapse = '\\n')}

Coefficient table (rounded for display):
{coefText}

General rules:
- Round numeric quantities you write in equations to 2 decimal places.
- For linear regression (Gaussian, identity link):
  * Write equations like: {response} = b0 + b1 × X1 + ...
- For binomial GLMs with logit link:
  * Write equations on the log-odds (logit) scale, e.g. logit(p) = ...
  * You may optionally also give p = exp(eta) / (1 + exp(eta)).
- For Poisson GLMs with log link:
  * Write equations on the log scale, e.g. log(mu) = ...
  * You may optionally also give mu = exp(eta).

{interactionRules}

Formatting:
- Write the equations in plain text, one per line.
- Label each equation with the relevant condition in brackets, like (when Attend = \"Yes\").
- Do not mention standard errors, t-values, z-values, or p-values.
- Return only the equations, no extra commentary.
")
}
