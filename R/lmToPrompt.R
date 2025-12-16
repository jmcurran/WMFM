#' Build an LLM prompt for fitted-model equations
#'
#' @param model A fitted \code{lm} or \code{glm} object.
#'
#' @return A single character string to be sent to the chat model.
#' @keywords internal
lmToPrompt = function(model) {
  mf = stats::model.frame(model)
  response = names(mf)[1]
  coefTable = coef(summary(model))
  coefText = paste(
    utils::capture.output(print(round(coefTable, 4))),
    collapse = "\n"
  )

  # Model type description
  if (inherits(model, "glm")) {
    fam = model$family$family
    link = model$family$link
    modelDesc = glue::glue(
      "This is a generalised linear model with {fam} family and {link} link."
    )
  } else {
    modelDesc = "This is a linear regression model with Gaussian errors and identity link."
  }

  predictors = setdiff(names(mf), response)
  predInfo = vapply(
    predictors,
    function(v) {
      x = mf[[v]]
      if (is.factor(x)) {
        lvls = levels(x)
        sprintf(
          "- %s: factor; levels = [%s]; reference = %s",
          v,
          paste(lvls, collapse = ", "),
          lvls[1]
        )
      } else {
        sprintf("- %s: numeric", v)
      }
    },
    character(1L)
  )

  # --- Detect interactions from the terms object ---
  tm = terms(model)
  termLabels = attr(tm, "term.labels")
  hasInteractions = any(grepl(":", termLabels, fixed = TRUE))

  if (hasInteractions) {
    # ---------------------- Interaction models ----------------------
    extraRules = "
The model includes interaction terms (coefficient names contain ':').

For interactions between a factor F and a numeric variable X:

- Let the coefficients be, symbolically:
    (Intercept) = a,   F[level] = b,   X = c,   F[level]:X = d.
  This symbolic notation is ONLY for your internal reasoning.
  In the equations you output, you must NEVER show the letters a, b, c, or d.

- For the reference level of F (where all F[level] = 0), write an equation such as
    logit(Pass) = a + c * X    (when F = reference level)
  but in the actual equation you produce, replace a and c with their numeric values,
  for example
    logit(Pass) = -4.71 + 0.42 * Test    (when F = \"No\")

- For a non-reference level L of F, you MUST show BOTH the intercept and the slope
  as NUMERIC sums BEFORE simplifying, and then show the simplified numeric version.
  For example, if the coefficients imply
    (Intercept) = -4.71,  F[L] = -5.13,  X = 0.42,  F[L]:X = 0.76,
  then you should write
    logit(Pass) = (-4.71 - 5.13) + (0.42 + 0.76) * Test = -9.84 + 1.18 * Test    (when F = \"Yes\")

IMPORTANT:
- Do NOT include symbolic patterns like (a + b) or (c + d) in the output.
- In the equations you return, always use the NUMERIC values only, and show sums like
  (-4.71 - 5.13) rather than (a + b).

This rule applies to ALL model types:
- If the model is binomial with logit link, write equations on the logit scale (e.g. logit(p)).
- If the model is Poisson with log link, write equations on the log(mu) scale.
"
  } else {
    # ---------------------- Additive models -------------------------
    extraRules = "
The model has NO interaction terms (only main effects).

VERY IMPORTANT: For every factor predictor, you MUST output one equation
for the reference level and one equation for EACH non-reference level.

Example (linear model): suppose the model is Exam ~ Attend + Test with coefficients
  (Intercept) = a,  AttendYes = b,  Test = c.
This notation (a, b, c) is ONLY for your internal reasoning; do NOT use these letters
in the final equations you output.

In your actual equations you should plug in NUMERIC values. For example, if
  (Intercept) = 12.10,  AttendYes = 2.53,  Test = 3.52,
then you should output BOTH of the following equations:

- For the reference level (Attend = \"No\"):
    Exam = 12.10 + 3.52 * Test    (when Attend = \"No\")

- For the non-reference level (Attend = \"Yes\"):
    Exam = (12.10 + 2.53) + 3.52 * Test = 14.63 + 3.52 * Test    (when Attend = \"Yes\")

Exactly the SAME pattern must be followed for generalised linear models:
- For a binomial logit model, you might write, for a non-reference level L,
    logit(Pass) = (-7.70 + 2.14) + 0.70 * Test = -5.56 + 0.70 * Test    (when Attend = \"Yes\")
- For a Poisson log-link model, you might write
    log(mu) = (1.86 + 0.45) + 0.30 * X = 2.31 + 0.30 * X    (when F = \"High\")

Apply this idea to every factor in the actual model:
- First write the baseline equation for the reference level.
- Then, for each non-reference level L of the factor F, write an equation where
  the intercept is written as a NUMERIC sum (baseline intercept + coefficient for F = L)
  BEFORE simplifying, and then show the numeric simplified intercept.
- If there are several factors, you may either:
  * list equations for each factor separately (holding others at reference), or
  * list equations for important combinations of factor levels.

Do NOT return only a single equation when a factor is present.
There must be at least as many equations as there are levels you describe.
"
  }

  glue::glue("
You are given output from an R regression model.
{modelDesc}

Your task is to write fitted-model equations for teaching.

Response: {response}

Predictors:
{paste(predInfo, collapse = '\\n')}

Coefficient table (rounded for display):
{coefText}

General rules:
- Round numeric quantities you write in equations to 2 decimal places.
- Use the ASCII '*' character for multiplication (e.g. b1 * X1). Do NOT use any
  Unicode multiplication symbols such as '\\u00D7' or '\\u00B7'.
- For linear regression (Gaussian, identity link):
  * Write equations like: {response} = b0 + b1 * X1 + ...
- For binomial GLMs with logit link:
  * Write equations on the log-odds (logit) scale, e.g. logit(p) = ...
  * You may optionally also give p = exp(eta) / (1 + exp(eta)).
- For Poisson GLMs with log link:
  * Write equations on the log scale, e.g. log(mu) = ...
  * You may optionally also give mu = exp(eta).
- For factor predictors in ANY model (linear or GLM):
  * Always give separate equations for the reference level and each non-reference level.
  * When you adjust the intercept (and slopes, in interaction models), first show the
    NUMERIC sum (e.g. (-7.70 + 2.14)) and then show the simplified numeric value.

{extraRules}

Formatting:
- Write the equations in plain text, one equation per line (or with simple wrapped lines).
- Label each equation with the relevant condition in brackets, like
  (when Attend = \"Yes\") or (when Colour = \"Blue\").
- Do not mention standard errors, t-values, z-values, or p-values.
- Return ONLY the equations, with no extra commentary.
")
}
