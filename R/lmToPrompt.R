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
- Write an equation for the reference level of F (where all F[level] = 0), e.g.
    Response = a + c * X    (when F = reference level)
- For a non-reference level L of F, you MUST show BOTH:
    * the intercept as a sum (a + b) BEFORE simplification, and
    * the slope as a sum (c + d) BEFORE simplification,
  and then show the simplified numeric version on the same line or the next line, e.g.
    Response = (a + b) + (c + d) * X = 14.63 + 4.75 * X   (when F = L)

This rule applies to ALL model types:
- If the model is binomial with logit link, simply replace 'Response' by logit(p)
  or logit(Pass), but still show the sums (a + b), (c + d) before simplifying.
- If the model is Poisson with log link, use log(mu) similarly.

General rules for interactions:
- For each relevant combination of factor levels, give a separate equation.
- Each equation must be labelled with its condition in brackets, e.g.
    (when Attend = \"Yes\" and Gender = \"Female\").
"
  } else {
    # ---------------------- Additive models -------------------------
    extraRules = "
The model has NO interaction terms (only main effects).

VERY IMPORTANT: For every factor predictor, you MUST output one equation
for the reference level and one equation for EACH non-reference level.

Example (linear model): suppose the model is Exam ~ Attend + Test with coefficients
  (Intercept) = a,  AttendYes = b,  Test = c.
Then you MUST output BOTH of the following equations:

- For the reference level (Attend = \"No\"):
    Exam = a + c * Test    (when Attend = \"No\")

- For the non-reference level (Attend = \"Yes\"):
    Exam = (a + b) + c * Test = 14.63 + 3.52 * Test    (when Attend = \"Yes\")

Exactly the SAME pattern must be followed for generalised linear models:
- For a binomial logit model, you might write, for a non-reference level L,
    logit(Pass) = (a + b) + c * Test = -5.56 + 0.70 * Test    (when Attend = L)
- For a Poisson log-link model, you might write
    log(mu) = (a + b) + c * X = 2.31 + 0.45 * X    (when F = L)

Apply this idea to every factor in the actual model:
- First write the baseline equation for the reference level.
- Then, for each non-reference level L of the factor F, write an equation where
  the intercept is written as a sum (baseline intercept + coefficient for F = L)
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
    symbolic sum (e.g. (a + b) or (c + d)), and then show the simplified numeric value.

{extraRules}

Formatting:
- Write the equations in plain text, one equation per line (or with simple wrapped lines).
- Label each equation with the relevant condition in brackets, like
  (when Attend = \"Yes\") or (when Colour = \"Blue\").
- Do not mention standard errors, t-values, z-values, or p-values.
- Return ONLY the equations, with no extra commentary.
")
}
