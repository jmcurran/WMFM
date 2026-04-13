#' Build an L prompt for fitted-model equations
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

  if (inherits(model, "glm")) {
    fam = model$family$family
    link = model$family$link
    modelDesc = glue::glue(
      "This is a generalised linear model with {fam} family and {link} link."
    )
  } else {
    fam = NULL
    link = NULL
    modelDesc = "This is a linear regression model with Gaussian errors and identity link."
  }

  binomialLabelNote = ""
  if (inherits(model, "glm") && identical(fam, "binomial") && identical(link, "logit")) {
    outcomeLabels = getBinomialOutcomeLabels(model)
    successLabel = outcomeLabels$successLabel
    failureLabel = outcomeLabels$failureLabel
    binomialLabelNote = glue::glue(
      "For this binomial model, the fitted success outcome is '{successLabel}' and the other outcome is '{failureLabel}'. Use Pr({successLabel}) and Odds({successLabel}) instead of a generic p. Also show Pr({failureLabel}) and Odds({failureLabel}) for the complementary outcome."
    )
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

  tm = stats::terms(model)
  termLabels = attr(tm, "term.labels")
  hasInteractions = any(grepl(":", termLabels, fixed = TRUE))

  if (hasInteractions) {
    extraRules = "
The model includes interaction terms (coefficient names contain ':').

For interactions between a factor F and a numeric variable X:

- Let the coefficients be, symbolically:
    (Intercept) = a,   F[level] = b,   X = c,   F[level]:X = d.
  This symbolic notation is ONLY for your internal reasoning.
  In the equations you output, you must NEVER show the letters a, b, c, or d.

- For the reference level of F (where all F[level] = 0), write an equation such as
    logit(Pr(Pass)) = a + c * X    (when F = reference level)
  but in the actual equation you produce, replace a and c with their numeric values,
  for example
    logit(Pr(Pass)) = -4.71 + 0.42 * Test    (when F = \"No\")

- For a non-reference level L of F, you MUST show BOTH the intercept and the slope
  as numeric combinations BEFORE simplifying, and then show the simplified numeric version.

- Use ordinary algebraic signs:
    * write (11.69 - 1.56), NOT (11.69 + (-1.56))
    * write (-1.56 - 1.59), NOT (-1.56 + (-1.59))
    * write -3.15 * X, NOT + (-3.15) * X

- For example, if the coefficients imply
    (Intercept) = -4.71,  F[L] = -5.13,  X = 0.42,  F[L]:X = 0.76,
  then you should write
    logit(Pr(Pass)) = (-4.71 - 5.13) + (0.42 + 0.76) * Test = -9.84 + 1.18 * Test    (when F = \"Yes\")

IMPORTANT:
- Do NOT include symbolic patterns like (a + b) or (c + d) in the output.
- In the equations you return, always use the numeric values only.
- NEVER write expressions like + (-1.56) or + (-1.59) * X.
- Always simplify signs so that negative quantities are written with '-' instead.

This rule applies to ALL model types:
- If the model is binomial with logit link, write equations on the logit scale and then also on the original probability scale.
- If the model is Poisson with log link, write equations on the log(mu) scale and then also on the original count scale.
"
  } else {
    extraRules = "
The model has NO interaction terms (only main effects).

VERY IMPORTANT: For every factor predictor, you MUST output one equation
for the reference level and one equation for EACH non-reference level.
Example (linear model): suppose the model is Exam ~ Attend + Test with coefficients
  (Intercept) = a,  AttendYes = b,  Test = c.
This notation (a, b, c) is ONLY for your internal reasoning; do NOT use these letters
in the final equations you output.

In your actual equations you should plug in numeric values. For example, if
  (Intercept) = 12.10,  AttendYes = 2.53,  Test = 3.52,
then you should output BOTH of the following equations:

- For the reference level (Attend = \"No\"):
    Exam = 12.10 + 3.52 * Test    (when Attend = \"No\")

- For the non-reference level (Attend = \"Yes\"):
    Exam = (12.10 + 2.53) + 3.52 * Test = 14.63 + 3.52 * Test    (when Attend = \"Yes\")

Exactly the SAME pattern must be followed for generalised linear models:
- For a binomial logit model, you might write, for a non-reference level L,
    logit(Pr(Pass)) = (-7.70 + 2.14) + 0.70 * Test = -5.56 + 0.70 * Test    (when Attend = \"Yes\")
    Odds(Pass) = exp(-5.56 + 0.70 * Test)    (when Attend = \"Yes\")
    Odds(Fail) = exp(-(-5.56 + 0.70 * Test))    (when Attend = \"Yes\")
    Pr(Pass) = exp(-5.56 + 0.70 * Test) / (1 + exp(-5.56 + 0.70 * Test))    (when Attend = \"Yes\")
    Pr(Fail) = 1 / (1 + exp(-5.56 + 0.70 * Test))    (when Attend = \"Yes\")
- For a Poisson log-link model, you might write
    log(mu) = (1.86 + 0.45) + 0.30 * X = 2.31 + 0.30 * X    (when F = \"High\")
    mu = exp(2.31 + 0.30 * X)    (when F = \"High\")

Apply this idea to every factor in the actual model:
- First write the baseline equation for the reference level.
- Then, for each non-reference level L of the factor F, write an equation where
  the intercept is written as a numeric combination (baseline intercept with the
  coefficient for F = L) BEFORE simplifying, and then show the simplified numeric intercept.
- If the factor coefficient is negative, write subtraction, not + (negative number).
  For example, write (11.69 - 1.56), NOT (11.69 + (-1.56)).
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
{binomialLabelNote}

Your task is to write fitted-model equations for teaching.
These must be presented ONLY in fully expanded, condition-specific form,
NOT in abstract coefficient or dummy-variable form.

Response: {response}

Predictors:
{paste(predInfo, collapse = '\\n')}

Coefficient table (rounded for display):
{coefText}

General rules:
- Round numeric quantities you write in equations to 2 decimal places.
- Use the ASCII '*' character for multiplication (e.g. b1 * X1). Do NOT use any
  Unicode multiplication symbols such as '\\u00D7' or '\\u00B7'.
- Use ordinary algebraic sign formatting throughout:
  * write 11.69 - 1.56 * X, NOT 11.69 + (-1.56) * X
  * write (11.69 - 1.56), NOT (11.69 + (-1.56))
  * write -3.15 * X, NOT + (-3.15) * X
- Never show '+ (-number)' anywhere in the output.
- Do NOT write the model in coefficient or dummy-variable form such as:
    Y = b0 + b1 * X1 + b2 * FactorLevel
- Do NOT include any equation containing indicator variables such as AttendYes,
  GroupB, or similar dummy-variable names.
- ONLY write fully expanded fitted equations for specific predictor conditions.
- For linear regression (Gaussian, identity link):
  * Write equations like: {response} = b0 + b1 * X1 + ...
- For binomial GLMs with logit link:
  * Write equations on the log-odds (logit) scale using the actual outcome label, e.g. logit(Pr(Success)) = ...
  * Then ALWAYS also give BOTH odds equations and BOTH probability equations using the actual outcome labels:
    Odds(Success) = exp(eta)
    Odds(Failure) = exp(-eta)
    Pr(Success) = exp(eta) / (1 + exp(eta))
    Pr(Failure) = 1 / (1 + exp(eta)).
- For Poisson GLMs with log link:
  * Write equations on the log scale, e.g. log(mu) = ...
  * Then ALWAYS also give the transformed equation on the original count scale:
    mu = exp(eta).
- For factor predictors in ANY model (linear or GLM):
  * Always give separate equations for the reference level and each non-reference level.
  * When you adjust the intercept (and slopes, in interaction models), first show the
    numeric combination and then show the simplified numeric value.
  * Use '+' when the added quantity is positive and '-' when the added quantity is negative.
- For any non-identity link, do NOT stop at the linear predictor.
  After each equation on the link scale, also write the corresponding equation
  transformed back to the original response scale.
- For a Poisson model, each log(mu) equation must be followed by
  mu = exp(...) for the same condition.
- For a binomial logit model, each logit(Pr(Success)) equation must be followed by
  Odds(Success) = exp(...) for the same condition,
  Odds(Failure) = exp(-(...)) for the same condition,
  Pr(Success) = exp(...) / (1 + exp(...)) for the same condition, and
  Pr(Failure) = 1 / (1 + exp(...)) for the same condition.
- When writing the transformed equation on the original scale, use the simplified form
  of the linear predictor inside exp(...), not the unsimplified combination.

{extraRules}

Formatting:
- Write the equations in plain text, one equation per line (or with simple wrapped lines).
- Label each equation with the relevant condition in brackets, like
  (when Attend = \"Yes\") or (when Colour = \"Blue\").
- Do NOT include any general, combined, or coefficient-form equation.
- The output must consist ONLY of condition-specific equations.
- Do not mention standard errors, t-values, z-values, or p-values.
- Return ONLY the equations, with no extra commentary.
")
}
