#----------------------------------------
# LLM provider
#----------------------------------------

get_chat_provider = function() {
  # If you later want OpenAI, you can uncomment this:
  # if (nzchar(Sys.getenv("OPENAI_API_KEY"))) {
  #   return(chat_openai())
  # }

  # For now: always use Ollama
  chat_ollama(base_url = "http://corrin.stat.auckland.ac.nz:11434",
              model = "gpt-oss")
}

#----------------------------------------
# Structured schema (only for OpenAI)
#----------------------------------------

type_lm_equations = type_object(
  "Equations describing a fitted regression model.",
  equations = type_array(
    type_object(
      "One equation under a particular condition.",
      condition = type_string("When the equation applies."),
      equation  = type_string("The algebraic equation.")
    )
  )
)

#----------------------------------------
# Prompt builder for equations
#----------------------------------------

lm_to_prompt = function(model) {
  mf = model.frame(model)
  response = names(mf)[1]
  coef_tbl = coef(summary(model))
  coef_txt = paste(capture.output(print(round(coef_tbl, 4))), collapse = "\n")

  # Model type description
  if (inherits(model, "glm")) {
    fam = model$family$family
    link = model$family$link
    model_desc = glue("This is a generalised linear model with {fam} family and {link} link.")
  } else {
    model_desc = "This is a linear regression model with Gaussian errors and identity link."
  }

  predictors = setdiff(names(mf), response)
  pred_info = sapply(
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
    USE.NAMES = FALSE
  )

  # --- Detect interactions from the terms object ---
  tm = terms(model)
  term_labels = attr(tm, "term.labels")
  has_interactions = any(grepl(":", term_labels, fixed = TRUE))

  if (has_interactions) {
    # ----- Interaction models -----
    interaction_rules = "
- The model includes interaction terms (see coefficient names containing ':').
- Use the coefficient table to express how BOTH the intercept and slopes change across
  levels of the factor involved in the interaction.

- When there is a single factor-by-numeric interaction such as Attend * Test:
  * Let the coefficients be (symbolically): (Intercept) = a, AttendYes = b,
    Test = c, AttendYes:Test = d.
  * First write the equation for the reference level of the factor, e.g.
      Exam = a + c * Test    (when Attend = \"No\")
  * Then, for the non-reference level (e.g. Attend = \"Yes\"), show BOTH the intercept
    and slope as sums BEFORE simplifying, for example:
      Exam = (a + b) + (c + d) * Test = 14.63 + 4.75 * Test    (when Attend = \"Yes\")
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
    # ----- Additive models (no interactions) -----
    interaction_rules = "
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
{model_desc}

Your task is to write fitted-model equations for teaching.

Response: {response}

Predictors:
{paste(pred_info, collapse = '\\n')}

Coefficient table (rounded for display):
{coef_txt}

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

{interaction_rules}

Formatting:
- Write the equations in plain text, one per line.
- Label each equation with the relevant condition in brackets, like (when Attend = \"Yes\").
- Do not mention standard errors, t-values, z-values, or p-values.
- Return only the equations, no extra commentary.
")
}


#----------------------------------------
# Prompt builder for explanation
#----------------------------------------

lm_to_explanation_prompt = function(model) {
  mf = model.frame(model)
  response = names(mf)[1]
  coef_tbl = coef(summary(model))
  coef_txt = paste(capture.output(print(round(coef_tbl, 4))), collapse = "\n")

  n = nrow(mf)
  if (inherits(model, "glm")) {
    fam = model$family$family
    link = model$family$link
    s = summary(model)
    if (!is.null(s$null.deviance) && s$null.deviance > 0) {
      pseudo_r2 = 1 - s$deviance / s$null.deviance
      r2_txt = glue("Pseudo R-squared (1 - dev/null): {round(pseudo_r2, 3)}")
    } else {
      r2_txt = "Pseudo R-squared not available."
    }
    model_desc = glue("This is a generalised linear model with {fam} family and {link} link.")
  } else {
    s = summary(model)
    r2 = round(s$r.squared, 3)
    r2_txt = glue("R-squared: {r2}")
    model_desc = "This is a linear regression model with Gaussian errors and identity link."
    fam = "gaussian"
    link = "identity"
  }

  outcome_desc = if (inherits(model, "glm") && fam == "binomial") {
    "The outcome is binary (0/1), so the model describes how the probability of '1' changes."
  } else if (inherits(model, "glm") && fam == "poisson") {
    "The outcome is a non-negative count, so the model describes how the expected count changes."
  } else {
    "The outcome is continuous, so the model describes how the mean response changes."
  }

  glue("
You are a friendly statistics tutor.
Explain the results of this regression model in clear, non-technical language.

{model_desc}
{outcome_desc}

Response variable: {response}
Number of observations: {n}
{r2_txt}

Coefficient table:
{coef_txt}

Guidelines:
- Briefly explain what the response represents and what the predictors represent.
- Describe the direction of each important effect (positive/negative).
- Use the rounded coefficients to give an intuitive sense of size
  (e.g. 'for each 1-unit increase in Test, the exam score increases by about 3.8 points').
- For binomial/logit models, talk about higher or lower probability of the event.
- For Poisson/log models, talk about higher or lower expected counts or rates.
- For factor predictors, explain differences between groups.
- If there are interactions, briefly describe how one effect depends on another variable.
- Do not mention standard errors, t-values, z-values, or p-values explicitly.
- Keep it to 1–3 short paragraphs.

Now provide the explanation.
")
}

#----------------------------------------
# Master function for equations (with caching)
#----------------------------------------

lm_equations = function(model, chat) {
  key = paste("eq", deparse(formula(model)), paste(coef(model), collapse = ";"))

  if (!is.null(.env_cache[[key]])) {
    return(.env_cache[[key]])
  }

  prompt = lm_to_prompt(model)

  if (inherits(chat, "ProviderOpenAI")) {
    res = chat$chat_structured(prompt, type = type_lm_equations)
    out = as_tibble(res$equations)
  } else {
    out = chat$chat(prompt)
  }

  .env_cache[[key]] = out
  out
}

#----------------------------------------
# Master function for explanation (with caching)
#----------------------------------------

lm_explanation = function(model, chat) {
  key = paste("exp", deparse(formula(model)), paste(coef(model), collapse = ";"))

  if (!is.null(.env_cache[[key]])) {
    return(.env_cache[[key]])
  }

  prompt = lm_to_explanation_prompt(model)
  out = chat$chat(prompt)
  .env_cache[[key]] = out
  out
}
