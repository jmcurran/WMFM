.env_cache = new.env(parent = emptyenv())

library(shiny)
library(tools)
library(sortable)
library(ellmer)
library(glue)
library(tibble)
library(ggplot2)

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

#----------------------------------------
# UI
#----------------------------------------

ui = fluidPage(
  
  withMathJax(), 
  
  titlePanel("Model Builder"),
  
  # CSS to shrink the rank lists to ~4–5 lines
  tags$style(HTML("
    .bucket-list .rank-list {
      max-height: 8em;
      overflow-y: auto;
    }
  ")),
  
  # ---- File selector row ----
  fluidRow(
    column(
      12,
      fileInput(
        "file",
        "Choose CSV, TXT, or RDA",
        accept = c(".csv", ".txt", ".rda", ".RData")
      )
    )
  ),
  
  hr(),
  
  # ---- Drag-and-drop variable buckets ----
  h4("Assign variables"),
  uiOutput("var_buckets"),
  
  hr(),
  
  # ---- Tabs for model vs plot ----
  tabsetPanel(
    id = "main_tabs",
    
    # ---- Tab 1: Model ----
    tabPanel(
      "Model",
      
      h4("Model type"),
      radioButtons(
        "model_type",
        label = NULL,
        choices = c(
          "Linear regression" = "lm",
          "Logistic regression (binomial, logit)" = "logistic",
          "Poisson regression (log link)" = "poisson"
        ),
        selected = "lm"
      ),
      
      h4("Response"),
      uiOutput("response_picker"),
      
      h4("Model formula"),
      textInput("formula_text", label = NULL, value = "", width = "100%"),
      verbatimTextOutput("formula_status"),
      
      br(),
      actionButton("fit_btn", "Fit model"),
      actionButton("reset_btn", "Reset model"),
      
      hr(),
      
      # ---- Equations + summary ----
      fluidRow(
        column(
          6,
          h4("Fitted equations"),
          uiOutput("model_equations")
        ),
        column(
          6,
          h4("Model summary"),
          verbatimTextOutput("model_output")
        )
      ),
      
      hr(),
      hr(),
      
      h4("Model formula"),
      uiOutput("model_formula"),
      
      h4("Model explanation"),
      uiOutput("model_explanation")
    ),
    
    # ---- Tab 2: Plot ----
    tabPanel(
      "Plot",
      h4("Data and fitted model"),
      plotOutput("model_plot"),
      helpText(
        "The plot shows the observed data and the fitted model ",
        "against one numeric predictor (x-axis), optionally separated by a factor."
      )
    )
  )
)


#----------------------------------------
# Server
#----------------------------------------

server = function(input, output, session) {
  
  `%||%` = function(x, y) {
    if (is.null(x)) {
      y
    } else {
      x
    }
  }
  
  chat_provider = get_chat_provider()
  
  rv = reactiveValues(
    data = NULL,
    all_vars = character(0),
    auto_formula = "",
    model_equations = NULL,
    model_explanation = NULL,
    bucket_group_id = 0 
    # last_response = NULL  # optional now
  )
  
  
  model_fit = reactiveVal(NULL)
  
  output$model_plot = renderPlot({
    m = model_fit()
    req(m)  # need a fitted model
    
    mf = model.frame(m)
    response  = names(mf)[1]
    predictors = names(mf)[-1]
    
    # Pick one numeric predictor for x
    num_preds = predictors[sapply(mf[predictors], is.numeric)]
    if (length(num_preds) == 0) {
      plot.new()
      text(0.5, 0.5, "No numeric predictors to plot.", cex = 1.2)
      return(invisible())
    }
    xvar = num_preds[1]
    
    # Optionally pick one factor for grouping
    fac_preds = predictors[sapply(mf[predictors], is.factor)]
    fvar = if (length(fac_preds) > 0) fac_preds[1] else NULL
    
    # Build grid for fitted lines
    x_seq = seq(min(mf[[xvar]], na.rm = TRUE),
                max(mf[[xvar]], na.rm = TRUE),
                length.out = 100)
    
    grid_list = list()
    grid_list[[xvar]] = x_seq
    
    if (!is.null(fvar)) {
      grid_list[[fvar]] = levels(mf[[fvar]])
    }
    
    # For any other predictors, hold them at a typical value
    other_preds = setdiff(predictors, c(xvar, fvar))
    for (v in other_preds) {
      x = mf[[v]]
      if (is.numeric(x)) {
        grid_list[[v]] = mean(x, na.rm = TRUE)
      } else if (is.factor(x)) {
        grid_list[[v]] = levels(x)[1]
      } else {
        # leave out weird types
      }
    }
    
    newdata = expand.grid(grid_list, stringsAsFactors = FALSE)
    
    # Predictions on response scale
    if (inherits(m, "glm")) {
      fit_vals = predict(m, newdata = newdata, type = "response")
    } else {
      fit_vals = predict(m, newdata = newdata)
    }
    newdata$fit = fit_vals
    
    # Build plot
    if (is.null(fvar)) {
      ggplot() +
        geom_point(
          data = mf,
          aes_string(x = xvar, y = response),
          alpha = 0.6
        ) +
        geom_line(
          data = newdata,
          aes_string(x = xvar, y = "fit"),
          linewidth = 1
        ) +
        labs(x = xvar, y = response)
    } else {
      ggplot() +
        geom_point(
          data = mf,
          aes_string(x = xvar, y = response, colour = fvar),
          alpha = 0.6
        ) +
        geom_line(
          data = newdata,
          aes_string(x = xvar, y = "fit", colour = fvar),
          linewidth = 1
        ) +
        labs(x = xvar, y = response, colour = fvar)
    }
  })
  
  
  output$model_formula = renderUI({
    m = model_fit()
    if (is.null(m)) {
      return(helpText("Fit a model to see the model formula."))
    }
    
    mf = model.frame(m)
    response = names(mf)[1]
    predictors = names(mf)[-1]
    
    terms_obj   = terms(m)
    term_labels = attr(terms_obj, "term.labels")
    data_classes = attr(terms_obj, "dataClasses")
    
    terms_tex = c("\\beta_0")
    beta_idx  = 1L
    
    # ----- Main effects -----
    for (lab in term_labels[!grepl(":", term_labels, fixed = TRUE)]) {
      v = lab
      cls = data_classes[[v]]
      x   = mf[[v]]
      is_cat = !is.null(cls) && cls %in% c("factor", "ordered", "character", "logical")
      
      if (is_cat) {
        if (!is.factor(x)) {
          x = as.factor(x)
        }
        lvls = levels(x)
        if (length(lvls) >= 2) {
          for (lvl in lvls[-1]) {
            terms_tex = c(
              terms_tex,
              glue("\\beta_{beta_idx} \\times \\mathbf{{1}}\\{{ {v}_i = \\text{{\"{lvl}\"}} \\}}")
            )
            beta_idx = beta_idx + 1L
          }
        }
      } else {
        terms_tex = c(
          terms_tex,
          glue("\\beta_{beta_idx} \\times {v}_i")
        )
        beta_idx = beta_idx + 1L
      }
    }
    
    # ----- Interaction terms (2-way only) -----
    interaction_labels = term_labels[grepl(":", term_labels, fixed = TRUE)]
    
    for (lab in interaction_labels) {
      vars = strsplit(lab, ":", fixed = TRUE)[[1]]
      if (length(vars) != 2) {
        next
      }
      
      v1 = vars[1]
      v2 = vars[2]
      x1 = mf[[v1]]
      x2 = mf[[v2]]
      cls1 = data_classes[[v1]]
      cls2 = data_classes[[v2]]
      is_cat1 = !is.null(cls1) && cls1 %in% c("factor", "ordered", "character", "logical")
      is_cat2 = !is.null(cls2) && cls2 %in% c("factor", "ordered", "character", "logical")
      
      # numeric:numeric
      if (!is_cat1 && !is_cat2) {
        terms_tex = c(
          terms_tex,
          glue("\\beta_{beta_idx} \\times {v1}_i \\times {v2}_i")
        )
        beta_idx = beta_idx + 1L
        next
      }
      
      # factor:numeric
      if (is_cat1 && !is_cat2) {
        fac_var = v1
        num_var = v2
        fac_x   = if (is.factor(x1)) x1 else as.factor(x1)
      } else if (!is_cat1 && is_cat2) {
        fac_var = v2
        num_var = v1
        fac_x   = if (is.factor(x2)) x2 else as.factor(x2)
      } else {
        # factor:factor (skip or handle similarly if you want)
        next
      }
      
      lvls = levels(fac_x)
      if (length(lvls) >= 2) {
        for (lvl in lvls[-1]) {
          terms_tex = c(
            terms_tex,
            glue(
              "\\beta_{beta_idx} \\times {num_var}_i \\times \\mathbf{{1}}\\{{ {fac_var}_i = \\text{{\"{lvl}\"}} \\}}"
            )
          )
          beta_idx = beta_idx + 1L
        }
      }
    }
    
    rhs = paste(terms_tex, collapse = " + ")
    
    # ----- LHS -----
    if (inherits(m, "glm")) {
      fam  = m$family$family
      link = m$family$link
      
      if (fam == "binomial" && link == "logit") {
        lhs = "\\operatorname{logit}(p_i)"
      } else if (fam == "poisson" && link == "log") {
        lhs = "\\log(\\mu_i)"
      } else {
        if (length(predictors) > 0) {
          cond = paste0(predictors, "_i", collapse = ", ")
          lhs = glue("\\mathrm{{E}}[{response}_i \\mid {cond}]")
        } else {
          lhs = glue("\\mathrm{{E}}[{response}_i]")
        }
      }
    } else {
      if (length(predictors) > 0) {
        cond = paste0(predictors, "_i", collapse = ", ")
        lhs = glue("\\mathrm{{E}}[{response}_i \\mid {cond}]")
      } else {
        lhs = glue("\\mathrm{{E}}[{response}_i]")
      }
    }
    
    formula_tex = glue("$$
{lhs} = {rhs}
$$")
    
    withMathJax(HTML(formula_tex))
  })
  
  # ------- Helper: formula checker -------
  check_formula = function() {
    if (is.null(rv$data)) {
      return(list(ok = FALSE, msg = "Load a data set first."))
    }
    
    txt = trimws(input$formula_text)
    
    if (txt == "") {
      return(list(
        ok = FALSE,
        msg = "Enter a model formula, e.g.  y ~ x1 + x2"
      ))
    }
    
    # basic character whitelist (allows interactions via * and :)
    if (!grepl("^[~+*:0-9A-Za-z_(). -]+$", txt)) {
      return(list(ok = FALSE, msg = "Formula contains illegal characters."))
    }
    
    # parse as formula
    f = tryCatch({
      as.formula(txt)
    }, error = function(e) {
      NULL
    })
    
    if (is.null(f)) {
      return(list(ok = FALSE, msg = "Formula is not syntactically valid."))
    }
    
    # check variables exist in data
    vars = all.vars(f)
    missing = setdiff(vars, names(rv$data))
    
    if (length(missing) > 0) {
      return(list(
        ok = FALSE,
        msg = paste("Unknown variable(s):", paste(missing, collapse = ", "))
      ))
    }
    
    list(ok = TRUE, msg = "Formula OK.")
  }
  
  # ------- Helper: load delimited text (csv/txt) -------
  load_delimited = function(sep) {
    df = tryCatch({
      read.table(
        input$file$datapath,
        sep = sep,
        header = TRUE,
        stringsAsFactors = FALSE,
        check.names = TRUE,
        fill = TRUE
      )
    }, error = function(e) {
      NULL
    })
    
    if (is.null(df)) {
      showNotification("Failed to read file with the chosen separator.", type = "error")
      return(NULL)
    }
    
    rv$data = df
    rv$all_vars = names(df)
    rv$auto_formula = ""
    rv$vars_bucket = names(df)
    rv$factors_bucket = character(0)
    rv$cont_bucket = character(0)
    model_fit(NULL)
    rv$model_equations = NULL
    rv$model_explanation = NULL
    updateTextInput(session, "formula_text", value = "")
  }
  
  # ---- Load dataset when file is chosen ----
  observeEvent(input$file, {
    req(input$file)
    
    ext = tolower(file_ext(input$file$name))
    
    # ---- RDA/RData ----
    if (ext %in% c("rda", "rdata")) {
      e = new.env()
      loaded = load(input$file$datapath, envir = e)
      df_names = loaded[sapply(loaded, function(x) {
        is.data.frame(e[[x]])
      })]
      
      if (length(df_names) == 0) {
        showNotification("No data frame in RDA file.", type = "error")
        return(NULL)
      }
      
      df = e[[df_names[1]]]
      
      rv$data = df
      rv$all_vars = names(df)
      rv$auto_formula = ""
      model_fit(NULL)
      rv$model_equations = NULL
      rv$model_explanation = NULL
      updateTextInput(session, "formula_text", value = "")
      return(NULL)
    }
    
    # ---- CSV or TXT ----
    if (ext %in% c("csv", "txt")) {
      
      # CSV: use comma separator directly
      if (ext == "csv") {
        load_delimited(",")
        return(NULL)
      }
      
      # TXT: ask user for separator via modal
      if (ext == "txt") {
        showModal(
          modalDialog(
            title = "Choose column separator",
            radioButtons(
              "sep_input",
              "Separator:",
              choices = c(
                "Comma (,)" = ",",
                "Tab (\\t)" = "\t",
                "Semicolon (;)" = ";",
                "Space ( )" = " ",
                "Other (type below)" = "OTHER"
              )
            ),
            textInput("sep_other", "If OTHER, type separator:", value = ""),
            footer = tagList(
              modalButton("Cancel"),
              actionButton("confirm_sep", "Load file")
            )
          )
        )
        return(NULL)
      }
    }
    
    # ---- Unsupported extension ----
    showNotification("Unsupported file type. Please upload CSV, TXT, or RDA.", type = "error")
  })
  
  # ---- Handle separator confirmation for TXT ----
  observeEvent(input$confirm_sep, {
    req(input$file)
    
    chosen = input$sep_input
    if (identical(chosen, "OTHER")) {
      chosen = input$sep_other
    }
    
    if (is.null(chosen) || chosen == "") {
      showNotification("Please specify a separator.", type = "error")
      return(NULL)
    }
    
    removeModal()
    load_delimited(chosen)
  })
  
  # ---- Drag-and-drop buckets UI ----
  output$var_buckets = renderUI({
    if (is.null(rv$data)) {
      return(helpText("Load a data set to see variables."))
    }
    
    # Remove the chosen response from the Variables list
    current_resp = input$response_var
    vars = rv$all_vars
    
    if (!is.null(current_resp) && nzchar(current_resp)) {
      vars = setdiff(vars, current_resp)
    }
    
    bucket_list(
      header = NULL,
      group_name = paste0("vars_group_", rv$bucket_group_id),  # <- changed
      orientation = "horizontal",
      add_rank_list(
        text = "Variables",
        labels = vars,
        input_id = "variables"
      ),
      add_rank_list(
        text = "Factors",
        labels = character(0),
        input_id = "factors"
      ),
      add_rank_list(
        text = "Continuous",
        labels = character(0),
        input_id = "continuous"
      )
    )
  })
  

  
  
  # ---- Response picker ----
  output$response_picker = renderUI({
    if (is.null(rv$data)) {
      return(NULL)
    }
    selectInput("response_var", "", choices = rv$all_vars)
  })
  
  # ---- Auto-populate formula from buckets (additive model) ----
  observeEvent(
    list(input$response_var, input$factors, input$continuous),
    {
      if (is.null(rv$data)) {
        return(NULL)
      }
      
      resp = input$response_var
      if (is.null(resp) || resp == "") {
        return(NULL)
      }
      
      factors = input$factors %||% character(0)
      cont    = input$continuous %||% character(0)
      
      preds = c(factors, cont)
      preds = unique(setdiff(preds, resp))
      
      rhs = if (length(preds) == 0) {
        "1"
      } else {
        paste(preds, collapse = " + ")
      }
      
      new_auto = paste(resp, "~", rhs)
      current  = trimws(input$formula_text)
      
      # Only overwrite if user hasn't customised the formula
      if (current == "" || current == rv$auto_formula) {
        rv$auto_formula = new_auto
        updateTextInput(session, "formula_text", value = new_auto)
      } else {
        rv$auto_formula = new_auto
      }
    }
  )
  
  # ---- Keep 'Variables' bucket in sync with chosen response ----
  observeEvent(input$response_var, {
    new_resp = input$response_var
    old_resp = rv$last_response
    
    # Add back old response if needed
    if (!is.null(old_resp) && old_resp != "") {
      if (!(old_resp %in% c(rv$factors_bucket, rv$cont_bucket, rv$vars_bucket))) {
        rv$vars_bucket = c(rv$vars_bucket, old_resp)
      }
    }
    
    # Remove new response
    rv$vars_bucket = setdiff(rv$vars_bucket, new_resp)
    
    rv$last_response = new_resp
  })
  
  
  
  # ---- Show formula validation status ----
  output$formula_status = renderText({
    res = check_formula()
    res$msg
  })
  
  # ---- Fit model when button clicked ----
  observeEvent(input$fit_btn, {
    res = check_formula()
    if (!res$ok) {
      showNotification(res$msg, type = "error")
      return(NULL)
    }
    
    f = as.formula(input$formula_text)
    resp_name = all.vars(f)[1]
    
    # Work on a copy of the data so we can safely coerce factors
    df_mod = rv$data
    
    # Anything in the Factors bucket should be treated as a factor
    factor_vars = input$factors %||% character(0)
    for (v in factor_vars) {
      if (!is.null(df_mod[[v]]) && !is.factor(df_mod[[v]])) {
        df_mod[[v]] = factor(df_mod[[v]])
      }
    }
    
    y = df_mod[[resp_name]]
    
    # Fit the chosen model type
    if (input$model_type == "lm") {
      m = lm(f, data = df_mod)
    } else if (input$model_type == "logistic") {
      if (!all(na.omit(y) %in% c(0, 1))) {
        showNotification(
          "Warning: response is not 0/1. Logistic regression expects a binary outcome.",
          type = "warning"
        )
      }
      m = glm(f, data = df_mod, family = binomial(link = "logit"))
    } else if (input$model_type == "poisson") {
      if (any(na.omit(y) < 0) || any(na.omit(y) %% 1 != 0)) {
        showNotification(
          "Warning: response has negative or non-integer values. Poisson regression expects non-negative counts.",
          type = "warning"
        )
      }
      m = glm(f, data = df_mod, family = poisson(link = "log"))
    } else {
      showNotification("Unknown model type.", type = "error")
      return(NULL)
    }
    
    model_fit(m)
    
    withProgress(message = "Talking to the language model...", value = 0, {
      incProgress(0.3, detail = "Deriving equations...")
      eq = lm_equations(m, chat_provider)
      
      incProgress(0.7, detail = "Writing explanation...")
      expl = lm_explanation(m, chat_provider)
      
      rv$model_equations   = eq
      rv$model_explanation = expl
      incProgress(1)
    })
  })
  
  
  observeEvent(input$reset_btn, {
    # Clear fitted model
    model_fit(NULL)
    
    # Clear stored equations / explanation
    rv$model_equations   = NULL
    rv$model_explanation = NULL
    
    # Reset auto-formula tracking
    rv$auto_formula = ""
    
    # Reset model type
    updateRadioButtons(session, "model_type", selected = "lm")
    
    # Reset formula input
    updateTextInput(session, "formula_text", value = "")
    
    # Reset the response selector to the first variable (like on initial load)
    if (!is.null(rv$data) && length(rv$all_vars) > 0) {
      updateSelectInput(
        session,
        "response_var",
        label   = NULL,
        choices = rv$all_vars,
        selected = rv$all_vars[1]   # <- use first variable as response
      )
    }
    
    rv$last_response   = NULL
    rv$bucket_group_id = rv$bucket_group_id + 1L  # force new buckets
  })
  
  
  
  output$model_equations = renderUI({
    eq = rv$model_equations
    if (is.null(eq)) {
      return(helpText("Fit a model to see the equations."))
    }
    
    scroll_style <- "
    max-height: 300px;
    overflow-y: auto;
    overflow-x: auto;
    padding: 8px;
    border: 1px solid #ccc;
    border-radius: 6px;
    background-color: #f9f9f9;
  "
    
    # Build inner content
    if (is.data.frame(eq) && all(c("condition", "equation") %in% names(eq))) {
      items = lapply(seq_len(nrow(eq)), function(i) {
        div(
          tags$p(tags$strong(eq$condition[i])),
          tags$pre(
            style = "white-space: pre; margin-top: -6px; margin-bottom: 8px;",
            eq$equation[i]
          )
        )
      })
      content <- tagList(items)
    } else if (is.character(eq)) {
      content <- tags$pre(
        style = "white-space: pre; margin: 0;",
        eq
      )
    } else {
      content <- tags$pre(
        style = "white-space: pre; margin: 0;",
        paste(capture.output(str(eq)), collapse = "\n")
      )
    }
    
    div(style = scroll_style, content)
  })
  
  
  # ---- Model explanation ----
  output$model_explanation = renderUI({
    expl = rv$model_explanation
    if (is.null(expl)) {
      return(helpText("Fit a model to see a textual explanation."))
    }
    tags$pre(
      style = "white-space: pre-wrap; word-wrap: break-word;",
      expl
    )
  })
  
  
  # ---- Display model summary ----
  output$model_output = renderPrint({
    m = model_fit()
    if (is.null(m)) {
      cat("No model fitted yet.")
    } else {
      summary(m)
    }
  })
}

shinyApp(ui = ui, server = server)
