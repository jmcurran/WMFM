.env_cache = new.env(parent = emptyenv())

library(shiny)
library(tools)
library(sortable)
library(ellmer)
library(glue)
library(tibble)

#----------------------------------------
# LLM provider
#----------------------------------------

get_chat_provider = function() {
  # If you later want OpenAI, you can uncomment this:
  # if (nzchar(Sys.getenv("OPENAI_API_KEY"))) {
  #   return(chat_openai())
  # }
  
  # For now: always use Ollama
  chat_ollama(model = "gpt-oss")
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
  
  glue("
You are given output from an R regression model.
{model_desc}

Your task is to write fitted-model equations.

Response: {response}

Predictors:
{paste(pred_info, collapse = '\\n')}

Coefficient table:
{coef_txt}

Rules:
- Round coefficients to 2 decimals.
- For linear regression (Gaussian, identity link):
  * Write an equation like: {response} = b0 + b1 * X1 + ...
- For binomial GLMs with logit link:
  * Write the equation on the log-odds (logit) scale, e.g. logit(p) = ...
  * Optionally also give p = exp(eta) / (1 + exp(eta)).
- For Poisson GLMs with log link:
  * Write the equation on the log scale, e.g. log(mu) = ...
  * Optionally also give mu = exp(eta).
- For factor predictors, give one equation per relevant factor level by adjusting the intercept.
- If there are interactions, reflect how effects depend on the interacting variables.
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
  
  # ---- Model type + Response + formula ----
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
  
  h4("Model explanation"),
  uiOutput("model_explanation")
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
    model_explanation = NULL
    # last_response = NULL  # optional now
  )
  
  
  model_fit = reactiveVal(NULL)
  
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
      group_name = "vars_group",
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
    y = rv$data[[resp_name]]
    
    # Fit the chosen model type
    if (input$model_type == "lm") {
      m = lm(f, data = rv$data)
    } else if (input$model_type == "logistic") {
      # quick sanity warning
      if (!all(na.omit(y) %in% c(0, 1))) {
        showNotification(
          "Warning: response is not 0/1. Logistic regression expects a binary outcome.",
          type = "warning"
        )
      }
      m = glm(f, data = rv$data, family = binomial(link = "logit"))
    } else if (input$model_type == "poisson") {
      if (any(na.omit(y) < 0) || any(na.omit(y) %% 1 != 0)) {
        showNotification(
          "Warning: response has negative or non-integer values. Poisson regression expects non-negative counts.",
          type = "warning"
        )
      }
      m = glm(f, data = rv$data, family = poisson(link = "log"))
    } else {
      showNotification("Unknown model type.", type = "error")
      return(NULL)
    }
    
    model_fit(m)
    
    # Show a progress bar while talking to the LLM
    withProgress(message = "Talking to the language model...", value = 0, {
      incProgress(0.3, detail = "Deriving equations...")
      eq = lm_equations(m, chat_provider)
      
      incProgress(0.7, detail = "Writing explanation...")
      expl = lm_explanation(m, chat_provider)
      
      rv$model_equations = eq
      rv$model_explanation = expl
      incProgress(1)
    })
  })
  
  # ---- Nicely formatted equations ----
  output$model_equations = renderUI({
    eq = rv$model_equations
    if (is.null(eq)) {
      return(helpText("Fit a model to see the equations."))
    }
    
    # Structured tibble: condition + equation
    if (is.data.frame(eq) && all(c("condition", "equation") %in% names(eq))) {
      items = lapply(seq_len(nrow(eq)), function(i) {
        div(
          tags$p(tags$strong(eq$condition[i])),
          tags$code(eq$equation[i])
        )
      })
      tagList(items)
    } else if (is.character(eq)) {
      # Plain text from Ollama
      tags$pre(eq)
    } else {
      # Fallback
      tags$pre(capture.output(str(eq)))
    }
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
