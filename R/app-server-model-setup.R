#' Register model setup observers
#'
#' Wires variable buckets, interaction selection, response selection,
#' derived-variable creation, and automatic formula updates for the app server.
#'
#' @param input Shiny input object.
#' @param output Shiny output object.
#' @param session Shiny session object.
#' @param rv App reactive values object.
#' @param setBucketState Function used to synchronise factor and continuous buckets.
#'
#' @return No return value; called for observer side effects.
#'
#' @keywords internal
registerModelSetupObservers = function(input, output, session, rv, setBucketState) {
  # -------------------------------------------------------------------
  # Drag-and-drop buckets UI
  # -------------------------------------------------------------------
  output$var_buckets = renderUI({
    if (is.null(rv$data)) {
      return(helpText("Load a data set to see variables."))
    }

    vars = rv$allVars %||% character(0)

    # Preserve current bucket contents across re-renders,
    # but sanitize against current dataset vars
    factors = intersect(rv$bucketFactors %||% character(0), vars)
    cont    = intersect(rv$bucketContinuous %||% character(0), vars)

    # Remove the chosen response AND anything already placed into buckets
    currentResp = input$response_var

    vars = setdiff(vars, c(currentResp, factors, cont))

    selectedAdjustmentVariables = sanitizeAdjustmentVariables(
      selectedVariables = rv$adjustmentVariables,
      eligibleVariables = buildEligibleAdjustmentVariables(
        responseVariable = currentResp,
        factorVariables = factors,
        continuousVariables = cont
      )
    )
    rv$adjustmentVariables = selectedAdjustmentVariables

    factorLabels = setNames(
      lapply(
        factors,
        function(variableName) {
          renderBucketVariableLabel(variableName, selectedAdjustmentVariables)
        }
      ),
      factors
    )

    continuousLabels = setNames(
      lapply(
        cont,
        function(variableName) {
          renderBucketVariableLabel(variableName, selectedAdjustmentVariables)
        }
      ),
      cont
    )

    bucket_list(
      header      = NULL,
      group_name  = paste0("vars_group_", rv$bucketGroupId),
      orientation = "horizontal",
      add_rank_list(
        text     = "Variables",
        labels   = vars,
        input_id = "variables"
      ),
      add_rank_list(
        text     = "Factors",
        labels   = factorLabels,
        input_id = "factors"
      ),
      add_rank_list(
        text     = "Numeric",
        labels   = continuousLabels,
        input_id = "continuous"
      )
    )
  })


  output$adjustment_variables_ui = renderUI({
    renderAdjustmentVariablesUi(
      rv = rv,
      responseVariable = input$response_var
    )
  })

  observeEvent(input$adjustment_variables, {
    syncAdjustmentVariablesSelection(
      rv = rv,
      responseVariable = input$response_var,
      selectedVariables = input$adjustment_variables
    )
  }, ignoreInit = TRUE)

  observeEvent(input$adjustment_variables_inline, {
    syncAdjustmentVariablesSelection(
      rv = rv,
      responseVariable = input$response_var,
      selectedVariables = input$adjustment_variables_inline
    )
  }, ignoreInit = TRUE)

  # -------------------------------------------------------------------
  # Interactions UI (2-way and 3-way) built from Factor + Continuous buckets
  # -------------------------------------------------------------------
  output$interaction_ui = renderUI({
    if (is.null(rv$data)) {
      return(NULL)
    }

    factors = rv$bucketFactors %||% character(0)
    cont    = rv$bucketContinuous %||% character(0)
    resp    = input$response_var

    # Exclude the response from predictors
    predsAll = unique(setdiff(c(factors, cont), resp))

    # Hard limit: only first 3 predictors are allowed in the model
    predsLimited = predsAll
    if (length(predsLimited) > 3) {
      predsLimited = predsLimited[1:3]
    }

    if (length(predsLimited) < 2) {
      return(NULL)
    }

    # We'll label types using the full bucket info, but only build
    # interactions from predsLimited
    varType = c(
      setNames(rep("(F)", length(factors)), factors),
      setNames(rep("(C)", length(cont)),    cont)
    )

    # ---- Build 2-way and 3-way combinations ----
    combos2 = list()
    combos3 = list()

    if (length(predsLimited) >= 2) {
      combos2 = asplit(combn(predsLimited, 2), 2)
    }
    if (length(predsLimited) >= 3) {
      combos3 = asplit(combn(predsLimited, 3), 2)
    }

    combos = c(combos2, combos3)
    if (length(combos) == 0) {
      return(NULL)
    }

    # Internal values: "var1:var2" or "var1:var2:var3"
    values = vapply(
      combos,
      function(x) {
        paste(x, collapse = ":")
      },
      FUN.VALUE = character(1)
    )

    adjustmentVariables = rv$adjustmentVariables %||% character(0)

    # Pretty labels with (F)/(C), : signs, and adjustment-role guardrails
    labels = vapply(
      combos,
      function(x) {
        interactionLabel = paste(
          sprintf("%s %s", x, varType[x]),
          collapse = " : "
        )

        if (any(x %in% adjustmentVariables)) {
          interactionLabel = paste0(interactionLabel, " [includes adjustment variable]")
        }

        interactionLabel
      },
      FUN.VALUE = character(1)
    )

    # Add a special "all interactions" option at the top
    allValue = "__ALL_INTERACTIONS__"
    allLabel = "All possible 2-way and 3-way interactions"

    choiceValues = c(allValue, values)
    choiceLabels = c(allLabel, labels)

    choices = setNames(choiceValues, choiceLabels)

    currentInteractions = input$interactions %||% character(0)
    pendingInteractions = rv$pendingExampleInteractions %||% character(0)
    selectedInteractions = intersect(
      unique(c(pendingInteractions, currentInteractions)),
      choiceValues
    )

    infoText = NULL
    if (length(predsAll) > 3) {
      infoText = helpText(
        sprintf(
          "You have placed %d variables into the Factors/Continuous buckets. ",
          length(predsAll)
        ),
        "Only the first 3 (",
        paste(predsLimited, collapse = ", "),
        ") are used in the model and for interactions."
      )
    }

    tagList(
      h5("Interactions (optional)"),
      infoText,
      helpText("Select 2-way or 3-way interaction terms to include in the model formula."),
      selectInput(
        inputId  = "interactions",
        label    = NULL,
        choices  = choices,
        selected = selectedInteractions,
        multiple = TRUE,
        width    = "100%"
      )
    )
  })

  # -------------------------------------------------------------------
  observeEvent(input$interactions, {
    if (isTRUE(rv$isResetting)) {
      return(NULL)
    }

    currentInteractions = input$interactions %||% character(0)
    pendingInteractions = rv$pendingExampleInteractions %||% character(0)

    if (length(currentInteractions) == 0) {
      return(NULL)
    }

    if (length(pendingInteractions) == 0) {
      return(NULL)
    }

    if (all(pendingInteractions %in% currentInteractions)) {
      rv$pendingExampleInteractions = character(0)
    }
  }, ignoreInit = TRUE)

  # When user selects "All possible interactions", expand to all codes
  # -------------------------------------------------------------------
  observeEvent(
    input$interactions,
    {
      ints = input$interactions %||% character(0)

      if (!"__ALL_INTERACTIONS__" %in% ints) {
        return(NULL)
      }

      buckets = getCurrentBuckets()
      factors = buckets$factors
      cont = buckets$continuous
      resp = input$response_var

      setBucketState(
        factors = factors,
        continuous = cont
      )

      predsAll = unique(setdiff(c(factors, cont), resp))

      predsLimited = predsAll
      if (length(predsLimited) > 3) {
        predsLimited = predsLimited[1:3]
      }

      if (length(predsLimited) < 2) {
        updateSelectInput(
          session,
          "interactions",
          selected = character(0)
        )
        return(NULL)
      }

      combos2 = list()
      combos3 = list()

      if (length(predsLimited) >= 2) {
        combos2 = asplit(combn(predsLimited, 2), 2)
      }
      if (length(predsLimited) >= 3) {
        combos3 = asplit(combn(predsLimited, 3), 2)
      }

      combos = c(combos2, combos3)

      if (length(combos) == 0) {
        updateSelectInput(
          session,
          "interactions",
          selected = character(0)
        )
        return(NULL)
      }

      allInts = vapply(
        combos,
        function(x) {
          paste(x, collapse = ":")
        },
        FUN.VALUE = character(1)
      )

      updateSelectInput(
        session,
        "interactions",
        selected = allInts
      )
    },
    ignoreInit = TRUE
  )

  # -------------------------------------------------------------------
  # Warn + confirm when a numeric variable is moved into the Factors bucket
  # -------------------------------------------------------------------
  observeEvent(input$factors, {
    if (isTRUE(rv$isResetting)) {
      return(NULL)
    }

    if (is.null(rv$data)) {
      rv$lastFactors = character(0)
      return(NULL)
    }

    currentFactors = intersect(input$factors %||% character(0), rv$allVars %||% character(0))
    previousFactors = intersect(rv$lastFactors %||% character(0), rv$allVars %||% character(0))

    newVars = setdiff(currentFactors, previousFactors)

    if (length(newVars) == 0) {
      rv$lastFactors = currentFactors
      return(NULL)
    }

    v = newVars[1]
    col = rv$data[[v]]

    if (is.numeric(col) && !is.factor(col)) {
      rv$pendingFactorVar = v

      showModal(
        modalDialog(
          title = buildNumericFactorConfirmTitle(),
          buildNumericFactorConfirmMessage(v),
          footer = tagList(
            actionButton("cancel_factor_numeric", buildNumericFactorCancelLabel()),
            actionButton("confirm_factor_numeric", buildNumericFactorConfirmLabel())
          )
        )
      )

      return(NULL)
    }

    rv$lastFactors = currentFactors
  })

  # -------------------------------------------------------------------
  # Add derived variable to the data + refresh buckets
  # -------------------------------------------------------------------
  observeEvent(input$addDerivedVarBtn, {
    if (is.null(rv$data)) {
      showNotification(buildLoadDataFirstMessage(), type = "message")
      return(NULL)
    }

    showModal(
      modalDialog(
        title = buildAddDerivedVariableTitle(),
        easyClose = TRUE,
        tagList(
          helpText(
            buildAddDerivedVariableHelpText()
          ),
          textInput(
            inputId = "derivedVarTextModal",
            label = NULL,
            placeholder = buildAddDerivedVariablePlaceholder(),
            value = ""
          )
        ),
        footer = tagList(
          modalButton("Cancel"),
          actionButton(
            "confirmAddDerivedVarBtn",
            buildAddDerivedVariableConfirmLabel(),
            class = "btn-success"
          )
        )
      )
    )
  }, ignoreInit = TRUE)

  observeEvent(input$confirmAddDerivedVarBtn, {
    if (is.null(rv$data)) {
      showNotification(buildLoadDataFirstMessage(), type = "message")
      return(NULL)
    }

    res = addDerivedVariableToData(rv$data, input$derivedVarTextModal)

    if (!isTRUE(res$ok)) {
      showNotification(res$msg, type = "error", duration = 8)
      return(NULL)
    }

    rv$data = res$data

    # Refresh variable list used by the buckets + response picker
    rv$allVars = names(rv$data)

    # Force buckets to re-render (without losing current placements)
    rv$bucketGroupId = rv$bucketGroupId + 1L

    # Keep the current response if possible, otherwise fall back to first column
    currentResp = input$response_var
    selectedResp = if (!is.null(currentResp) && nzchar(currentResp) && currentResp %in% rv$allVars) {
      currentResp
    } else {
      rv$allVars[1]
    }

    updateSelectInput(
      session,
      "response_var",
      choices  = rv$allVars,
      selected = selectedResp
    )

    removeModal()
    showNotification(res$msg, type = "message", duration = 4)
  }, ignoreInit = TRUE)

  # -------------------------------------------------------------------
  # Response picker
  # -------------------------------------------------------------------
  output$response_picker = renderUI({

    req(is.data.frame(rv$data))
    req(input$model_type)

    vars = names(rv$data)

    labels = vapply(
      vars,
      function(v) {
        chk = validateResponseVar(rv$data, v, input$model_type)
        if (isTRUE(chk$ok)) {
          v
        } else {
          paste0("\u274C ", v)
        }
      },
      character(1)
    )

    choices = setNames(vars, labels)

    current = rv$lastResponse %||% input$response_var %||% ""
    selected = if (nzchar(current) && current %in% vars) current else vars[1]

    selectInput(
      inputId  = "response_var",
      label    = "",
      choices  = choices,
      selected = selected
    )
  })

  output$response_explain = renderUI({

    req(is.data.frame(rv$data))
    req(input$model_type)

    resp = input$response_var %||% ""
    if (!nzchar(resp) || !(resp %in% names(rv$data))) {
      return(NULL)
    }

    chk = validateResponseVar(rv$data, resp, input$model_type)

    if (isTRUE(chk$ok)) {
      return(NULL)
    }

    tags$div(
      style = "margin-top: 6px; color: #b00020;",
      paste0("\u274C ", chk$reason)
    )
  })


  # -------------------------------------------------------------------
  # Helper: current valid bucket contents for the active dataset
  # -------------------------------------------------------------------
  getCurrentBuckets = function() {
    vars = rv$allVars %||% character(0)

    factors = intersect(rv$bucketFactors %||% character(0), vars)
    cont = intersect(rv$bucketContinuous %||% character(0), vars)

    list(
      factors = factors,
      continuous = cont
    )
  }

  # -------------------------------------------------------------------
  # Auto-populate formula from buckets + optional interactions
  # (limit to 3 predictors; expert mode for compact notation)
  # -------------------------------------------------------------------
  observeEvent(
    list(
      input$response_var,
      rv$bucketFactors,
      rv$bucketContinuous,
      input$interactions,
      input$expert_mode
    ),
    {
      if (isTRUE(rv$isResetting)) {
        return(NULL)
      }

      if (is.null(rv$data)) {
        return(NULL)
      }

      resp = input$response_var
      if (is.null(resp) || resp == "" || !(resp %in% rv$allVars)) {
        return(NULL)
      }

      buckets = getCurrentBuckets()
      factors = buckets$factors
      cont = buckets$continuous
      ints = unique(c(
        rv$pendingExampleInteractions %||% character(0),
        input$interactions %||% character(0)
      ))

      predsAll = unique(setdiff(c(factors, cont), resp))

      preds = predsAll
      if (length(preds) > 3) {
        preds = preds[1:3]
      }

      if (length(preds) == 0) {
        newAuto = paste(resp, "~ 1")
        current = trimws(input$formula_text)

        if (current == "" || current == rv$autoFormula) {
          rv$autoFormula = newAuto
          updateTextInput(session, "formula_text", value = newAuto)
        } else {
          rv$autoFormula = newAuto
        }
        return(NULL)
      }

      ints = Filter(
        function(z) {
          all(strsplit(z, ":", fixed = TRUE)[[1]] %in% preds)
        },
        ints
      )

      expertRhs = NULL
      expertOn = isTRUE(input$expert_mode)

      if (expertOn) {
        if (length(preds) == 2 && length(ints) == 1) {
          varsInt = strsplit(ints[1], ":", fixed = TRUE)[[1]]
          if (length(varsInt) == 2 && setequal(varsInt, preds)) {
            expertRhs = paste(preds, collapse = " * ")
          }
        }

        if (is.null(expertRhs) && length(preds) >= 2 && length(ints) > 0) {
          lenInts = vapply(
            strsplit(ints, ":", fixed = TRUE),
            length,
            FUN.VALUE = integer(1)
          )
          pairInts = ints[lenInts == 2]
          higherInts = ints[lenInts > 2]

          if (length(higherInts) == 0) {
            allPairs = apply(
              combn(preds, 2),
              2,
              function(x) {
                paste(x, collapse = ":")
              }
            )
            if (length(pairInts) == length(allPairs) && setequal(pairInts, allPairs)) {
              expertRhs = paste0("(", paste(preds, collapse = " + "), ")^2")
            }
          }
        }
      }

      if (!is.null(expertRhs)) {
        rhs = expertRhs
      } else {
        rhsPieces = character(0)
        mainPart = paste(preds, collapse = " + ")
        rhsPieces = c(rhsPieces, mainPart)

        if (length(ints) > 0) {
          intPart = paste(ints, collapse = " + ")
          rhsPieces = c(rhsPieces, intPart)
        }

        rhs = paste(rhsPieces, collapse = " + ")
      }

      newAuto = paste(resp, "~", rhs)
      current = trimws(input$formula_text)

      if (current == "" || current == rv$autoFormula) {
        rv$autoFormula = newAuto
        updateTextInput(session, "formula_text", value = newAuto)
      } else {
        rv$autoFormula = newAuto
      }
    }
  )

  # -------------------------------------------------------------------
  # User response to numeric-as-factor confirmation
  # -------------------------------------------------------------------
  observeEvent(input$confirm_factor_numeric, {
    req(rv$pendingFactorVar)
    removeModal()

    rv$lastFactors      = input$factors %||% character(0)
    v                   = rv$pendingFactorVar
    rv$pendingFactorVar = NULL
    rv$pendingExampleInteractions = character(0)

    showNotification(
      paste0(
        "Variable '", v,
        "' will be treated as a factor when fitting the model."
      ),
      type     = "message",
      duration = 6
    )
  }, ignoreInit = TRUE)

  observeEvent(input$cancel_factor_numeric, {
    req(rv$pendingFactorVar)
    removeModal()

    rv$lastFactors = input$factors %||% character(0)

    showNotification(
      paste0(
        "If you do not want '", rv$pendingFactorVar,
        "' treated as a factor, drag it back to the Continuous bucket."
      ),
      type     = "warning",
      duration = 10
    )

    rv$pendingFactorVar = NULL
    rv$pendingExampleInteractions = character(0)
  }, ignoreInit = TRUE)

  # -------------------------------------------------------------------
  # Keep 'Variables' bucket in sync with chosen response (if needed)
  # -------------------------------------------------------------------
  observeEvent(input$response_var, {
    newResp         = input$response_var
    rv$lastResponse = newResp
  })
}
