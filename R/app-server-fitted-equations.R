#' Register fitted equation observers
#'
#' Wires the fitted equation, fitted means header, and fitted means table
#' outputs for the fitted model tab.
#'
#' @param output Shiny output object.
#' @param rv App server reactive values object.
#' @param modelFit Reactive value containing the fitted model.
#'
#' @return Invisibly returns NULL.
#'
#' @keywords internal
registerFittedEquationObservers = function(output, rv, modelFit) {
  # -------------------------------------------------------------------
  # Fitted equations / fitted means equations UI (scrollable box)
  # -------------------------------------------------------------------
  output$model_equations = renderUI({

    m = modelFit()
    # ---------------------------------------------------------------
    # If predictors are all factors, show "fitted means" equations
    # constructed from the regression coefficients
    # ---------------------------------------------------------------
    if (!is.null(m) && isFactorOnlyPredictorModel(m)) {

      info       = makeFittedMeansData(m)
      grid       = info$grid
      predictors = info$predictors
      mf         = info$mf

      makeLabel = function(oneRow) {
        if (length(predictors) == 0) {
          return("Mean")
        }
        paste0(
          "Mean(",
          paste(
            paste0(
              predictors, "=", vapply(predictors, function(v) as.character(oneRow[[v]]), "")
            ),
            collapse = ", "
          ),
          ")"
        )
      }

      eqLines = lapply(seq_len(nrow(grid)), function(i) {
        oneRow = grid[i, predictors, drop = FALSE]

        # Ensure factor columns carry the model's levels (important for model.matrix)
        for (v in predictors) {
          oneRow[[v]] = factor(oneRow[[v]], levels = levels(mf[[v]]))
        }

        makeMeanEquation(m, oneRowDf = oneRow, label = makeLabel(oneRow))
      })

      scrollStyle = "
      max-height: 300px;
      overflow-y: auto;
      overflow-x: auto;
      padding: 8px;
      border: 1px solid #ccc;
      border-radius: 6px;
      background-color: #f9f9f9;
    "

      return(div(
        style = scrollStyle,
        tagList(
          tags$p(
            tags$strong("How are the means constructed from the regression table?")
          ),
          tags$pre(
            style = "white-space: pre; margin: 0;",
            local({
              headingText = "Rounded to three significant figures for clarity"
              underline = paste(rep("-", nchar(headingText)), collapse = "")
              headingBlock = paste0(headingText, "\n", underline)

              paste(
                c(
                  headingBlock,
                  unlist(eqLines)
                ),
                collapse = "\n\n"
              )
            })
          )
        )
      ))
    }

    # ---------------------------------------------------------------
    # Otherwise, show the existing LLM-derived fitted equations
    # ---------------------------------------------------------------
    eq = rv$modelEquations
    if (is.null(eq)) {
      return(helpText("Fit a model to see the equations."))
    }

    scrollStyle = "
    max-height: 300px;
    overflow-y: auto;
    overflow-x: auto;
    padding: 8px;
    border: 1px solid #ccc;
    border-radius: 6px;
    background-color: #f9f9f9;
  "

    roleMetadata = buildEquationDisplayRoleMetadata(m)
    roleSummary = buildEquationDisplayRoleSummary(roleMetadata)

    roleSummaryUi = buildFittedEquationRoleSummaryUi(roleSummary)
    content = buildFittedEquationContentUi(eq, roleSummaryUi = roleSummaryUi)

    div(
      style = scrollStyle,
      tagList(
        content
      )
    )
  })

  # -------------------------------------------------------------------
  # Dynamic header for fitted equations / fitted means
  # -------------------------------------------------------------------
  output$model_equations_header = renderUI({
    m = modelFit()

    if (!is.null(m) && isFactorOnlyPredictorModel(m)) {
      if (inherits(m, "glm") && identical(m$family$family, "binomial") && identical(m$family$link, "logit")) {
        h4("Fitted probabilities and odds")
      } else if (inherits(m, "glm") && identical(m$family$family, "poisson") && identical(m$family$link, "log")) {
        h4("Fitted expected counts")
      } else {
        h4("Fitted means")
      }
    } else {
      h4("Fitted equations")
    }
  })

  # -------------------------------------------------------------------
  # Fitted means UI (scrollable box)
  # -------------------------------------------------------------------
  output$fitted_means = renderUI({
    m = modelFit()
    if (is.null(m)) {
      return(helpText("Fit a model to see fitted means."))
    }

    if (!isFactorOnlyPredictorModel(m)) {
      return(helpText("Fitted means are shown when all predictors are factors."))
    }

    info = makeFittedMeansData(m)
    predictors = info$predictors
    mf = info$mf
    grid = info$grid

    layout = chooseFactorLayout(mf, predictors)

    if (layout$type == "oneWay") {
      df = grid[order(grid[[layout$rowVar]]), , drop = FALSE]
      return(tagList(
        renderOneWayTable(df, layout$rowVar, ".fit")
      ))
    }

    if (layout$type == "twoWay") {
      df = grid
      return(tagList(
        renderTwoWayTable(df, layout$rowVar, layout$colVar, ".fit")
      ))
    }

    if (layout$type == "threeWay") {
      splitVar = layout$splitVar
      splitLevels = unique(grid[[splitVar]])

      tables = lapply(splitLevels, function(s) {
        df = grid[grid[[splitVar]] == s, , drop = FALSE]
        tagList(
          tags$h5(paste0(splitVar, " = ", s)),
          renderTwoWayTable(df, layout$rowVar, layout$colVar, ".fit")
        )
      })

      return(tagList(
        tables
      ))
    }


    helpText("Fitted means table layout is only implemented for 1-3 factor predictors.")
  })


  invisible(NULL)
}
