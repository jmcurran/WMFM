#' Register symbolic model formula observers
#'
#' Wires the symbolic model formula output for the fitted model tab.
#'
#' @param output Shiny output object.
#' @param modelFit Reactive value containing the fitted model.
#'
#' @return Invisibly returns NULL.
#'
#' @keywords internal
registerModelFormulaObservers = function(output, modelFit) {
  # -------------------------------------------------------------------
  # Symbolic model formula (LaTeX via MathJax)
  # -------------------------------------------------------------------
  output$model_formula = renderUI({
    m = modelFit()
    if (is.null(m)) {
      return(helpText("Fit a model to see the model formula."))
    }

    mf = model.frame(m)
    response = names(mf)[1]

    # Terms object gives us main effects + interactions
    tt = terms(m)
    termLabels = attr(tt, "term.labels")
    mainLabels = termLabels[!grepl(":", termLabels)]
    intLabels  = termLabels[grepl(":", termLabels)]

    # ----- Build RHS: main effects first -----
    termsTex = c("\\beta_0")
    betaIdx = 1L

    for (lbl in mainLabels) {
      v = lbl
      x = mf[[v]]

      if (is.factor(x)) {
        lvls = levels(x)
        if (length(lvls) >= 2) {
          # One indicator per non-reference level
          for (lvl in lvls[-1]) {
            termsTex = c(
              termsTex,
              glue("\\beta_{betaIdx} \\times \\mathbf{{1}}\\{{ {v}_i = \\text{{\"{lvl}\"}} \\}}")
            )
            betaIdx = betaIdx + 1L
          }
        }
      } else {
        # Numeric predictor
        termsTex = c(
          termsTex,
          glue("\\beta_{betaIdx} \\times {v}_i")
        )
        betaIdx = betaIdx + 1L
      }
    }

    # ----- Add interaction terms -----
    for (lbl in intLabels) {
      vars = strsplit(lbl, ":", fixed = TRUE)[[1]]

      # ---------------- Two-way interactions: keep detailed expansion ----------------
      if (length(vars) == 2) {
        v1 = vars[1]
        v2 = vars[2]
        x1 = mf[[v1]]
        x2 = mf[[v2]]

        isFac1 = is.factor(x1)
        isFac2 = is.factor(x2)

        # numeric x numeric
        if (!isFac1 && !isFac2) {
          termsTex = c(
            termsTex,
            glue("\\beta_{betaIdx} \\times {v1}_i \\times {v2}_i")
          )
          betaIdx = betaIdx + 1L

          # factor x numeric
        } else if (isFac1 && !isFac2) {
          lvls1 = levels(x1)
          for (lvl1 in lvls1[-1]) {
            termsTex = c(
              termsTex,
              glue("\\beta_{betaIdx} \\times {v2}_i \\times \\mathbf{{1}}\\{{ {v1}_i = \\text{{\"{lvl1}\"}} \\}}")
            )
            betaIdx = betaIdx + 1L
          }

          # numeric x factor
        } else if (!isFac1 && isFac2) {
          lvls2 = levels(x2)
          for (lvl2 in lvls2[-1]) {
            termsTex = c(
              termsTex,
              glue("\\beta_{betaIdx} \\times {v1}_i \\times \\mathbf{{1}}\\{{ {v2}_i = \\text{{\"{lvl2}\"}} \\}}")
            )
            betaIdx = betaIdx + 1L
          }

          # factor x factor
        } else {
          lvls1 = levels(x1)
          lvls2 = levels(x2)
          for (lvl1 in lvls1[-1]) {
            for (lvl2 in lvls2[-1]) {
              termsTex = c(
                termsTex,
                glue(
                  "\\beta_{betaIdx} \\times \\mathbf{{1}}\\{{ {v1}_i = \\text{{\"{lvl1}\"}}, {v2}_i = \\text{{\"{lvl2}\"}} \\}}"
                )
              )
              betaIdx = betaIdx + 1L
            }
          }
        }

        # ---------------- Three-way interactions: generic product term ----------------
      } else if (length(vars) == 3) {
        v1 = vars[1]
        v2 = vars[2]
        v3 = vars[3]

        # For readability, we show a compact generic interaction term,
        # regardless of whether the variables are factors or numeric.
        termsTex = c(
          termsTex,
          glue("\\beta_{betaIdx} \\times {v1}_i \\times {v2}_i \\times {v3}_i")
        )
        betaIdx = betaIdx + 1L

        # Skip higher-order (4-way+) interactions if present
      } else {
        next
      }
    }

    # ----- LHS: depends on model type -----
    predictors = mainLabels       # for conditioning in E[. | .]
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

    # ----- Layout: single line vs align* for long formulas -----
    if (length(termsTex) <= 3) {
      rhsTex = paste(termsTex, collapse = " + ")
      formulaTex = glue("$$
{lhs} = {rhsTex}
$$")
    } else {
      firstLine = glue("{lhs} &= {termsTex[1]}")
      if (length(termsTex) > 1) {
        otherLines = paste0(" &+ ", termsTex[-1], collapse = " \\\\\n")
        body = paste0(firstLine, " \\\\\n", otherLines)
      } else {
        body = firstLine
      }
      formulaTex = glue("$$
\\begin{{align*}}
{body}
\\end{{align*}}
$$")
    }

    withMathJax(HTML(formulaTex))
  })


  invisible(NULL)
}
