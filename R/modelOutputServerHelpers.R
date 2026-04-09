#' Register model output tabs (ANOVA + confidence intervals)
#'
#' Defines Shiny outputs used in the Fitted Model tab:
#' - ANOVA / analysis of deviance
#' - Confidence interval table
#' - Teaching notes and derivations
#'
#' @param output Shiny output object
#' @param modelFit reactiveVal holding fitted model
#'
#' @return None (called for side effects)
#' @keywords internal
registerModelOutputTabs = function(output, modelFit) {

  # -------------------------------------------------------------
  # ANOVA / Analysis of deviance
  # -------------------------------------------------------------
  output$model_anova = renderPrint({
    m = modelFit()

    if (is.null(m)) {
      cat("No model fitted yet.")
      return()
    }

    if (inherits(m, "glm")) {

      fam = m$family$family

      if (fam %in% c("poisson", "binomial")) {
        print(anova(m, test = "Chisq"))
      } else {
        print(anova(m))
      }

    } else {
      print(anova(m))
    }
  })


  # -------------------------------------------------------------
  # Confidence interval note
  # -------------------------------------------------------------
  output$modelConfintNoteUi = renderUI({

    m = modelFit()

    if (is.null(m)) {
      return(helpText("Fit a model to see confidence intervals."))
    }

    helpText(
      "Confidence intervals are shown for model coefficients and for",
      "quantities derived from combinations of coefficients.",
      "For GLMs, intervals are computed on the link scale and",
      "transformed back to the response scale."
    )
  })


  # -------------------------------------------------------------
  # Confidence interval table
  # -------------------------------------------------------------
  output$modelConfintTable = renderTable({

    m = modelFit()

    if (is.null(m)) {
      return(NULL)
    }

    buildModelConfidenceIntervalData(m)$table

  }, striped = TRUE, bordered = TRUE, spacing = "s")


  # -------------------------------------------------------------
  # Confidence interval derivations (teaching block)
  # -------------------------------------------------------------
  output$modelConfintDetailsUi = renderUI({

    m = modelFit()

    if (is.null(m)) {
      return(NULL)
    }

    details = buildModelConfidenceIntervalData(m)$details

    if (length(details) == 0) {
      return(helpText("No additional derivations available for this model."))
    }

    tagList(details)
  })
}
