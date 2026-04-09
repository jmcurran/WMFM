#' Register model output tabs (ANOVA + confidence intervals)
#'
#' Defines Shiny outputs used in the Fitted Model tab:
#' - ANOVA / analysis of deviance
#' - Confidence interval table
#' - Drill-down row explanation and optional teaching material
#'
#' @param output Shiny output object.
#' @param input Shiny input object.
#' @param modelFit reactiveVal holding fitted model.
#'
#' @return None (called for side effects).
#' @keywords internal
#'
#' @importFrom shiny helpText renderPrint renderTable renderUI req selectInput tableOutput
#' @importFrom shiny tagList tags verbatimTextOutput
registerModelOutputTabs = function(output, input, modelFit) {

  getCiData = function() {
    m = modelFit()

    if (is.null(m)) {
      return(NULL)
    }

    buildModelConfidenceIntervalData(
      model = m,
      numericReference = "zero"
    )
  }

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

  output$modelConfintNoteUi = renderUI({

    ciData = getCiData()

    if (is.null(ciData)) {
      return(helpText("Fit a model to see confidence intervals."))
    }

    noteText = ciData$note %||% paste(
      "Use the table first. If you want to unpack one interval, choose a single row below to see how it was built."
    )

    helpText(noteText)
  })

  output$modelConfintTable = renderTable({

    ciData = getCiData()

    if (is.null(ciData)) {
      return(NULL)
    }

    ciData$table

  }, striped = TRUE, bordered = TRUE, spacing = "s")

  output$modelConfintSelectorUi = renderUI({

    ciData = getCiData()

    if (is.null(ciData)) {
      return(NULL)
    }

    selectInput(
      inputId = "modelConfintSelectedRow",
      label = "Explain this row",
      choices = buildModelConfidenceIntervalRowChoices(ciData),
      selected = ""
    )
  })

  output$modelConfintSelectedRowUi = renderUI({

    ciData = getCiData()

    if (is.null(ciData)) {
      return(NULL)
    }

    detail = findModelConfidenceIntervalDetail(
      ciData = ciData,
      selectedLabel = input$modelConfintSelectedRow %||% ""
    )

    if (is.null(detail)) {
      return(
        helpText("Choose a row if you want to see how that interval was constructed.")
      )
    }

    tagList(
      tags$h5("Selected-row explanation"),
      renderModelConfidenceIntervalDetailUi(detail)
    )
  })

  output$modelConfintTeachingNoteUi = renderUI({

    ciData = getCiData()

    if (is.null(ciData) || is.null(ciData$teachingNote) || !nzchar(ciData$teachingNote)) {
      return(NULL)
    }

    tags$details(
      tags$summary("Teaching note"),
      tags$div(
        style = "margin-top: 8px;",
        helpText(ciData$teachingNote)
      )
    )
  })

  output$modelConfintVcovUi = renderUI({

    ciData = getCiData()

    if (is.null(ciData) || is.null(ciData$vcovTable)) {
      return(NULL)
    }

    tags$details(
      tags$summary("Variance-covariance matrix"),
      tags$div(
        style = "margin-top: 8px;",
        tableOutput("modelConfintVcovTable")
      )
    )
  })

  output$modelConfintVcovTable = renderTable({

    ciData = getCiData()

    if (is.null(ciData) || is.null(ciData$vcovTable)) {
      return(NULL)
    }

    as.data.frame(ciData$vcovTable)

  }, striped = TRUE, bordered = TRUE, spacing = "s", rownames = TRUE)
}
