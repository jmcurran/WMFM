#' Register model help and data context observers
#'
#' Wires the model-help button, package data description modal, uploaded-data
#' context modal, and research-question state synchronisation.
#'
#' @param input Shiny input object.
#' @param output Shiny output object.
#' @param session Shiny session object.
#' @param rv App reactive values object.
#'
#' @return No return value; called for observer side effects.
#'
#' @keywords internal
registerModelHelpObservers = function(input, output, session, rv) {
  # -------------------------------------------------------------------
  # Display the R help file if using s2ox data set
  # -------------------------------------------------------------------
  datasetLoaded = reactive({
    is.data.frame(rv$data)
  })

  output$modelHelpBtnUi = renderUI({
    isReady = datasetLoaded()
    source = input$data_source %||% "upload"

    btnLabel = if (identical(source, "package")) {
      "Data description"
    } else {
      "Provide data context"
    }

    tags$button(
      id    = "modelHelpBtn",
      type  = "button",
      class = "btn btn-outline-secondary action-button",
      disabled = if (!isReady) "disabled" else NULL,
      btnLabel
    )
  })

  output$userDatasetContextUi = renderUI({

    if (!identical(input$data_source %||% "", "upload") || !datasetLoaded()) {
      return(NULL)
    }

    ctx = trimws(rv$userDatasetContext %||% "")

    if (nzchar(ctx)) {
      tags$div(
        style = "margin-top: 6px; color: #2b6a2b;",
        "Data context has been provided."
      )
    } else {
      tags$div(
        style = "margin-top: 6px; color: #666;",
        "No data context has been provided yet."
      )
    }
  })


  output$modelHelpModalBody = renderUI({

    req(is.data.frame(rv$data))

    # Always: generated variable summary
    summaryDf = buildVarSummary(rv$data)

    # Optional: s20x help HTML
    helpHtml = NULL
    pkg = input$data_package %||% ""
    if (identical(input$data_source, "package") && identical(pkg, "s20x")) {
      helpHtml = getS20xDocHtml(input$package_dataset)
    }

    tagList(
      if (identical(input$data_source, "package") && identical(pkg, "s20x")) {
        tagList(
          h4("Package help"),
          if (!is.null(helpHtml) && nzchar(helpHtml)) {
            tags$div(
              style = "max-height: 280px; overflow-y: auto; border: 1px solid #ddd; padding: 10px; border-radius: 6px;",
              htmltools::HTML(helpHtml)
            )
          } else {
            tags$div(class = "alert alert-warning", "No help page found for this dataset.")
          },
          hr()
        )
      },

      h4("Variable summary"),
      helpText("Includes factor levels and quick type/missing summaries."),
      renderVarSummaryUi(summaryDf)
    )
  })


  # -------------------------------------------------------------------
  # Model tab: Help modal (Rd text if s20x + generated summary)
  # -------------------------------------------------------------------
  observeEvent(input$modelHelpBtn, {

    if (is.null(rv$data)) {
      showNotification(buildLoadDataFirstMessage(), type = "message")
      return(NULL)
    }

    if (identical(input$data_source %||% "", "package")) {
      titleText = buildDataDescriptionTitle(input$package_dataset %||% "")

      showModal(
        modalDialog(
          title = titleText,
          size = "l",
          easyClose = TRUE,
          uiOutput("modelHelpModalBody"),
          footer = tagList(
            modalButton("Close")
          )
        )
      )

      return(NULL)
    }

    showModal(
      modalDialog(
        title = buildProvideDataContextTitle(),
        size = "l",
        easyClose = TRUE,
        tagList(
          helpText(
            buildProvideDataContextHelpText()
          ),
          tags$textarea(
            id = "userDatasetContextModal",
            class = "form-control",
            rows = 8,
            placeholder = buildProvideDataContextPlaceholder(),
            rv$userDatasetContext %||% ""
          )
        ),
        footer = tagList(
          modalButton("Cancel"),
          actionButton("saveUserDatasetContextBtn", "Save data context", class = "btn-primary")
        )
      )
    )
  }, ignoreInit = TRUE)

  observeEvent(input$saveUserDatasetContextBtn, {
    rv$userDatasetContext = trimws(input$userDatasetContextModal %||% "")
    removeModal()

    if (nzchar(rv$userDatasetContext)) {
      showNotification(buildDataContextSavedMessage(), type = "message", duration = 4)
    } else {
      showNotification(buildDataContextClearedMessage(), type = "message", duration = 4)
    }
  }, ignoreInit = TRUE)

  observeEvent(input$researchQuestion, {
    rv$researchQuestion = trimws(input$researchQuestion %||% "")
  }, ignoreInit = FALSE)

    invisible(NULL)
}
