#' Register data loading observers for the app server
#'
#' @param input Shiny input object.
#' @param session Shiny session object.
#' @param rv App reactive values object.
#' @param exampleLoadStatus Reactive value used to display example load status.
#' @param resetModelPage Function that clears model-page state after a data change.
#' @param applyLoadedExampleToInputs Function that applies loaded example input state.
#'
#' @return No return value; called for observer side effects.
#'
#' @keywords internal
registerDataLoadObservers = function(
  input,
  session,
  rv,
  exampleLoadStatus,
  resetModelPage,
  applyLoadedExampleToInputs
) {
  # -------------------------------------------------------------------
  # Helper: load delimited text (csv/txt)
  # -------------------------------------------------------------------
  loadDelimited = function(sep) {
    df = tryCatch({
      read.table(
        input$file$datapath,
        sep              = sep,
        header           = TRUE,
        stringsAsFactors = FALSE,
        check.names      = TRUE,
        fill             = TRUE
      )
    }, error = function(e) {
      NULL
    })

    if (is.null(df)) {
      showNotification(buildDelimitedFileReadFailedMessage(), type = "error")
      return(NULL)
    }

    rv$data = df
    rv$allVars = names(df)
    rv$userDatasetContext = ""
    rv$researchQuestion = ""
    rv$loadedExample = NULL
    exampleLoadStatus(buildExampleReadyStatus())
    updateTextInput(session, "researchQuestion", value = "")
    resetModelPage(resetResponse = TRUE)
  }

  # -------------------------------------------------------------------
  # Load dataset when file is chosen
  # -------------------------------------------------------------------
  observeEvent(input$file, {
    req(input$data_source == "upload")
    req(input$file)

    ext = tolower(file_ext(input$file$name))

    # RDA/RData
    if (ext %in% c("rda", "rdata")) {
      e       = new.env()
      loaded  = load(input$file$datapath, envir = e)
      dfNames = loaded[sapply(loaded, function(x) {
        is.data.frame(e[[x]])
      })]

      if (length(dfNames) == 0) {
        showNotification(buildNoDataFrameInRdaMessage(), type = "error")
        return(NULL)
      }

      df = e[[dfNames[1]]]

      rv$data = df
      rv$allVars = names(df)
      rv$userDatasetContext = ""
      rv$researchQuestion = ""
      updateTextInput(session, "researchQuestion", value = "")
      resetModelPage(resetResponse = TRUE)

      return(NULL)
    }

    # CSV or TXT
    if (ext %in% c("csv", "txt")) {

      # CSV: use comma separator directly
      if (ext == "csv") {
        loadDelimited(",")
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
                "Comma (,)"          = ",",
                "Tab (\\t)"          = "\t",
                "Semicolon (;)"      = ";",
                "Space ( )"          = " ",
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

    # Unsupported extension
    showNotification(buildUnsupportedUploadFileTypeMessage(), type = "error")
  })

  # -------------------------------------------------------------------
  # Handle separator confirmation for TXT
  # -------------------------------------------------------------------
  observeEvent(input$confirm_sep, {
    req(input$file)

    chosen = input$sep_input
    if (identical(chosen, "OTHER")) {
      chosen = input$sep_other
    }

    if (is.null(chosen) || chosen == "") {
      showNotification(buildMissingSeparatorMessage(), type = "error")
      return(NULL)
    }

    removeModal()
    loadDelimited(chosen)
  })

  # -------------------------------------------------------------------
  # Load package dataset when selected
  # -------------------------------------------------------------------
  observeEvent(input$package_dataset, {
    req(input$data_source == "package")

    pkg = input$data_package %||% ""
    dsName = input$package_dataset

    if (identical(pkg, "s20x")) {
      s20xOk = tryCatch(
        {
          ensureS20xInstalled()
          TRUE
        },
        error = function(e) {
          showNotification(conditionMessage(e), type = "error")
          FALSE
        }
      )

      if (!isTRUE(s20xOk)) {
        return(NULL)
      }
    }

    if (is.null(dsName) || dsName == "") {
      return(NULL)
    }

    env = new.env()

    ok = tryCatch({
      data(list = dsName, package = pkg, envir = env)
      TRUE
    }, error = function(e) {
      FALSE
    })

    if (!ok || !exists(dsName, envir = env, inherits = FALSE)) {
      showNotification(
        buildPackageDatasetLoadFailedMessage(dsName, pkg),
        type = "error"
      )
      return(NULL)
    }

    df = env[[dsName]]

    if (!is.data.frame(df)) {
      showNotification(buildSelectedObjectNotDataFrameMessage(), type = "error")
      return(NULL)
    }

    rv$data = df
    rv$allVars = names(df)
    rv$userDatasetContext = ""
    rv$researchQuestion = ""
    rv$loadedExample = NULL
    exampleLoadStatus(buildExampleReadyStatus())
    updateTextInput(session, "researchQuestion", value = "")
    resetModelPage(resetResponse = TRUE)

    # Switch to the Model tab after loading a package data set
    updateTabsetPanel(session, "main_tabs", selected = "Model")
  })

  observeEvent(input$loadExampleBtn, {
    exampleName = trimws(input$exampleName %||% "")

    if (!nzchar(exampleName)) {
      showNotification(buildChooseExampleFirstMessage(), type = "warning", duration = 6)
      return(NULL)
    }

    exampleInfo = tryCatch(
      loadExampleSpec(exampleName),
      error = function(e) {
        showNotification(conditionMessage(e), type = "error", duration = 8)
        NULL
      }
    )

    if (is.null(exampleInfo)) {
      return(NULL)
    }

    rv$data = exampleInfo$data
    rv$allVars = names(exampleInfo$data)
    rv$userDatasetContext = trimws(exampleInfo$dataContext %||% "")
    rv$researchQuestion = trimws(exampleInfo$researchQuestion %||% "")
    rv$loadedExample = utils::modifyList(
      exampleInfo,
      list(name = exampleName)
    )

    updateRadioButtons(session, "data_source", selected = "upload")
    applyLoadedExampleToInputs(exampleInfo)

    exampleLoadStatus(buildLoadedExampleStatus(exampleName))

    updateTabsetPanel(session, "main_tabs", selected = "Model")
  }, ignoreInit = TRUE)


  invisible(NULL)
}
