#' Register startup data choice observers for the app server
#'
#' Keeps the startup package scan, example-list loading, and package-dataset
#' choice observers out of the top-level app server so that `appServer()` can
#' act more like an orchestration layer.
#'
#' @param input Shiny input object.
#' @param output Shiny output object.
#' @param session Shiny session object.
#'
#' @return A list containing startup-related reactive values needed by later
#'   server sections.
#'
#' @keywords internal
#'
#' @importFrom shiny reactiveVal renderText observe observeEvent isolate req
#' @importFrom shiny updateSelectInput showNotification removeNotification
registerStartupDataChoiceObservers = function(input, output, session) {
  packageChoices = reactiveVal(character(0))
  packageScanStatus = reactiveVal(NULL)
  packageDatasetStatus = reactiveVal(buildPackageDatasetChoiceStatus())
  exampleChoices = reactiveVal(buildLoadingExampleChoice())
  developerModeUnlocked = reactiveVal(FALSE)
  exampleLoadStatus = reactiveVal(buildInitialExampleLoadStatus())

  initialPackageChoices = tryCatch(
    {
      ensureS20xInstalled()
      "s20x"
    },
    error = function(e) {
      character(0)
    }
  )

  packageChoices(initialPackageChoices)

  packageScanStatus(buildInitialPackageScanStatus(initialPackageChoices))

  output$packageScanStatus = renderText({
    packageScanStatus() %||% ""
  })

  output$packageDatasetStatus = renderText({
    packageDatasetStatus() %||% ""
  })

  output$exampleLoadStatus = renderText({
    exampleLoadStatus() %||% ""
  })

  observe({
    choices = exampleChoices()
    selected = isolate(input$exampleName %||% "")

    if (!nzchar(selected) || !(selected %in% choices)) {
      selected = if (length(choices) > 0) {
        choices[1]
      } else {
        ""
      }
    }

    updateSelectInput(
      session,
      "exampleName",
      choices = choices,
      selected = selected
    )
  })

  observe({
    choices = packageChoices()
    selected = isolate(input$data_package %||% "")

    if (!nzchar(selected) || !(selected %in% choices)) {
      if ("s20x" %in% choices) {
        selected = "s20x"
      } else if (length(choices) > 0) {
        selected = choices[1]
      } else {
        selected = ""
      }
    }

    updateSelectInput(
      session,
      "data_package",
      choices = choices,
      selected = selected
    )

    if (length(choices) == 0) {
      updateSelectInput(
        session,
        "package_dataset",
        choices = character(0),
        selected = character(0)
      )
    }
  })

  session$onFlushed(function() {
    startupNotificationId = "wmfm-startup-data-choices"

    packageScanStatus("Preparing the built-in examples.")
    packageDatasetStatus(buildPackageDatasetPendingScanStatus())
    exampleLoadStatus("Loading the built-in examples.")
    exampleChoices(buildLoadingExampleChoice())

    showNotification(
      buildStartupDataChoicesMessage(),
      id = startupNotificationId,
      type = "message",
      duration = NULL
    )

    session$onFlushed(function() {
      exampleChoices(listWMFMExamples(includeTestExamples = isTRUE(isolate(developerModeUnlocked()))))
      exampleLoadStatus(buildExampleReadyStatus())
      packageScanStatus("Checking installed packages for datasets.")

      session$onFlushed(function() {
        packageNames = getInstalledPackagesWithData()

        if (length(packageNames) == 0) {
          packageChoices(character(0))
          packageScanStatus("No installed packages with datasets were found.")
          packageDatasetStatus(buildNoPackageDatasetsStatus())
          removeNotification(startupNotificationId)
          return(NULL)
        }

        packageScanStatus(buildPackageListUpdatingStatus())
        packageChoices(packageNames)

        packageScanStatus(
          buildPackageScanCompleteStatus(
            packageNames = packageNames,
            initialPackageChoices = initialPackageChoices
          )
        )

        removeNotification(startupNotificationId)
      }, once = TRUE)
    }, once = TRUE)
  }, once = TRUE)

  observeEvent(developerModeUnlocked(), {
    exampleChoices(listWMFMExamples(includeTestExamples = isTRUE(developerModeUnlocked())))
  }, ignoreInit = TRUE)

  observeEvent(input$data_package, {

    req(input$data_source == "package")

    pkg = input$data_package %||% ""

    if (!nzchar(pkg)) {
      packageDatasetStatus(buildPackageDatasetChoiceStatus())
      updateSelectInput(
        session,
        "package_dataset",
        choices = character(0),
        selected = character(0)
      )
      return(NULL)
    }

    packageDatasetStatus(buildPackageDatasetCheckingStatus(pkg))
    updateSelectInput(
      session,
      "package_dataset",
      choices = c("Loading datasets..." = ""),
      selected = ""
    )

    session$onFlushed(function() {
      if (!identical(isolate(input$data_package %||% ""), pkg)) {
        return(NULL)
      }

      datasetNotificationId = "wmfm-package-dataset-list"
      showNotification(
        buildPackageDatasetFindingMessage(pkg),
        id = datasetNotificationId,
        type = "message",
        duration = NULL
      )
      on.exit(removeNotification(datasetNotificationId), add = TRUE)

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
          packageDatasetStatus(buildS20xPackageMissingStatus())
          updateSelectInput(
            session,
            "package_dataset",
            choices = setNames("", buildS20xPackageMissingChoiceLabel()),
            selected = ""
          )
          return(NULL)
        }
      }

      dsNames = getPackageDatasetNames(pkg)

      if (length(dsNames) == 0) {
        packageDatasetStatus(buildPackageDatasetEmptyStatus(pkg))
        updateSelectInput(
          session,
          "package_dataset",
          choices = setNames("", buildPackageDatasetEmptyChoiceLabel()),
          selected = ""
        )
        return(NULL)
      }

      packageDatasetStatus(buildPackageDatasetFoundStatus(pkg, dsNames))
      choices = c("Choose a data set..." = "", dsNames)

      updateSelectInput(
        session,
        "package_dataset",
        choices = choices,
        selected = ""
      )
    }, once = TRUE)
  }, ignoreInit = FALSE)

  list(
    developerModeUnlocked = developerModeUnlocked,
    exampleLoadStatus = exampleLoadStatus
  )
}
