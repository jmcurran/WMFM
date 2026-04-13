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
#' @importFrom shiny checkboxInput helpText renderPrint renderTable renderUI req selectInput
#' @importFrom shiny tagList tags tableOutput uiOutput
#' @importFrom stats anova
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

  getDisplayScaleChoices = function(ciTable) {

    if (is.null(ciTable) || !is.data.frame(ciTable) || !("displayScale" %in% names(ciTable))) {
      return(character(0))
    }

    scaleOrder = c(
      "fittedValue",
      "slope",
      "probability",
      "odds",
      "oddsMultiplier",
      "expectedValue",
      "expectedValueMultiplier"
    )

    present = unique(as.character(ciTable$displayScale))
    present = present[!is.na(present) & nzchar(present)]
    present = intersect(scaleOrder, present)

    labelMap = c(
      fittedValue = "Fitted value",
      slope = "Slope",
      probability = "Probability",
      odds = "Odds",
      oddsMultiplier = "Odds multiplier",
      expectedValue = "Expected value",
      expectedValueMultiplier = "Expected value multiplier"
    )

    stats::setNames(present, labelMap[present])
  }

  getSelectedDisplayScale = function(ciData) {

    if (is.null(ciData) || is.null(ciData$table)) {
      return(NULL)
    }

    choices = getDisplayScaleChoices(ciData$table)
    choiceValues = unname(choices)

    if (length(choiceValues) == 0) {
      return(NULL)
    }

    requested = input$modelConfintDisplayScale %||% ""

    if (nzchar(requested) && requested %in% choiceValues) {
      return(requested)
    }

    choiceValues[1]
  }

  isFittedQuantityScale = function(displayScale) {
    displayScale %in% c("fittedValue", "probability", "odds", "expectedValue")
  }

  getDisplayedCiTable = function(ciData) {

    if (is.null(ciData) || is.null(ciData$table) || !is.data.frame(ciData$table)) {
      return(NULL)
    }

    tableDf = ciData$table

    if (!("displayScale" %in% names(tableDf))) {
      return(tableDf)
    }

    selectedScale = getSelectedDisplayScale(ciData)

    if (is.null(selectedScale) || !nzchar(selectedScale)) {
      return(tableDf)
    }

    isComplement = rep(FALSE, nrow(tableDf))
    if ("isComplement" %in% names(tableDf)) {
      isComplement = isTRUE(tableDf$isComplement) | identical(tableDf$isComplement, TRUE)
      isComplement = as.logical(tableDf$isComplement)
      isComplement[is.na(isComplement)] = FALSE
    }

    rowRole = if ("rowRole" %in% names(tableDf)) {
      as.character(tableDf$rowRole)
    } else {
      rep(NA_character_, nrow(tableDf))
    }

    showComplement = isTRUE(input$modelConfintShowComplement %||% FALSE)

    if (identical(selectedScale, "probability") || identical(selectedScale, "odds")) {
      if (!showComplement) {
        fittedKeep = tableDf$displayScale %in% selectedScale & !isComplement
      } else {
        fittedKeep = tableDf$displayScale %in% selectedScale
      }
    } else {
      fittedKeep = tableDf$displayScale %in% selectedScale
    }

    effectScaleMap = c(
      fittedValue = "slope",
      probability = "oddsMultiplier",
      odds = "oddsMultiplier",
      expectedValue = "expectedValueMultiplier"
    )

    keep = fittedKeep

    if (isFittedQuantityScale(selectedScale) && !all(is.na(rowRole))) {
      mappedEffectScale = effectScaleMap[[selectedScale]]
      effectKeep = rowRole %in% "covariateEffect"

      if (!is.null(mappedEffectScale) && nzchar(mappedEffectScale)) {
        effectKeep = effectKeep & tableDf$displayScale %in% mappedEffectScale
      }

      keep = fittedKeep | effectKeep
    }

    out = tableDf[keep, , drop = FALSE]

    if (nrow(out) == 0) {
      return(out)
    }

    if ("sortKey" %in% names(out)) {
      out = out[order(out$sortKey, out$quantity), , drop = FALSE]
    }

    rownames(out) = NULL
    out
  }

  buildCiDisplayNote = function(ciData) {

    if (is.null(ciData)) {
      return(NULL)
    }

    selectedScale = getSelectedDisplayScale(ciData)

    prefix = switch(
      selectedScale,
      fittedValue = "Fitted value view.",
      slope = "Slope view.",
      probability = "Probability view.",
      odds = "Odds view.",
      oddsMultiplier = "Odds multiplier view.",
      expectedValue = "Expected value view.",
      expectedValueMultiplier = "Expected value multiplier view.",
      NULL
    )

    suffix = ciData$note %||% NULL

    paste(c(prefix, suffix), collapse = " ")
  }

  getPrimarySecondaryColumns = function(displayedTable) {

    secondaryPresent = (
      !is.null(displayedTable) &&
        is.data.frame(displayedTable) &&
        nrow(displayedTable) > 0 &&
        all(c("secondaryScale", "secondaryEstimate", "secondaryLower", "secondaryUpper") %in% names(displayedTable))
    )

    if (!secondaryPresent) {
      return(list(
        primaryLabels = c("Estimate", "Lower", "Upper"),
        secondaryLabels = NULL
      ))
    }

    secondaryScaleValues = unique(as.character(displayedTable$secondaryScale))
    secondaryScaleValues = secondaryScaleValues[!is.na(secondaryScaleValues) & nzchar(secondaryScaleValues)]

    if (length(secondaryScaleValues) != 1) {
      return(list(
        primaryLabels = c("Estimate", "Lower", "Upper"),
        secondaryLabels = NULL
      ))
    }

    secondaryLabelBase = switch(
      secondaryScaleValues[[1]],
      odds = "Odds",
      oddsMultiplier = "Odds multiplier",
      logOdds = "Log-odds",
      logMean = "Log mean",
      expectedValue = "Expected value",
      expectedValueMultiplier = "Expected value multiplier",
      coefficient = "Coefficient",
      NULL
    )

    if (is.null(secondaryLabelBase)) {
      return(list(
        primaryLabels = c("Estimate", "Lower", "Upper"),
        secondaryLabels = NULL
      ))
    }

    list(
      primaryLabels = c("Estimate", "Lower", "Upper"),
      secondaryLabels = c(
        paste0(secondaryLabelBase, " estimate"),
        paste0(secondaryLabelBase, " lower"),
        paste0(secondaryLabelBase, " upper")
      )
    )
  }

  formatCiNumber = function(x) {

    if (length(x) == 0 || is.na(x)) {
      return("")
    }

    if (is.infinite(x)) {
      return(if (x > 0) "Inf" else "-Inf")
    }

    sprintf("%.3f", as.numeric(x))
  }

  buildCiHtmlTable = function(displayedTable) {

    if (is.null(displayedTable) || !is.data.frame(displayedTable) || nrow(displayedTable) == 0) {
      return(helpText("No confidence-interval rows are available for the selected display scale."))
    }

    labels = getPrimarySecondaryColumns(displayedTable)

    secondaryPresent = !is.null(labels$secondaryLabels)

    effectStart = NA_integer_
    if ("rowRole" %in% names(displayedTable)) {
      effectIndex = which(as.character(displayedTable$rowRole) %in% "covariateEffect")
      if (length(effectIndex) > 0) {
        effectStart = effectIndex[1]
      }
    }

    headerCells = c(
      tags$th("Quantity", style = "text-align: left;"),
      tags$th(labels$primaryLabels[1]),
      tags$th(labels$primaryLabels[2]),
      tags$th(labels$primaryLabels[3])
    )

    if (secondaryPresent) {
      headerCells = c(
        headerCells,
        tags$th(labels$secondaryLabels[1]),
        tags$th(labels$secondaryLabels[2]),
        tags$th(labels$secondaryLabels[3])
      )
    }

    bodyRows = lapply(seq_len(nrow(displayedTable)), function(i) {
      row = displayedTable[i, , drop = FALSE]

      rowStyle = if (!is.na(effectStart) && identical(i, effectStart)) {
        "border-top: 1px solid #000;"
      } else {
        NULL
      }

      cells = c(
        tags$td(as.character(row$quantity[[1]]), style = "text-align: left;"),
        tags$td(formatCiNumber(row$estimate[[1]])),
        tags$td(formatCiNumber(row$lower[[1]])),
        tags$td(formatCiNumber(row$upper[[1]]))
      )

      if (secondaryPresent) {
        cells = c(
          cells,
          tags$td(formatCiNumber(row$secondaryEstimate[[1]])),
          tags$td(formatCiNumber(row$secondaryLower[[1]])),
          tags$td(formatCiNumber(row$secondaryUpper[[1]]))
        )
      }

      tags$tr(style = rowStyle, cells)
    })

    tags$table(
      class = "table table-striped table-bordered table-condensed",
      tags$thead(tags$tr(headerCells)),
      tags$tbody(bodyRows)
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

  output$modelConfintControlsUi = renderUI({

    ciData = getCiData()

    if (is.null(ciData) || is.null(ciData$table)) {
      return(NULL)
    }

    choices = getDisplayScaleChoices(ciData$table)

    if (length(choices) == 0) {
      return(NULL)
    }

    selectedScale = getSelectedDisplayScale(ciData)
    isLogisticScale = selectedScale %in% c("probability", "odds")
    hasComplement = any(as.logical(ciData$table$isComplement %||% FALSE))

    tagList(
      selectInput(
        inputId = "modelConfintDisplayScale",
        label = "Display scale",
        choices = choices,
        selected = selectedScale
      ),
      if (isLogisticScale && hasComplement) {
        checkboxInput(
          inputId = "modelConfintShowComplement",
          label = "Also show the complementary outcome",
          value = FALSE
        )
      }
    )
  })

  output$modelConfintNoteUi = renderUI({

    ciData = getCiData()

    if (is.null(ciData)) {
      return(helpText("Fit a model to see confidence intervals."))
    }

    helpText(buildCiDisplayNote(ciData))
  })

  output$modelConfintTableUi = renderUI({

    ciData = getCiData()
    displayedTable = getDisplayedCiTable(ciData)

    buildCiHtmlTable(displayedTable)
  })

  output$modelConfintSelectorUi = renderUI({

    ciData = getCiData()
    displayedTable = getDisplayedCiTable(ciData)

    if (is.null(ciData) || is.null(displayedTable) || nrow(displayedTable) == 0) {
      return(NULL)
    }

    displayedLabels = as.character(displayedTable$quantity)
    allChoices = buildModelConfidenceIntervalRowChoices(ciData)
    matchingChoices = allChoices[names(allChoices) %in% displayedLabels]

    if (length(matchingChoices) == 0) {
      return(NULL)
    }

    selectInput(
      inputId = "modelConfintSelectedRow",
      label = "Choose a row",
      choices = matchingChoices,
      selected = matchingChoices[[1]]
    )
  })

  output$modelConfintSelectedRowUi = renderUI({

    ciData = getCiData()
    displayedTable = getDisplayedCiTable(ciData)

    if (is.null(ciData) || is.null(displayedTable) || nrow(displayedTable) == 0) {
      return(NULL)
    }

    selectedLabel = input$modelConfintSelectedRow %||% ""

    if (!nzchar(selectedLabel) || !(selectedLabel %in% displayedTable$quantity)) {
      selectedLabel = displayedTable$quantity[[1]]
    }

    detail = findModelConfidenceIntervalDetail(
      ciData = ciData,
      selectedLabel = selectedLabel
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

    helpText(ciData$teachingNote)
  })

  output$modelConfintVcovUi = renderUI({

    ciData = getCiData()

    if (is.null(ciData) || is.null(ciData$vcovTable)) {
      return(NULL)
    }

    tableOutput("modelConfintVcovTable")
  })

  output$modelConfintVcovTable = renderTable({

    ciData = getCiData()

    if (is.null(ciData) || is.null(ciData$vcovTable)) {
      return(NULL)
    }

    as.data.frame(ciData$vcovTable)

  }, striped = TRUE, bordered = TRUE, spacing = "s", rownames = TRUE)
}
