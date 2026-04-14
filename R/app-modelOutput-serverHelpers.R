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
#' @importFrom shiny helpText renderPrint renderTable renderUI req selectInput tableOutput checkboxInput
#' @importFrom shiny tagList tags
#' @importFrom stats anova setNames
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

  getCiDisplayChoices = function(ciData) {

    if (is.null(ciData) || is.null(ciData$table) || nrow(ciData$table) == 0) {
      return(character(0))
    }

    tbl = ciData$table

    if (!("displayScale" %in% names(tbl))) {
      return(character(0))
    }

    displayScales = unique(tbl$displayScale)
    displayScales = displayScales[!is.na(displayScales) & nzchar(displayScales)]

    if (length(displayScales) == 0) {
      return(character(0))
    }

    labelMap = c(
      fittedValue = "Fitted value",
      slope = "Slope",
      probability = "Probability",
      odds = "Odds",
      oddsMultiplier = "Odds multiplier",
      expectedValue = "Expected value",
      expectedValueMultiplier = "Expected value multiplier",
      coefficient = "Coefficient"
    )

    labels = ifelse(
      displayScales %in% names(labelMap),
      unname(labelMap[displayScales]),
      displayScales
    )

    stats::setNames(displayScales, labels)
  }

  getDefaultCiDisplayScale = function(ciData) {

    choices = getCiDisplayChoices(ciData)

    if (length(choices) == 0) {
      return("")
    }

    values = unname(choices)

    preferred = c(
      "probability",
      "expectedValue",
      "fittedValue",
      "odds",
      "oddsMultiplier",
      "expectedValueMultiplier",
      "slope",
      "coefficient"
    )

    matchValue = preferred[preferred %in% values][1]

    if (!is.na(matchValue) && nzchar(matchValue)) {
      return(matchValue)
    }

    values[1]
  }

  buildFittedSectionTitle = function(ciData, ciTable) {

    if (is.null(ciTable) || !is.data.frame(ciTable) || nrow(ciTable) == 0) {
      return("Fitted values")
    }

    fittedRows = ciTable[ciTable$rowRole %in% "fittedQuantity", , drop = FALSE]

    if (nrow(fittedRows) == 0) {
      return("Fitted values")
    }

    framework = fittedRows$modelFramework[which(!is.na(fittedRows$modelFramework))[1]] %||% ""
    quantityBase = switch(
      framework,
      binomialLogit = "Fitted probabilities",
      poissonLog = "Fitted expected values",
      lm = "Fitted values",
      "Fitted values"
    )

    settingsText = NULL
    detailIndex = which(vapply(
      ciData$details %||% list(),
      function(oneDetail) {
        identical(oneDetail$label %||% "", fittedRows$quantity[[1]] %||% "")
      },
      logical(1)
    ))

    if (length(detailIndex) >= 1) {
      settingsText = ciData$details[[detailIndex[1]]]$settings %||% ""
    }

    if (!nzchar(settingsText)) {
      return(quantityBase)
    }

    numericPieces = regmatches(
      settingsText,
      gregexpr("[A-Za-z][A-Za-z0-9._]* = -?[0-9]+(?:\\.[0-9]+)?", settingsText, perl = TRUE)
    )[[1]]

    if (length(numericPieces) == 0) {
      return(quantityBase)
    }

    numericText = paste(numericPieces, collapse = "; ")
    paste0(quantityBase, " (when ", numericText, ")")
  }

  buildCiDisplayNote = function(ciData, selectedScale, displayedTable = NULL) {

    fittedTitle = NULL
    if (!is.null(displayedTable) && is.data.frame(displayedTable) && nrow(displayedTable) > 0) {
      fittedTitle = buildFittedSectionTitle(ciData = ciData, ciTable = displayedTable)
    }

    scalePrefix = switch(
      selectedScale,
      probability = "Fitted values are shown on the probability scale. Predictor effects are shown as multiplicative changes in the odds.",
      odds = "Fitted values are shown on the odds scale. Predictor effects are shown as multiplicative changes in the odds.",
      oddsMultiplier = "Predictor effects are shown as multiplicative changes in the odds.",
      expectedValue = "Fitted values are shown on the expected-value scale. Predictor effects are shown as multiplicative changes in E(Y).",
      expectedValueMultiplier = "Predictor effects are shown as multiplicative changes in E(Y).",
      fittedValue = "Fitted values are shown on the response scale. Predictor effects are shown as one-unit changes on the response scale.",
      slope = "Predictor effects are shown as one-unit changes on the response scale.",
      coefficient = "Coefficient view.",
      NULL
    )

    pieces = c(
      if (!is.null(fittedTitle) && selectedScale %in% c("probability", "odds", "expectedValue", "fittedValue")) {
        paste0(fittedTitle, ".")
      },
      scalePrefix,
      ciData$note %||% NULL
    )
    pieces = pieces[!is.na(pieces) & nzchar(pieces)]

    paste(pieces, collapse = " ")
  }

  filterCiTableForDisplay = function(ciData, selectedScale, showComplement = FALSE) {

    if (is.null(ciData) || is.null(ciData$table) || nrow(ciData$table) == 0) {
      return(NULL)
    }

    tbl = ciData$table

    if (!("displayScale" %in% names(tbl))) {
      return(tbl)
    }

    if (is.null(selectedScale) || !nzchar(selectedScale)) {
      selectedScale = getDefaultCiDisplayScale(ciData)
    }

    fittedScales = c("probability", "odds", "expectedValue", "fittedValue")
    effectScales = c("oddsMultiplier", "expectedValueMultiplier", "slope")

    if (selectedScale %in% fittedScales) {
      keep = tbl$displayScale %in% c(selectedScale, effectScales)
    } else {
      keep = tbl$displayScale %in% selectedScale
    }

    out = tbl[keep, , drop = FALSE]

    if ("isComplement" %in% names(out) && !isTRUE(showComplement)) {
      keepComplement = is.na(out$isComplement) | !as.logical(out$isComplement)
      out = out[keepComplement, , drop = FALSE]
    }

    if ("sortKey" %in% names(out)) {
      out = out[order(out$sortKey), , drop = FALSE]
    }

    rownames(out) = NULL
    out
  }

  buildDisplayedCiTable = function(tbl, selectedScale, ciData = NULL) {

    if (is.null(tbl) || !is.data.frame(tbl) || nrow(tbl) == 0) {
      return(NULL)
    }

    makeDisplayBlock = function(oneTbl) {
      if (!is.data.frame(oneTbl) || nrow(oneTbl) == 0) {
        return(NULL)
      }

      out = oneTbl[, c("quantity", "estimate", "lower", "upper"), drop = FALSE]
      names(out) = c("Quantity", "Estimate", "Lower", "Upper")
      out
    }

    fittedRows = tbl[tbl$rowRole %in% "fittedQuantity", , drop = FALSE]
    effectRows = tbl[tbl$rowRole %in% "covariateEffect", , drop = FALSE]
    coefficientRows = tbl[tbl$rowRole %in% "coefficient", , drop = FALSE]

    pieces = list()

    if (nrow(fittedRows) > 0) {
      fittedTitle = if (!is.null(ciData)) {
        buildFittedSectionTitle(ciData = ciData, ciTable = fittedRows)
      } else {
        "Fitted values"
      }

      pieces[[length(pieces) + 1]] = data.frame(
        Quantity = fittedTitle,
        Estimate = NA_real_,
        Lower = NA_real_,
        Upper = NA_real_,
        stringsAsFactors = FALSE
      )
      pieces[[length(pieces) + 1]] = makeDisplayBlock(fittedRows)
    }

    if (nrow(effectRows) > 0) {
      if (length(pieces) > 0) {
        pieces[[length(pieces) + 1]] = data.frame(
          Quantity = "----------------",
          Estimate = NA_real_,
          Lower = NA_real_,
          Upper = NA_real_,
          stringsAsFactors = FALSE
        )
      }

      effectTitle = switch(
        selectedScale,
        probability = "Predictor effects (odds multipliers)",
        odds = "Predictor effects (odds multipliers)",
        oddsMultiplier = "Predictor effects (odds multipliers)",
        expectedValue = "Predictor effects (E(Y) multipliers)",
        expectedValueMultiplier = "Predictor effects (E(Y) multipliers)",
        fittedValue = "Predictor effects",
        slope = "Predictor effects",
        "Predictor effects"
      )

      pieces[[length(pieces) + 1]] = data.frame(
        Quantity = effectTitle,
        Estimate = NA_real_,
        Lower = NA_real_,
        Upper = NA_real_,
        stringsAsFactors = FALSE
      )
      pieces[[length(pieces) + 1]] = makeDisplayBlock(effectRows)
    }

    if (nrow(coefficientRows) > 0) {
      pieces[[length(pieces) + 1]] = makeDisplayBlock(coefficientRows)
    }

    display = do.call(rbind, pieces)
    rownames(display) = NULL
    display
  }

  buildModelConfidenceIntervalRowChoices = function(ciData, displayedTable = NULL) {

    tbl = displayedTable %||% ciData$table

    if (is.null(tbl) || nrow(tbl) == 0) {
      return(stats::setNames("", ""))
    }

    labels = tbl$quantity %||% character(0)
    labels = labels[!is.na(labels) & nzchar(labels)]

    if (length(labels) == 0) {
      return(stats::setNames("", ""))
    }

    stats::setNames(labels, labels)
  }

  findModelConfidenceIntervalDetail = function(ciData, selectedLabel) {

    if (
      is.null(ciData) ||
      is.null(ciData$details) ||
      length(ciData$details) == 0 ||
      is.null(selectedLabel) ||
      !nzchar(selectedLabel)
    ) {
      return(NULL)
    }

    detailIndex = which(vapply(
      ciData$details,
      function(x) {
        identical(x$label %||% "", selectedLabel)
      },
      logical(1)
    ))

    if (length(detailIndex) != 1) {
      return(NULL)
    }

    ciData$details[[detailIndex]]
  }

  renderModelConfidenceIntervalDetailUi = function(detail) {

    if (is.null(detail)) {
      return(helpText("Choose a row if you want to see how that interval was constructed."))
    }

    tagList(
      tags$p(tags$strong(detail$label %||% "")),
      tags$p(detail$settings %||% ""),
      tags$p(tags$strong("Built from: "), detail$builtFrom %||% ""),
      tags$p(tags$strong("Variance formula: "), detail$varianceFormula %||% ""),
      tags$p(tags$strong("Scale note: "), detail$scaleNote %||% "")
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

    if (is.null(ciData)) {
      return(NULL)
    }

    choices = getCiDisplayChoices(ciData)

    if (length(choices) == 0) {
      return(NULL)
    }

    tagList(
      selectInput(
        inputId = "modelConfintDisplayScale",
        label = "Display scale",
        choices = choices,
        selected = getDefaultCiDisplayScale(ciData)
      ),
      {
        hasComplement = !is.null(ciData$table) &&
          "isComplement" %in% names(ciData$table) &&
          any(ciData$table$isComplement %in% TRUE, na.rm = TRUE)

        if (isTRUE(hasComplement)) {
          checkboxInput(
            inputId = "modelConfintShowComplement",
            label = "Show complementary outcome rows",
            value = FALSE
          )
        }
      }
    )
  })

  output$modelConfintNoteUi = renderUI({

    ciData = getCiData()

    if (is.null(ciData)) {
      return(helpText("Fit a model to see confidence intervals."))
    }

    selectedScale = input$modelConfintDisplayScale %||% getDefaultCiDisplayScale(ciData)
    displayed = filterCiTableForDisplay(
      ciData = ciData,
      selectedScale = selectedScale,
      showComplement = isTRUE(input$modelConfintShowComplement %||% FALSE)
    )
    noteText = buildCiDisplayNote(
      ciData = ciData,
      selectedScale = selectedScale,
      displayedTable = displayed
    )

    helpText(noteText)
  })

  output$modelConfintTableUi = renderUI({

    ciData = getCiData()

    if (is.null(ciData)) {
      return(NULL)
    }

    tableOutput("modelConfintTable")
  })

  output$modelConfintTable = renderTable({

    ciData = getCiData()

    if (is.null(ciData)) {
      return(NULL)
    }

    selectedScale = input$modelConfintDisplayScale %||% getDefaultCiDisplayScale(ciData)
    displayed = filterCiTableForDisplay(
      ciData = ciData,
      selectedScale = selectedScale,
      showComplement = isTRUE(input$modelConfintShowComplement %||% FALSE)
    )

    buildDisplayedCiTable(
      displayed,
      selectedScale = selectedScale,
      ciData = ciData
    )

  }, striped = TRUE, bordered = TRUE, spacing = "s")

  output$modelConfintSelectorUi = renderUI({

    ciData = getCiData()

    if (is.null(ciData)) {
      return(NULL)
    }

    selectedScale = input$modelConfintDisplayScale %||% getDefaultCiDisplayScale(ciData)
    displayed = filterCiTableForDisplay(
      ciData = ciData,
      selectedScale = selectedScale,
      showComplement = isTRUE(input$modelConfintShowComplement %||% FALSE)
    )

    selectInput(
      inputId = "modelConfintSelectedRow",
      label = "Choose a row",
      choices = buildModelConfidenceIntervalRowChoices(ciData, displayedTable = displayed),
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

    renderModelConfidenceIntervalDetailUi(detail)
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
