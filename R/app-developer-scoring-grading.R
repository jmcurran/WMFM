#' Build developer scoring summary table
#'
#' Converts a scored `wmfmGrade` object into a compact one-row table for the
#' developer scoring and grading panel.
#'
#' @param gradeObj A `wmfmGrade` object.
#' @param method Character scalar naming the scoring method to inspect.
#'
#' @return A data frame suitable for `renderTable()`.
#'
#' @keywords internal
buildDeveloperScoringSummaryTable = function(gradeObj, method = "deterministic") {
  if (!inherits(gradeObj, "wmfmGrade")) {
    return(data.frame())
  }

  methodScore = gradeObj$scores$byMethod[[method]]

  if (is.null(methodScore)) {
    return(data.frame())
  }

  data.frame(
    method = method,
    overallScore = round(suppressWarnings(as.numeric(methodScore$overallScore)), 2),
    mark = round(suppressWarnings(as.numeric(methodScore$mark)), 2),
    scoreScale = round(suppressWarnings(as.numeric(gradeObj$scoreScale)), 2),
    scored = isTRUE(gradeObj$meta$scored),
    stringsAsFactors = FALSE
  )
}

#' Build developer scoring metric table
#'
#' Extracts the metric summary from a scored `wmfmGrade` object.
#'
#' @param gradeObj A `wmfmGrade` object.
#' @param method Character scalar naming the scoring method to inspect.
#'
#' @return A data frame suitable for `renderTable()`.
#'
#' @keywords internal
buildDeveloperScoringMetricTable = function(gradeObj, method = "deterministic") {
  if (!inherits(gradeObj, "wmfmGrade")) {
    return(data.frame())
  }

  methodScore = gradeObj$scores$byMethod[[method]]

  if (is.null(methodScore) || is.null(methodScore$metricSummary)) {
    return(data.frame())
  }

  metricTable = methodScore$metricSummary

  keepCols = intersect(
    c("label", "studentValue", "maxValue", "marksLost", "reason"),
    names(metricTable)
  )

  metricTable[, keepCols, drop = FALSE]
}

#' Build developer scoring loss table
#'
#' Extracts a named feedback table from a scored `wmfmGrade` object.
#'
#' @param gradeObj A `wmfmGrade` object.
#' @param tableName Character scalar naming a feedback table.
#' @param method Character scalar naming the scoring method to inspect.
#'
#' @return A data frame suitable for `renderTable()`.
#'
#' @keywords internal
buildDeveloperScoringLossTable = function(
    gradeObj,
    tableName,
    method = "deterministic"
) {
  if (!inherits(gradeObj, "wmfmGrade")) {
    return(data.frame())
  }

  methodFeedback = gradeObj$feedback$byMethod[[method]]

  if (is.null(methodFeedback) || is.null(methodFeedback[[tableName]])) {
    return(data.frame())
  }

  out = methodFeedback[[tableName]]

  if (!is.data.frame(out)) {
    return(data.frame())
  }

  out
}

#' Build developer scoring object text
#'
#' Captures the printed summary of a scored `wmfmGrade` object for developer
#' inspection.
#'
#' @param gradeObj A `wmfmGrade` object.
#'
#' @return A character scalar.
#'
#' @keywords internal
buildDeveloperScoringObjectText = function(gradeObj) {
  if (!inherits(gradeObj, "wmfmGrade")) {
    return("No grade object is available.")
  }

  paste(utils::capture.output(summary(gradeObj)), collapse = "\n")
}

#' Register developer scoring and grading observers
#'
#' Adds a developer-mode diagnostics panel for deterministic scoring of the
#' currently generated model explanation. The command-line scoring functions
#' remain canonical; the UI only displays their output.
#'
#' @param input Shiny input object.
#' @param output Shiny output object.
#' @param session Shiny session object.
#' @param rv App reactive values object.
#' @param modelFit Reactive value containing the fitted model.
#' @param developerModeUnlocked Reactive value indicating developer mode status.
#'
#' @return A list containing the current developer grade reactive value.
#'
#' @keywords internal
registerDeveloperScoringGradingObservers = function(
    input,
    output,
    session,
    rv,
    modelFit,
    developerModeUnlocked
) {
  developerGrade = shiny::reactiveVal(NULL)
  developerScoreStatus = shiny::reactiveVal(
    "Score the current explanation to inspect the deterministic grading object."
  )

  output$developerScoringGradingUi = shiny::renderUI({
    if (!isTRUE(developerModeUnlocked())) {
      return(shiny::tagList(
        shiny::h4("Scoring and grading"),
        shiny::helpText(
          "Unlock developer mode in Settings to inspect scoring and grading diagnostics."
        )
      ))
    }

    shiny::tagList(
      shiny::h4("Scoring and grading"),
      shiny::helpText(
        "This developer panel scores the current generated explanation using the same deterministic grading path used from the command line. It does not define or tune grading thresholds."
      ),
      shiny::actionButton(
        inputId = "scoreCurrentExplanationBtn",
        label = "Score current explanation",
        class = "btn btn-primary btn-sm"
      ),
      shiny::tags$br(),
      shiny::tags$br(),
      shiny::textOutput("developerScoreStatus"),
      shiny::tags$hr(class = "hr-tight"),
      shiny::h5("Summary"),
      shiny::tableOutput("developerScoreSummaryTable"),
      shiny::h5("Metric summary"),
      shiny::tableOutput("developerScoreMetricTable"),
      shiny::h5("Where marks were lost"),
      shiny::tableOutput("developerScoreMarksLostTable"),
      shiny::h5("Strengths"),
      shiny::tableOutput("developerScoreStrengthsTable"),
      shiny::h5("Grade object summary"),
      shiny::verbatimTextOutput("developerScoreObjectText")
    )
  })

  output$developerScoreStatus = shiny::renderText({
    developerScoreStatus() %||% ""
  })

  shiny::observeEvent(input$scoreCurrentExplanationBtn, {
    if (!isTRUE(developerModeUnlocked())) {
      developerScoreStatus("Developer mode is locked.")
      developerGrade(NULL)
      return(NULL)
    }

    m = modelFit()
    explanationText = rv$modelExplanation

    if (is.null(m)) {
      developerScoreStatus("Fit a model before scoring an explanation.")
      developerGrade(NULL)
      return(NULL)
    }

    if (is.null(explanationText) || !nzchar(trimws(as.character(explanationText)))) {
      developerScoreStatus("Generate a model explanation before scoring it.")
      developerGrade(NULL)
      return(NULL)
    }

    scoredGrade = tryCatch(
      grade(
        x = m,
        explanation = explanationText,
        method = "deterministic",
        autoScore = TRUE
      ),
      error = function(e) {
        developerScoreStatus(
          paste("Could not score the current explanation:", conditionMessage(e))
        )
        NULL
      }
    )

    developerGrade(scoredGrade)

    if (!is.null(scoredGrade)) {
      developerScoreStatus("Current explanation scored with the deterministic grading path.")
    }
  })

  output$developerScoreSummaryTable = shiny::renderTable({
    buildDeveloperScoringSummaryTable(developerGrade())
  })

  output$developerScoreMetricTable = shiny::renderTable({
    buildDeveloperScoringMetricTable(developerGrade())
  })

  output$developerScoreMarksLostTable = shiny::renderTable({
    buildDeveloperScoringLossTable(developerGrade(), "whereMarksLost")
  })

  output$developerScoreStrengthsTable = shiny::renderTable({
    buildDeveloperScoringLossTable(developerGrade(), "strengths")
  })

  output$developerScoreObjectText = shiny::renderText({
    buildDeveloperScoringObjectText(developerGrade())
  })

  list(
    developerGrade = developerGrade
  )
}
