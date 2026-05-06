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

#' Format developer scoring duration
#'
#' @param seconds Numeric scalar giving a duration in seconds.
#'
#' @return A compact character scalar.
#'
#' @keywords internal
formatDeveloperScoringDuration = function(seconds) {
  seconds = suppressWarnings(as.numeric(seconds)[1])

  if (is.na(seconds) || !is.finite(seconds) || seconds < 0) {
    return("not available")
  }

  if (seconds < 60) {
    return(sprintf("%.1f seconds", seconds))
  }

  minutes = floor(seconds / 60)
  remainingSeconds = round(seconds - minutes * 60)

  sprintf("%d min %02d sec", minutes, remainingSeconds)
}

#' Build developer scoring repeated-run progress text
#'
#' @param completed Number of completed runs.
#' @param total Total number of requested runs.
#' @param elapsedSeconds Elapsed runtime in seconds.
#'
#' @return A character scalar.
#'
#' @keywords internal
buildDeveloperScoringProgressText = function(
    completed,
    total,
    elapsedSeconds
) {
  completed = suppressWarnings(as.integer(completed)[1])
  total = suppressWarnings(as.integer(total)[1])
  elapsedSeconds = suppressWarnings(as.numeric(elapsedSeconds)[1])

  if (is.na(completed) || completed < 0) {
    completed = 0L
  }

  if (is.na(total) || total < 1) {
    total = 1L
  }

  if (is.na(elapsedSeconds) || elapsedSeconds < 0) {
    elapsedSeconds = 0
  }

  meanSeconds = if (completed > 0) {
    elapsedSeconds / completed
  } else {
    NA_real_
  }

  remainingRuns = max(total - completed, 0L)
  estimatedRemaining = if (completed > 0) {
    meanSeconds * remainingRuns
  } else {
    NA_real_
  }

  paste0(
    completed,
    " of ",
    total,
    " runs complete. Elapsed: ",
    formatDeveloperScoringDuration(elapsedSeconds),
    ". Estimated remaining: ",
    formatDeveloperScoringDuration(estimatedRemaining),
    "."
  )
}

#' Build developer repeated scoring summary table
#'
#' @param repeatedResult A developer repeated scoring result object.
#'
#' @return A one-row data frame suitable for `renderTable()`.
#'
#' @keywords internal
buildDeveloperRepeatedScoringSummaryTable = function(repeatedResult) {
  if (is.null(repeatedResult) || !is.list(repeatedResult)) {
    return(data.frame())
  }

  runTable = repeatedResult$runTable

  if (!is.data.frame(runTable) || nrow(runTable) < 1) {
    return(data.frame())
  }

  successful = runTable[runTable$scored %in% TRUE, , drop = FALSE]
  marks = suppressWarnings(as.numeric(successful$mark))
  marks = marks[!is.na(marks)]

  data.frame(
    requestedRuns = repeatedResult$totalRuns %||% nrow(runTable),
    completedRuns = nrow(runTable),
    scoredRuns = nrow(successful),
    meanMark = if (length(marks) > 0) round(mean(marks), 2) else NA_real_,
    minMark = if (length(marks) > 0) round(min(marks), 2) else NA_real_,
    maxMark = if (length(marks) > 0) round(max(marks), 2) else NA_real_,
    elapsed = formatDeveloperScoringDuration(repeatedResult$elapsedSeconds %||% NA_real_),
    stringsAsFactors = FALSE
  )
}

#' Build developer repeated scoring run table
#'
#' @param repeatedResult A developer repeated scoring result object.
#'
#' @return A data frame suitable for `renderTable()`.
#'
#' @keywords internal
buildDeveloperRepeatedScoringRunTable = function(repeatedResult) {
  if (is.null(repeatedResult) || !is.list(repeatedResult)) {
    return(data.frame())
  }

  runTable = repeatedResult$runTable

  if (!is.data.frame(runTable)) {
    return(data.frame())
  }

  runTable
}

extractDeveloperGradeMark = function(gradeObj, method = "deterministic") {
  if (!inherits(gradeObj, "wmfmGrade")) {
    return(NA_real_)
  }

  methodScore = gradeObj$scores$byMethod[[method]]

  if (is.null(methodScore)) {
    return(NA_real_)
  }

  suppressWarnings(as.numeric(methodScore$mark)[1])
}

extractDeveloperGradeOverallScore = function(gradeObj, method = "deterministic") {
  if (!inherits(gradeObj, "wmfmGrade")) {
    return(NA_real_)
  }

  methodScore = gradeObj$scores$byMethod[[method]]

  if (is.null(methodScore)) {
    return(NA_real_)
  }

  suppressWarnings(as.numeric(methodScore$overallScore)[1])
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
  developerRepeatedResult = shiny::reactiveVal(NULL)
  developerScoreStatus = shiny::reactiveVal(
    "Score the current explanation to inspect the deterministic grading object."
  )
  developerRepeatedStatus = shiny::reactiveVal(
    "Repeated scoring has not been run in this session. Start with 3 runs for a new example."
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
      shiny::verbatimTextOutput("developerScoreObjectText"),
      shiny::tags$hr(class = "hr-tight"),
      shiny::h4("Repeated explanation scoring"),
      shiny::helpText(
        "Repeated runs make fresh explanation requests and can take several minutes. The progress dialog estimates remaining time from completed runs. Start with 3 runs when testing a new example."
      ),
      shiny::numericInput(
        inputId = "developerScoringRunCount",
        label = "Number of repeated runs",
        value = 3,
        min = 1,
        max = 50,
        step = 1
      ),
      shiny::actionButton(
        inputId = "runRepeatedScoringBtn",
        label = "Run repeated scoring",
        class = "btn btn-warning btn-sm"
      ),
      shiny::tags$br(),
      shiny::tags$br(),
      shiny::textOutput("developerRepeatedScoreStatus"),
      shiny::h5("Repeated-run summary"),
      shiny::tableOutput("developerRepeatedScoreSummaryTable"),
      shiny::h5("Repeated-run detail"),
      shiny::tableOutput("developerRepeatedScoreRunTable")
    )
  })

  output$developerScoreStatus = shiny::renderText({
    developerScoreStatus() %||% ""
  })

  output$developerRepeatedScoreStatus = shiny::renderText({
    developerRepeatedStatus() %||% ""
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

  shiny::observeEvent(input$runRepeatedScoringBtn, {
    if (!isTRUE(developerModeUnlocked())) {
      developerRepeatedStatus("Developer mode is locked.")
      developerRepeatedResult(NULL)
      return(NULL)
    }

    m = modelFit()

    if (is.null(m)) {
      developerRepeatedStatus("Fit a model before running repeated scoring.")
      developerRepeatedResult(NULL)
      return(NULL)
    }

    if (is.null(rv$chatProvider)) {
      developerRepeatedStatus("Choose and apply a chat provider before running repeated explanation scoring.")
      developerRepeatedResult(NULL)
      return(NULL)
    }

    totalRuns = suppressWarnings(as.integer(input$developerScoringRunCount %||% 3L))

    if (is.na(totalRuns) || totalRuns < 1L) {
      totalRuns = 1L
    }

    totalRuns = min(totalRuns, 50L)
    grades = vector("list", totalRuns)
    runRows = vector("list", totalRuns)
    startTime = Sys.time()

    shiny::withProgress(
      message = "Running repeated explanation scoring",
      value = 0,
      {
        for (runIdx in seq_len(totalRuns)) {
          runStartTime = Sys.time()
          elapsedBeforeRun = as.numeric(difftime(runStartTime, startTime, units = "secs"))
          progressText = buildDeveloperScoringProgressText(
            completed = runIdx - 1L,
            total = totalRuns,
            elapsedSeconds = elapsedBeforeRun
          )

          shiny::incProgress(
            amount = 0,
            detail = progressText
          )
          developerRepeatedStatus(progressText)

          explanationText = tryCatch(
            buildAppExplanation(
              model = m,
              chatProvider = rv$chatProvider,
              useExplanationCache = FALSE
            ),
            error = function(e) {
              NULL
            }
          )

          if (!is.null(explanationText)) {
            explanationText = postProcessExplanationText(explanationText)
          }

          scoredGrade = NULL
          statusText = "scored"

          if (is.null(explanationText) || !nzchar(trimws(as.character(explanationText)))) {
            statusText = "no explanation returned"
          } else {
            scoredGrade = tryCatch(
              grade(
                x = m,
                explanation = explanationText,
                method = "deterministic",
                autoScore = TRUE
              ),
              error = function(e) {
                statusText <<- paste("scoring failed:", conditionMessage(e))
                NULL
              }
            )
          }

          grades[[runIdx]] = scoredGrade
          runElapsed = as.numeric(difftime(Sys.time(), runStartTime, units = "secs"))

          runRows[[runIdx]] = data.frame(
            run = runIdx,
            scored = inherits(scoredGrade, "wmfmGrade"),
            overallScore = round(extractDeveloperGradeOverallScore(scoredGrade), 2),
            mark = round(extractDeveloperGradeMark(scoredGrade), 2),
            elapsedSeconds = round(runElapsed, 1),
            status = statusText,
            stringsAsFactors = FALSE
          )

          elapsedAfterRun = as.numeric(difftime(Sys.time(), startTime, units = "secs"))
          progressText = buildDeveloperScoringProgressText(
            completed = runIdx,
            total = totalRuns,
            elapsedSeconds = elapsedAfterRun
          )

          shiny::incProgress(
            amount = 1 / totalRuns,
            detail = progressText
          )
          developerRepeatedStatus(progressText)
        }
      }
    )

    runTable = do.call(rbind, runRows)
    elapsedSeconds = as.numeric(difftime(Sys.time(), startTime, units = "secs"))

    developerRepeatedResult(list(
      grades = grades,
      runTable = runTable,
      totalRuns = totalRuns,
      elapsedSeconds = elapsedSeconds,
      createdAt = as.character(Sys.time())
    ))

    developerRepeatedStatus(
      paste0(
        "Repeated scoring complete. ",
        buildDeveloperScoringProgressText(
          completed = nrow(runTable),
          total = totalRuns,
          elapsedSeconds = elapsedSeconds
        )
      )
    )
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

  output$developerRepeatedScoreSummaryTable = shiny::renderTable({
    buildDeveloperRepeatedScoringSummaryTable(developerRepeatedResult())
  })

  output$developerRepeatedScoreRunTable = shiny::renderTable({
    buildDeveloperRepeatedScoringRunTable(developerRepeatedResult())
  })

  list(
    developerGrade = developerGrade,
    developerRepeatedResult = developerRepeatedResult
  )
}
