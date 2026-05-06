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



#' Extract developer scoring interaction metadata
#'
#' @param model A fitted model object.
#'
#' @return A list with interaction term metadata.
#'
#' @keywords internal
extractDeveloperScoringInteractionInfo = function(model) {
  out = list(
    interactionTerms = character(0),
    interactionMinPValue = NA_real_
  )

  coefficientTable = tryCatch(
    withCallingHandlers(
      summary(model)$coefficients,
      warning = function(w) {
        if (grepl("essentially perfect fit", conditionMessage(w), fixed = TRUE)) {
          invokeRestart("muffleWarning")
        }
      }
    ),
    error = function(e) {
      NULL
    }
  )

  if (is.null(coefficientTable) || !is.matrix(coefficientTable)) {
    return(out)
  }

  termNames = rownames(coefficientTable)

  if (is.null(termNames) || length(termNames) == 0) {
    return(out)
  }

  interactionIdx = grepl(":", termNames, fixed = TRUE)

  if (!any(interactionIdx)) {
    return(out)
  }

  pColumnIdx = grep("^Pr\\(", colnames(coefficientTable))
  interactionMinPValue = NA_real_

  if (length(pColumnIdx) >= 1) {
    interactionPValues = suppressWarnings(
      as.numeric(coefficientTable[interactionIdx, pColumnIdx[1]])
    )
    interactionPValues = interactionPValues[!is.na(interactionPValues)]

    if (length(interactionPValues) > 0) {
      interactionMinPValue = min(interactionPValues)
    }
  }

  list(
    interactionTerms = termNames[interactionIdx],
    interactionMinPValue = interactionMinPValue
  )
}

#' Resolve the developer scoring model type
#'
#' @param model A fitted model object.
#' @param modelType Optional UI model type.
#'
#' @return A character scalar.
#' @keywords internal
resolveDeveloperScoringModelType = function(model, modelType = NULL) {
  modelType = trimws(as.character(modelType %||% ""))[1]

  if (nzchar(modelType) && modelType %in% c("lm", "logistic", "poisson")) {
    return(modelType)
  }

  if (inherits(model, "glm")) {
    familyName = tryCatch(model$family$family, error = function(e) NA_character_)

    if (identical(familyName, "binomial")) {
      return("logistic")
    }

    if (identical(familyName, "poisson")) {
      return("poisson")
    }
  }

  "lm"
}

#' Build a WMFM model object for developer scoring
#'
#' The app stores the current fitted model as a native fitted model object. The
#' grading API is intentionally defined on `wmfmModel`, so this helper wraps the
#' current app state in the same classed object before calling `grade()`.
#'
#' @param model A fitted model object from the app.
#' @param rv App reactive values object.
#' @param input Shiny input object.
#' @param explanationText Character scalar explanation text.
#'
#' @return A `wmfmModel` object, or `NULL` when the current state is incomplete.
#' @keywords internal
buildDeveloperScoringWmfmModel = function(
    model,
    rv,
    input,
    explanationText = NULL
) {
  if (is.null(model)) {
    return(NULL)
  }

  if (inherits(model, "wmfmModel")) {
    return(model)
  }

  formula = tryCatch(stats::formula(model), error = function(e) NULL)

  if (is.null(formula) || !inherits(formula, "formula")) {
    formulaText = trimws(as.character(input$formula_text %||% ""))[1]

    if (nzchar(formulaText)) {
      formula = tryCatch(stats::as.formula(formulaText), error = function(e) NULL)
    }
  }

  if (is.null(formula) || !inherits(formula, "formula")) {
    return(NULL)
  }

  data = rv$data

  if (!is.data.frame(data)) {
    data = tryCatch(stats::model.frame(model), error = function(e) NULL)
  }

  if (!is.data.frame(data)) {
    return(NULL)
  }

  interactionInfo = extractDeveloperScoringInteractionInfo(model)
  modelType = resolveDeveloperScoringModelType(
    model = model,
    modelType = input$model_type %||% NULL
  )

  newWmfmModel(
    model = model,
    formula = formula,
    modelType = modelType,
    data = data,
    dataContext = attr(model, "wmfm_dataset_doc", exact = TRUE) %||% rv$userDatasetContext %||% NULL,
    researchQuestion = attr(model, "wmfm_research_question", exact = TRUE) %||% rv$researchQuestion %||% NULL,
    equations = rv$modelEquations,
    explanation = explanationText %||% rv$modelExplanation,
    explanationAudit = rv$modelExplanationAudit,
    explanationClaimEvidenceMap = NULL,
    modelProfile = tryCatch(
      buildExplanationModelProfile(
        model = model,
        data = data,
        modelType = modelType
      ),
      error = function(e) {
        NULL
      }
    ),
    interactionTerms = interactionInfo$interactionTerms,
    interactionMinPValue = interactionInfo$interactionMinPValue,
    meta = list(
      sourceFunction = "appDeveloperScoring",
      exampleName = attr(model, "wmfm_example_name", exact = TRUE) %||% NA_character_,
      package = attr(model, "wmfm_dataset_package", exact = TRUE) %||% NA_character_
    )
  )
}

#' Grade the current app explanation for developer scoring
#'
#' @param model A fitted model object from the app.
#' @param rv App reactive values object.
#' @param input Shiny input object.
#' @param explanationText Character scalar explanation text.
#' @param method Character scalar naming the scoring method.
#'
#' @return A scored `wmfmGrade` object.
#' @keywords internal
scoreDeveloperExplanation = function(
    model,
    rv,
    input,
    explanationText,
    method = "deterministic"
) {
  wmfmModel = buildDeveloperScoringWmfmModel(
    model = model,
    rv = rv,
    input = input,
    explanationText = explanationText
  )

  if (is.null(wmfmModel)) {
    stop("Could not construct a WMFM grading object from the current app state.", call. = FALSE)
  }

  grade(
    x = wmfmModel,
    explanation = explanationText,
    method = method,
    autoScore = TRUE
  )
}



#' Build a compact developer scoring grade export
#'
#' @param gradeObj A `wmfmGrade` object.
#' @param method Character scalar naming the scoring method to inspect.
#'
#' @return A list suitable for JSON serialisation.
#' @keywords internal
buildDeveloperScoringGradeExport = function(gradeObj, method = "deterministic") {
  if (!inherits(gradeObj, "wmfmGrade")) {
    return(NULL)
  }

  list(
    class = class(gradeObj),
    scoreScale = suppressWarnings(as.numeric(gradeObj$scoreScale)[1]),
    scored = isTRUE(gradeObj$meta$scored),
    method = method,
    summary = buildDeveloperScoringSummaryTable(gradeObj, method = method),
    metrics = buildDeveloperScoringMetricTable(gradeObj, method = method),
    whereMarksLost = buildDeveloperScoringLossTable(
      gradeObj = gradeObj,
      tableName = "whereMarksLost",
      method = method
    ),
    strengths = buildDeveloperScoringLossTable(
      gradeObj = gradeObj,
      tableName = "strengths",
      method = method
    ),
    objectSummaryText = buildDeveloperScoringObjectText(gradeObj)
  )
}

#' Build a compact developer repeated scoring export
#'
#' @param repeatedResult A developer repeated scoring result object.
#' @param method Character scalar naming the scoring method to inspect.
#'
#' @return A list suitable for JSON serialisation.
#' @keywords internal
buildDeveloperRepeatedScoringExport = function(
    repeatedResult,
    method = "deterministic"
) {
  if (is.null(repeatedResult) || !is.list(repeatedResult)) {
    return(NULL)
  }

  runDetails = repeatedResult$runDetails %||% list()

  list(
    totalRuns = repeatedResult$totalRuns %||% NA_integer_,
    elapsedSeconds = repeatedResult$elapsedSeconds %||% NA_real_,
    createdAt = repeatedResult$createdAt %||% NA_character_,
    summary = buildDeveloperRepeatedScoringSummaryTable(repeatedResult),
    runTable = buildDeveloperRepeatedScoringRunTable(repeatedResult),
    runs = lapply(runDetails, function(runDetail) {
      list(
        run = runDetail$run %||% NA_integer_,
        status = runDetail$status %||% NA_character_,
        elapsedSeconds = runDetail$elapsedSeconds %||% NA_real_,
        explanation = runDetail$explanation %||% NA_character_,
        grade = buildDeveloperScoringGradeExport(
          gradeObj = runDetail$grade,
          method = method
        )
      )
    })
  )
}

#' Build developer scoring JSON export payload
#'
#' @param model A fitted model object from the app.
#' @param rv App reactive values object.
#' @param input Shiny input object.
#' @param gradeObj Current single-run grade object.
#' @param repeatedResult Current repeated-run result object.
#' @param method Character scalar naming the scoring method to inspect.
#'
#' @return A list suitable for JSON serialisation.
#' @keywords internal
buildDeveloperScoringJsonPayload = function(
    model = NULL,
    rv = NULL,
    input = NULL,
    gradeObj = NULL,
    repeatedResult = NULL,
    method = "deterministic"
) {
  formulaText = tryCatch(
    paste(deparse(stats::formula(model)), collapse = " "),
    error = function(e) {
      trimws(as.character(input$formula_text %||% ""))[1]
    }
  )

  list(
    schema = "wmfm-developer-scoring-export",
    schemaVersion = "1.0.0",
    createdAt = as.character(Sys.time()),
    purpose = "developer scoring and grading diagnostics",
    method = method,
    appState = list(
      modelClass = class(model),
      modelType = resolveDeveloperScoringModelType(
        model = model,
        modelType = input$model_type %||% NULL
      ),
      formula = formulaText,
      exampleName = attr(model, "wmfm_example_name", exact = TRUE) %||% NA_character_,
      package = attr(model, "wmfm_dataset_package", exact = TRUE) %||% NA_character_,
      researchQuestion = attr(model, "wmfm_research_question", exact = TRUE) %||%
        rv$researchQuestion %||% NA_character_
    ),
    current = list(
      explanation = rv$modelExplanation %||% NA_character_,
      grade = buildDeveloperScoringGradeExport(
        gradeObj = gradeObj,
        method = method
      )
    ),
    repeated = buildDeveloperRepeatedScoringExport(
      repeatedResult = repeatedResult,
      method = method
    )
  )
}

#' Build developer scoring JSON export text
#'
#' @param payload A developer scoring export payload.
#'
#' @return A JSON character scalar.
#' @keywords internal
buildDeveloperScoringJsonText = function(payload) {
  jsonlite::toJSON(
    payload,
    auto_unbox = TRUE,
    pretty = TRUE,
    null = "null",
    dataframe = "rows",
    POSIXt = "ISO8601",
    na = "null"
  )
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
      shiny::tableOutput("developerRepeatedScoreRunTable"),
      shiny::tags$hr(class = "hr-tight"),
      shiny::h5("Export scoring results"),
      shiny::helpText(
        "Download this JSON file and upload it in ChatGPT when you want help evaluating scoring and grading behaviour."
      ),
      shiny::downloadButton(
        outputId = "developerScoringJsonDownload",
        label = "Download scoring JSON",
        class = "btn btn-secondary btn-sm"
      ),
      shiny::tags$br(),
      shiny::tags$br(),
      shiny::verbatimTextOutput("developerScoringJsonPreview")
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
      scoreDeveloperExplanation(
        model = m,
        rv = rv,
        input = input,
        explanationText = explanationText,
        method = "deterministic"
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
    runDetails = vector("list", totalRuns)
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
              scoreDeveloperExplanation(
                model = m,
                rv = rv,
                input = input,
                explanationText = explanationText,
                method = "deterministic"
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

          runDetails[[runIdx]] = list(
            run = runIdx,
            status = statusText,
            elapsedSeconds = round(runElapsed, 1),
            explanation = explanationText %||% NA_character_,
            grade = scoredGrade
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
      runDetails = runDetails,
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

  developerScoringJsonPayload = shiny::reactive({
    buildDeveloperScoringJsonPayload(
      model = modelFit(),
      rv = rv,
      input = input,
      gradeObj = developerGrade(),
      repeatedResult = developerRepeatedResult(),
      method = "deterministic"
    )
  })

  output$developerScoringJsonPreview = shiny::renderText({
    payload = developerScoringJsonPayload()
    paste(
      "JSON export ready.",
      paste0("Schema: ", payload$schema, " v", payload$schemaVersion),
      paste0("Repeated runs: ", payload$repeated$totalRuns %||% 0),
      sep = "\n"
    )
  })

  output$developerScoringJsonDownload = shiny::downloadHandler(
    filename = function() {
      paste0(
        "wmfm-scoring-export-",
        format(Sys.time(), "%Y%m%d-%H%M%S"),
        ".json"
      )
    },
    content = function(file) {
      writeLines(
        buildDeveloperScoringJsonText(developerScoringJsonPayload()),
        con = file,
        useBytes = TRUE
      )
    }
  )

  list(
    developerGrade = developerGrade,
    developerRepeatedResult = developerRepeatedResult
  )
}
