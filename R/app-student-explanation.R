#' Build student explanation insertion choices
#'
#' Creates model-aware sentence fragments that can be inserted into a student's
#' explanation without generating interpretive prose for them.
#'
#' @param model A fitted model object.
#' @param confidenceLevel Confidence level used for interval fragments.
#'
#' @return A list with named `coefficients` and `intervals` character vectors.
#'
#' @keywords internal
buildStudentExplanationInsertionChoices = function(
    model,
    confidenceLevel = 0.95
) {
  if (is.null(model)) {
    return(list(coefficients = character(0), intervals = character(0)))
  }

  coefficientValues = tryCatch(
    stats::coef(model),
    error = function(e) numeric(0)
  )

  coefficientValues = coefficientValues[is.finite(coefficientValues)]
  coefficientNames = names(coefficientValues)

  if (is.null(coefficientNames)) {
    coefficientNames = rep("coefficient", length(coefficientValues))
  }

  coefficientFragments = vapply(
    seq_along(coefficientValues),
    function(index) {
      paste0(
        "the estimated coefficient for ",
        formatStudentExplanationTerm(coefficientNames[[index]]),
        " is ",
        formatStudentExplanationNumber(coefficientValues[[index]])
      )
    },
    character(1)
  )
  names(coefficientFragments) = formatStudentExplanationTerm(coefficientNames)

  intervalMatrix = tryCatch(
    stats::confint.default(model, level = confidenceLevel),
    error = function(e) NULL
  )

  intervalFragments = character(0)
  if (!is.null(intervalMatrix) && is.matrix(intervalMatrix) && ncol(intervalMatrix) >= 2) {
    intervalNames = rownames(intervalMatrix)
    if (is.null(intervalNames)) {
      intervalNames = rep("coefficient", nrow(intervalMatrix))
    }

    confidenceLabel = paste0(round(100 * confidenceLevel), "%")
    intervalFragments = vapply(
      seq_len(nrow(intervalMatrix)),
      function(index) {
        paste0(
          "the ", confidenceLabel, " confidence interval for ",
          formatStudentExplanationTerm(intervalNames[[index]]),
          " is ", formatStudentExplanationNumber(intervalMatrix[index, 1]),
          " to ", formatStudentExplanationNumber(intervalMatrix[index, 2])
        )
      },
      character(1)
    )
    names(intervalFragments) = formatStudentExplanationTerm(intervalNames)
  }

  list(
    coefficients = coefficientFragments,
    intervals = intervalFragments
  )
}

#' Format a model term for the student explanation toolbar
#'
#' @param term Character vector of model term labels.
#'
#' @return A character vector of readable labels.
#'
#' @keywords internal
formatStudentExplanationTerm = function(term) {
  term = as.character(term)
  term[term == "(Intercept)"] = "the intercept"
  term = gsub(":", " by ", term, fixed = TRUE)
  term
}

#' Format a number for the student explanation toolbar
#'
#' @param value Numeric vector.
#'
#' @return A character vector.
#'
#' @keywords internal
formatStudentExplanationNumber = function(value) {
  formatC(value, digits = 4, format = "fg", flag = "#")
}

#' Build the student explanation toolbar
#'
#' @param model A fitted model object.
#'
#' @return A Shiny tag list.
#'
#' @keywords internal
buildStudentExplanationToolbarUi = function(model) {
  if (is.null(model)) {
    return(shiny::helpText("Fit a model to make statistical results available for insertion."))
  }

  choices = buildStudentExplanationInsertionChoices(model)

  shiny::tagList(
    shiny::tags$div(
      class = "wmfm-student-explanation-toolbar",
      shiny::tags$div(
        class = "wmfm-student-explanation-toolbar-group",
        shiny::selectInput(
          inputId = "studentExplanationCoefficient",
          label = "Coefficient",
          choices = choices$coefficients,
          width = "100%"
        ),
        shiny::actionButton(
          inputId = "insertStudentExplanationCoefficient",
          label = "Insert coefficient",
          class = "btn-default btn-sm"
        )
      ),
      shiny::tags$div(
        class = "wmfm-student-explanation-toolbar-group",
        shiny::selectInput(
          inputId = "studentExplanationInterval",
          label = "Confidence interval",
          choices = choices$intervals,
          width = "100%"
        ),
        shiny::actionButton(
          inputId = "insertStudentExplanationInterval",
          label = "Insert interval",
          class = "btn-default btn-sm"
        )
      )
    )
  )
}

#' Build formative feedback for a student explanation
#'
#' Converts a scored `wmfmGrade` object into a compact feedback structure for
#' the student workspace. The result focuses on strengths and revision
#' priorities rather than exposing the full developer scoring record.
#'
#' @param gradeObj A scored `wmfmGrade` object.
#' @param method Character scalar naming the scoring method.
#'
#' @return A named list containing the score, strengths, and revision priorities.
#'
#' @keywords internal
buildStudentExplanationFeedback = function(
    gradeObj,
    method = "deterministic"
) {
  if (!inherits(gradeObj, "wmfmGrade")) {
    return(NULL)
  }

  methodScore = gradeObj$scores$byMethod[[method]]
  methodFeedback = gradeObj$feedback$byMethod[[method]]

  if (is.null(methodScore) || is.null(methodFeedback)) {
    return(NULL)
  }

  extractFeedbackText = function(x) {
    if (!is.data.frame(x) || nrow(x) == 0) {
      return(character(0))
    }

    preferredColumns = c("reason", "message", "feedback", "label")
    textColumn = preferredColumns[preferredColumns %in% names(x)][1]

    if (is.na(textColumn) || length(textColumn) == 0) {
      return(character(0))
    }

    values = trimws(as.character(x[[textColumn]]))
    unique(values[!is.na(values) & nzchar(values)])
  }

  strengths = extractFeedbackText(methodFeedback$strengths)
  priorities = extractFeedbackText(methodFeedback$whereMarksLost)

  if (length(priorities) == 0) {
    priorities = extractFeedbackText(methodFeedback$missingElements)
  }

  list(
    overallScore = suppressWarnings(as.numeric(methodScore$overallScore)[1]),
    mark = suppressWarnings(as.numeric(methodScore$mark)[1]),
    scoreScale = suppressWarnings(as.numeric(gradeObj$scoreScale)[1]),
    strengths = strengths,
    priorities = priorities
  )
}

#' Render formative student explanation feedback
#'
#' @param feedback A feedback structure returned by
#'   `buildStudentExplanationFeedback()`.
#'
#' @return A Shiny tag list, or `NULL` when no feedback is available.
#'
#' @keywords internal
renderStudentExplanationFeedbackUi = function(feedback) {
  if (is.null(feedback)) {
    return(NULL)
  }

  renderItems = function(items, emptyText) {
    if (length(items) == 0) {
      return(shiny::tags$p(emptyText))
    }

    shiny::tags$ul(
      lapply(items, shiny::tags$li)
    )
  }

  scoreText = ""
  if (is.finite(feedback$overallScore)) {
    scoreText = paste0(round(feedback$overallScore), "%")
  }

  shiny::tags$div(
    class = "wmfm-student-explanation-feedback",
    shiny::tags$h4("Feedback on your explanation"),
    if (nzchar(scoreText)) {
      shiny::tags$p(
        class = "wmfm-student-explanation-score",
        paste("Current rubric score:", scoreText)
      )
    },
    shiny::tags$h5("What is working well"),
    renderItems(
      feedback$strengths,
      "No clear strengths were identified yet. Add a precise statement about what the fitted model shows."
    ),
    shiny::tags$h5("What to revise next"),
    renderItems(
      feedback$priorities,
      "No major revision priorities were identified by the current rubric."
    ),
    shiny::tags$p(
      class = "wmfm-student-explanation-feedback-note",
      "Revise your explanation and check it again. This feedback does not reveal WMFM's model explanation."
    )
  )
}

#' Register student explanation workspace observers
#'
#' @param input Shiny input object.
#' @param output Shiny output object.
#' @param session Shiny session object.
#' @param rv App reactive values object.
#' @param modelFit Reactive fitted model holder.
#'
#' @return Invisible `NULL`.
#'
#' @keywords internal
registerStudentExplanationObservers = function(
    input,
    output,
    session,
    rv,
    modelFit
) {
  studentExplanationGrade = shiny::reactiveVal(NULL)
  studentExplanationStatus = shiny::reactiveVal("")

  output$studentExplanationToolbarUi = shiny::renderUI({
    buildStudentExplanationToolbarUi(modelFit())
  })

  output$studentExplanationFeedbackStatus = shiny::renderText({
    studentExplanationStatus()
  })

  output$studentExplanationFeedbackUi = shiny::renderUI({
    feedback = buildStudentExplanationFeedback(studentExplanationGrade())
    renderStudentExplanationFeedbackUi(feedback)
  })

  shiny::observeEvent(input$insertStudentExplanationCoefficient, {
    fragment = input$studentExplanationCoefficient
    if (!is.null(fragment) && nzchar(fragment)) {
      session$sendCustomMessage(
        type = "wmfmInsertStudentExplanation",
        message = list(text = fragment)
      )
    }
  })

  shiny::observeEvent(input$insertStudentExplanationInterval, {
    fragment = input$studentExplanationInterval
    if (!is.null(fragment) && nzchar(fragment)) {
      session$sendCustomMessage(
        type = "wmfmInsertStudentExplanation",
        message = list(text = fragment)
      )
    }
  })

  shiny::observeEvent(input$checkStudentExplanation, {
    model = modelFit()
    explanationText = trimws(as.character(input$studentExplanationText %||% ""))[1]

    if (is.null(model)) {
      studentExplanationGrade(NULL)
      studentExplanationStatus("Fit a model before checking your explanation.")
      return(NULL)
    }

    if (!nzchar(explanationText)) {
      studentExplanationGrade(NULL)
      studentExplanationStatus("Write an explanation before asking WMFM to check it.")
      return(NULL)
    }

    studentExplanationStatus("Checking your explanation...")

    scoredGrade = tryCatch(
      scoreDeveloperExplanation(
        model = model,
        rv = rv,
        input = input,
        explanationText = explanationText,
        method = "deterministic"
      ),
      error = function(e) {
        studentExplanationStatus(
          paste("WMFM could not check the explanation:", conditionMessage(e))
        )
        NULL
      }
    )

    studentExplanationGrade(scoredGrade)

    if (!is.null(scoredGrade)) {
      studentExplanationStatus(
        "Explanation checked. Revise it using the feedback below, then check it again."
      )
    }
  })

  shiny::observeEvent(input$studentExplanationText, {
    if (!is.null(studentExplanationGrade())) {
      studentExplanationGrade(NULL)
      studentExplanationStatus("Your explanation has changed. Check it again for updated feedback.")
    }
  }, ignoreInit = TRUE)

  shiny::observeEvent(modelFit(), {
    shiny::updateTextAreaInput(
      session = session,
      inputId = "studentExplanationText",
      value = ""
    )
    studentExplanationGrade(NULL)
    studentExplanationStatus("")
  }, ignoreInit = TRUE)

  invisible(NULL)
}
