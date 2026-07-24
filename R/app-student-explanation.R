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

#' Register student explanation workspace observers
#'
#' @param input Shiny input object.
#' @param output Shiny output object.
#' @param session Shiny session object.
#' @param modelFit Reactive fitted model holder.
#'
#' @return Invisible `NULL`.
#'
#' @keywords internal
registerStudentExplanationObservers = function(
    input,
    output,
    session,
    modelFit
) {
  output$studentExplanationToolbarUi = shiny::renderUI({
    buildStudentExplanationToolbarUi(modelFit())
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

  shiny::observeEvent(modelFit(), {
    shiny::updateTextAreaInput(
      session = session,
      inputId = "studentExplanationText",
      value = ""
    )
  }, ignoreInit = TRUE)

  invisible(NULL)
}
