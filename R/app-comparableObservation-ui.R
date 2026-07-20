#' Build a display table for comparable observations
#'
#' @param result A deterministic comparable-observation result.
#' @param displayCount Maximum number of observations to display.
#'
#' @return A data frame prepared for display, or `NULL` when no supported
#'   result is available.
#'
#' @keywords internal
#' @noRd
buildComparableObservationDisplayTable = function(result, displayCount = 5L) {
  if (!is.list(result) || !identical(result$status, "ok")) {
    return(NULL)
  }

  observations = result$observations
  if (!is.data.frame(observations) || !nrow(observations)) {
    return(NULL)
  }

  displayCount = suppressWarnings(as.integer(displayCount[[1]] %||% 5L))
  if (is.na(displayCount) || displayCount < 1L) {
    displayCount = 5L
  }
  observations = observations[seq_len(min(displayCount, nrow(observations))), , drop = FALSE]

  responseLabel = paste0("Observed ", result$responseName %||% "response")
  displayTable = data.frame(
    Rank = observations$rank,
    Observation = observations$observation,
    `Source row` = observations$row,
    response = signif(observations$response, 6),
    `Similarity distance` = signif(observations$distance, 6),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  names(displayTable)[[4]] = responseLabel
  displayTable
}

#' Format comparable-observation predictor settings for display
#'
#' @param result A deterministic comparable-observation result.
#'
#' @return Character scalar describing the resolved predictor settings.
#' @keywords internal
#' @noRd
formatComparableObservationSettings = function(result) {
  settings = result$resolvedPredictorValues
  if (!is.list(settings) || !length(settings)) {
    return("the predictor settings supplied in the follow-up question")
  }

  formatted = vapply(names(settings), function(name) {
    value = settings[[name]]
    valueText = if (length(value)) {
      as.character(value[[1]])
    } else {
      ""
    }
    paste0(name, " = ", valueText)
  }, character(1))

  paste(formatted, collapse = ", ")
}

#' Render comparable observations for the Model Explanation tab
#'
#' @param followupPayload Classified and enriched model follow-up payload.
#'
#' @return A Shiny tag list, or `NULL` when the follow-up question is not a
#'   comparable-observation request.
#'
#' @keywords internal
#' @noRd
buildComparableObservationResultUi = function(followupPayload) {
  if (!is.list(followupPayload) ||
      !identical(followupPayload$category, "comparable_observation_request")) {
    return(NULL)
  }

  result = followupPayload$comparableObservationResult %||% list(
    status = "unsupported",
    reason = "result_not_available"
  )

  if (!identical(result$status, "ok")) {
    return(shiny::tags$div(
      class = "alert alert-warning wmfm-comparable-observation-result",
      shiny::tags$strong("Comparable observations unavailable"),
      shiny::tags$p(
        "WMFM could not find verified comparable observations. This comparison currently supports ordinary linear models only and requires usable predictor values."
      )
    ))
  }

  displayTable = buildComparableObservationDisplayTable(result)
  if (is.null(displayTable)) {
    return(NULL)
  }

  responseValues = as.numeric(result$observations$response)
  responseSummary = paste0(
    "Across the ", result$neighbourCount, " selected observations, observed ",
    result$responseName, " ranged from ",
    formatFollowupPredictionNumber(min(responseValues, na.rm = TRUE)), " to ",
    formatFollowupPredictionNumber(max(responseValues, na.rm = TRUE)),
    ", with a median of ",
    formatFollowupPredictionNumber(stats::median(responseValues, na.rm = TRUE)), "."
  )

  shiny::tags$div(
    class = "wmfm-comparable-observation-result",
    shiny::tags$h4("Comparable observations from the fitted data"),
    shiny::tags$p(
      class = "wmfm-explanation-helper-note",
      paste0(
        "WMFM matched ", result$neighbourCount, " of ",
        result$totalFittedObservations, " fitted observations using ",
        formatComparableObservationSettings(result), "."
      )
    ),
    shiny::tags$p(
      class = "wmfm-explanation-helper-note",
      responseSummary
    ),
    shiny::tags$div(
      class = "table-responsive",
      shiny::tags$table(
        class = "table table-striped table-condensed wmfm-comparable-observation-table",
        shiny::tags$thead(
          shiny::tags$tr(lapply(names(displayTable), shiny::tags$th))
        ),
        shiny::tags$tbody(
          lapply(seq_len(nrow(displayTable)), function(rowIndex) {
            shiny::tags$tr(lapply(displayTable[rowIndex, , drop = FALSE], shiny::tags$td))
          })
        )
      )
    ),
    if (result$neighbourCount > nrow(displayTable)) {
      shiny::tags$p(
        class = "wmfm-explanation-helper-note",
        paste0(
          "The table shows the first ", nrow(displayTable),
          " neighbours; the response summary uses all ",
          result$neighbourCount, " selected observations."
        )
      )
    },
    shiny::tags$p(
      class = "wmfm-explanation-helper-note",
      "Similarity uses only predictors in the fitted model. These comparisons do not by themselves establish that a case is a bargain, unusually good value, or causally different."
    )
  )
}
