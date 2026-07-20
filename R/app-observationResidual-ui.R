#' Build a display table for an observation residual result
#'
#' @param result A deterministic observation residual result.
#'
#' @return A data frame prepared for display, or `NULL` when no supported
#'   result is available.
#'
#' @keywords internal
#' @noRd
buildObservationResidualDisplayTable = function(result) {
  if (!is.list(result) || !identical(result$status, "ok")) {
    return(NULL)
  }

  observations = result$observations
  if (!is.data.frame(observations) || !nrow(observations)) {
    return(NULL)
  }

  data.frame(
    Rank = observations$rank,
    Observation = observations$observation,
    `Source row` = observations$row,
    Observed = signif(observations$observed, 6),
    Fitted = signif(observations$fitted, 6),
    Residual = signif(unname(observations$residual), 6),
    `Ranking percentile` = paste0(signif(observations$percentile, 4), "%"),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
}

#' Render an observation residual result for the Model Explanation tab
#'
#' @param followupPayload Classified and enriched model follow-up payload.
#'
#' @return A Shiny tag list, or `NULL` when the follow-up question is not an
#'   observation residual request.
#'
#' @keywords internal
#' @noRd
buildObservationResidualResultUi = function(followupPayload) {
  if (!is.list(followupPayload) ||
      !identical(followupPayload$category, "observation_residual_request")) {
    return(NULL)
  }

  result = followupPayload$observationResidualResult %||% list(
    status = "unsupported",
    reason = "result_not_available"
  )

  if (!identical(result$status, "ok")) {
    return(shiny::tags$div(
      class = "alert alert-warning wmfm-observation-residual-result",
      shiny::tags$strong("Observation comparison unavailable"),
      shiny::tags$p(
        "WMFM could not produce the requested residual ranking. Existing-observation residual inspection currently supports ordinary linear models only."
      )
    ))
  }

  displayTable = buildObservationResidualDisplayTable(result)
  if (is.null(displayTable)) {
    return(NULL)
  }

  shiny::tags$div(
    class = "wmfm-observation-residual-result",
    shiny::tags$h4("Existing observations compared with fitted values"),
    shiny::tags$p(
      class = "wmfm-explanation-helper-note",
      paste0(
        "The table ranks ", nrow(displayTable), " of ",
        result$totalFittedObservations,
        " fitted observations using ", result$interpretation, "."
      )
    ),
    shiny::tags$div(
      class = "table-responsive",
      shiny::tags$table(
        class = "table table-striped table-condensed wmfm-observation-residual-table",
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
    shiny::tags$p(
      class = "wmfm-explanation-helper-note",
      "Residuals are observed minus fitted on the model response scale. These comparisons do not by themselves identify bargains, outliers, data errors, or causal effects."
    )
  )
}
