#' Build fitted-equation role summary UI
#'
#' Builds the predictor-role summary block used above fitted equations when
#' adjustment-variable metadata is present.
#'
#' @param roleSummary Character vector from
#'   \code{buildEquationDisplayRoleSummary()}.
#'
#' @return A \code{shiny.tag} object, or \code{NULL} when no summary should be
#'   shown.
#' @keywords internal
#' @importFrom htmltools div tags
buildFittedEquationRoleSummaryUi = function(roleSummary) {
  if (length(roleSummary) == 0) {
    return(NULL)
  }

  div(
    style = "margin-bottom: 10px;",
    tags$p(tags$strong("Predictor roles in this fitted model")),
    tags$ul(lapply(roleSummary, tags$li))
  )
}

#' Build fitted-equation content UI
#'
#' Renders fitted-equation content for supported equation output structures while
#' consistently prepending the predictor-role summary when available.
#'
#' @param eq Equation object produced by the fitted-equation pipeline.
#' @param roleSummaryUi Optional summary UI block from
#'   \code{buildFittedEquationRoleSummaryUi()}.
#'
#' @return A \code{shiny.tag} or \code{shiny.tag.list} object.
#' @keywords internal
#' @importFrom htmltools div tagList tags
buildFittedEquationContentUi = function(eq, roleSummaryUi = NULL) {
  if (is.data.frame(eq) && all(c("condition", "equation") %in% names(eq))) {
    items = lapply(seq_len(nrow(eq)), function(i) {
      div(
        tags$p(tags$strong(eq$condition[i])),
        tags$pre(
          style = "white-space: pre; margin-top: -6px; margin-bottom: 8px;",
          eq$equation[i]
        )
      )
    })

    return(tagList(roleSummaryUi, items))
  }

  if (is.character(eq)) {
    return(tagList(
      roleSummaryUi,
      tags$pre(
        style = "white-space: pre; margin: 0;",
        eq
      )
    ))
  }

  tagList(
    roleSummaryUi,
    tags$pre(
      style = "white-space: pre; margin: 0;",
      paste(capture.output(str(eq)), collapse = "\n")
    )
  )
}
