#' Build a constrained tutor-style prompt for explaining the explanation
#'
#' Uses the deterministic teaching summary as the main source of truth for an
#' optional, more conversational explanation of how the model explanation was
#' constructed. This avoids exposing raw audit internals and constrains the
#' language model to the information already shown in the app.
#'
#' @param teachingSummary A `wmfmExplanationTeachingSummary` object.
#' @param modelExplanation Optional character scalar giving the existing model
#'   explanation shown to the student.
#' @param researchQuestion Optional character scalar.
#'
#' @return A single character string prompt.
#' @keywords internal
buildExplanationTutorPrompt = function(
    teachingSummary,
    modelExplanation = NULL,
    researchQuestion = NULL
) {

  if (is.null(teachingSummary)) {
    stop("`teachingSummary` must not be NULL.", call. = FALSE)
  }

  evidenceLines = character(0)

  if (is.data.frame(teachingSummary$evidenceTable) && nrow(teachingSummary$evidenceTable) > 0) {
    evidenceLines = vapply(seq_len(nrow(teachingSummary$evidenceTable)), function(i) {
      paste0(
        "- ",
        teachingSummary$evidenceTable$section[[i]],
        ": ",
        teachingSummary$evidenceTable$summary[[i]]
      )
    }, character(1))
  }

  researchQuestionText = trimws(as.character(researchQuestion %||% ""))
  modelExplanationText = trimws(as.character(modelExplanation %||% ""))

  parts = c(
    "You are a friendly statistics tutor.",
    "Your job is to explain, in simple classroom language, how an existing model explanation was put together.",
    "Use only the information provided below.",
    "Do not invent new calculations, new evidence, new interpretations, or new model details.",
    "Do not mention prompts, hidden reasoning, chain of thought, internal system messages, or developer diagnostics.",
    "Do not say that the model \"thought\" anything.",
    "Write 1 to 3 short paragraphs.",
    "Keep the tone supportive, concrete, and student-friendly.",
    "Start from the research question when one is available.",
    "Very early in the explanation, briefly describe the data used in plain language before discussing the fitted results.",
    "If variable names appear, wrap them in backticks so the app can style them clearly.",
    "",
    if (nzchar(modelExplanationText)) c("Main model explanation shown to the student:", modelExplanationText, "") else NULL,
    "Teaching summary already shown in the app:",
    paste0("- Data used: ", teachingSummary$dataDescription %||% ""),
    paste0("- Scale used: ", teachingSummary$interpretationScale),
    paste0("- Starting values and comparison groups: ", teachingSummary$baselineChoice),
    paste0("- Change being described: ", teachingSummary$xChangeDescription),
    paste0("- Main result wording: ", teachingSummary$mainEffectDescription),
    paste0("- Uncertainty wording: ", teachingSummary$uncertaintySummary),
    if (length(evidenceLines) > 0) c("- Main pieces of information used:", evidenceLines) else NULL,
    paste0("- Research question link: ", teachingSummary$researchQuestionLink),
    if (nzchar(researchQuestionText)) c("", paste0("Research question: ", researchQuestionText)) else NULL,
    "",
    "Now explain this more simply for a student who wants to understand why the app described the model that way.",
    "Begin by restating the research question in plain language, then briefly say what data the model used, and then explain the interpretation choices."
  )

  paste(parts, collapse = "\n")
}

#' Build an optional tutor-style explanation of the explanation
#'
#' @param teachingSummary A `wmfmExplanationTeachingSummary` object.
#' @param chatProvider A chat provider object with a `$chat()` method.
#' @param modelExplanation Optional character scalar.
#' @param researchQuestion Optional character scalar.
#'
#' @return A character string or `NULL`.
#' @keywords internal
buildAppTeachingTutorExplanation = function(
    teachingSummary,
    chatProvider = NULL,
    modelExplanation = NULL,
    researchQuestion = NULL
) {

  if (is.null(chatProvider) || is.null(teachingSummary)) {
    return(NULL)
  }

  prompt = buildExplanationTutorPrompt(
    teachingSummary = teachingSummary,
    modelExplanation = modelExplanation,
    researchQuestion = researchQuestion
  )

  out = tryCatch(
    chatProvider$chat(prompt),
    error = function(e) {
      NULL
    }
  )

  out = trimws(as.character(out %||% ""))

  if (!nzchar(out)) {
    return(NULL)
  }

  out
}
