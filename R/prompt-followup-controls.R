#' Build deterministic follow-up explanation-control prompt block
#'
#' Converts supported follow-up classification categories into deterministic,
#' non-user-authored guidance blocks. Raw follow-up question text is never
#' inserted as an instruction.
#'
#' @param followupPayload Optional list produced by
#'   \code{classifyModelFollowupQuestion()}.
#'
#' @return A character scalar deterministic guidance block, or an empty string.
#' @keywords internal
#' @noRd
buildFollowupExplanationControlPromptBlock = function(followupPayload = NULL) {
  payload = followupPayload
  if (!is.list(payload)) {
    return("")
  }

  if (!isTRUE(payload$supported)) {
    return("")
  }

  category = as.character(payload$category %||% "")
  if (!nzchar(category) || identical(category, "no_followup")) {
    return("")
  }

  linesByCategory = list(
    concise_answer = c(
      "Deterministic follow-up explanation control:",
      "- Keep the response concise: use short sentences and avoid extra detail that is not needed to answer the model question.",
      "- Preserve all required WMFM uncertainty and final-answer safeguards while compressing wording."
    ),
    emphasis_uncertainty = c(
      "Deterministic follow-up explanation control:",
      "- Prioritise uncertainty interpretation: explicitly explain what the confidence interval implies for strength and precision of evidence.",
      "- Do not convert confidence-interval language into probability claims."
    ),
    emphasis_effect_size = c(
      "Deterministic follow-up explanation control:",
      "- Prioritise the estimated effect size and its units before secondary narrative details.",
      "- Keep effect-size interpretation aligned with WMFM supplied quantities only."
    ),
    emphasis_practical_interpretation = c(
      "Deterministic follow-up explanation control:",
      "- Emphasise practical interpretation in plain language while preserving exact WMFM quantities.",
      "- Prefer concrete real-world meaning over technical phrasing."
    ),
    emphasis_group_comparison = c(
      "Deterministic follow-up explanation control:",
      "- Prioritise group-comparison interpretation using WMFM comparison quantities and uncertainty.",
      "- Do not infer differences from interval overlap alone."
    ),
    emphasis_interaction = c(
      "Deterministic follow-up explanation control:",
      "- Prioritise interaction interpretation and describe how one predictor changes the association of another.",
      "- Anchor interaction statements to WMFM-provided conditional quantities."
    ),
    beginner_friendly = c(
      "Deterministic follow-up explanation control:",
      "- Use beginner-friendly wording: avoid jargon where possible and explain technical terms briefly when needed.",
      "- Keep the explanation faithful to WMFM evidence and safeguards."
    ),
    focus_research_question = c(
      "Deterministic follow-up explanation control:",
      "- Keep the explanation tightly focused on directly answering the stored research question.",
      "- De-emphasise side details that do not materially help answer that question."
    ),
    unit_change_request = c(
      "Deterministic follow-up explanation control:",
      "- Treat the request as a bounded unit-change interpretation preference and explain the model effect using the requested unit framing.",
      "- Use the WMFM deterministic requested unit-change interpretation when it is supplied.",
      "- Do not invent new computations; keep interpretation anchored to WMFM-provided model quantities and safeguards.",
      "- Prefer revising the relevant numeric-effect sentence over appending a separate prediction-style follow-up paragraph."
    ),
    prediction_request = c(
      "Deterministic follow-up explanation control:",
      "- Use the WMFM deterministic prediction payload only as context for consistency checks.",
      "- WMFM will append the deterministic numeric follow-up answer after the main explanation.",
      "- Do not write a second, approximate, or contradictory follow-up prediction inside the main explanation.",
      "- Do not recompute, alter, or extend prediction quantities beyond the deterministic payload supplied by WMFM.",
      "- Do not invent a pass mark, success threshold, affordability threshold, or other definition of whether an outcome is good unless the user supplied it."
    ),
    prediction_interval_request = c(
      "Deterministic follow-up explanation control:",
      "- Use the WMFM deterministic prediction payload only as context for consistency checks.",
      "- WMFM will append the deterministic numeric follow-up answer after the main explanation.",
      "- Do not write a second, approximate, or contradictory follow-up prediction inside the main explanation.",
      "- Do not recompute, alter, or extend prediction quantities beyond the deterministic payload supplied by WMFM.",
      "- Do not invent a pass mark, success threshold, affordability threshold, or other definition of whether an outcome is good unless the user supplied it."
    ),
    adjustment_prediction_comparison = c(
      "Deterministic follow-up explanation control:",
      "- Treat the request as a bounded comparison between the adjusted model and a simpler weight-only log-log model.",
      "- Directly answer whether the adjustment variables improve prediction using WMFM supplied fit-comparison quantities.",
      "- Do not only describe the adjusted model; include the simpler-model versus adjusted-model comparison.",
      "- Use WMFM supplied fit-comparison quantities only; do not claim out-of-sample predictive improvement.",
      "- Explain the result as in-sample fit on the log-response scale, using plain proportional-change wording where relevant."
    )
  )

  lines = linesByCategory[[category]]
  if (is.null(lines)) {
    return("")
  }

  predictionStatus = as.character(payload$predictionResult$status %||% "")
  if (identical(predictionStatus, "extrapolation_blocked")) {
    lines = c(
      lines,
      "- The deterministic prediction was blocked because the requested values require unsupported extrapolation.",
      "- Do not estimate, approximate, work backwards, or speculate about the requested outcome.",
      "- State only that WMFM does not support a prediction outside the permitted observed range."
    )
  }

  if (identical(category, "unit_change_request")) {
    lines = c(
      lines,
      "- First answer the main research question.",
      "- Weave the requested unit-change interpretation into the main numeric-effect explanation rather than adding a separate follow-up paragraph."
    )
  } else {
    lines = c(
      lines,
      "- First answer the main research question.",
      "- If you answer this follow-up, place it in a separate paragraph after the main research-question answer.",
      "- Keep the follow-up paragraph clearly tied to the follow-up request without adding headings unless existing style already uses headings."
    )
  }

  paste0("\n", paste(lines, collapse = "\n"), "\n")
}
