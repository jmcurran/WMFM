#' Generate one or more deliberately bad model explanations
#'
#' Generic for generating plausible but intentionally flawed explanations from
#' a fitted model.
#'
#' @param x An object.
#' @param ... Additional arguments passed to methods.
#'
#' @return Method-dependent output.
#' @export
generateBadExplanation = function(x, ...) {
  UseMethod("generateBadExplanation")
}

#' @describeIn generateBadExplanation
#' Generate plausible but intentionally flawed explanations starting from a
#' good explanation, either supplied directly or stored in a `wmfmModel`
#' object. The generated explanations are returned in a form that can be passed
#' directly to `grade()`.
#'
#' @param explanation Optional character scalar giving the base good
#'   explanation. If `NULL`, `x$explanation` is used.
#' @param type Character vector of bad explanation types, or `"auto"`.
#' @param severity Character scalar. One of `"subtle"`, `"moderate"`, or
#'   `"severe"`.
#' @param n Integer. Number of bad explanations to generate.
#' @param mixTypes Logical. Should individual explanations combine multiple bad
#'   types?
#' @param labelErrors Logical. Should the return value include error labels and
#'   severity metadata?
#' @param provider Optional chat provider. If `NULL`, uses `getChatProvider()`.
#' @param showProgress Logical. Should command-line progress messages be shown?
#'   Defaults to `interactive()`.
#' @param showTiming Logical. Should command-line timing summaries be shown?
#'   Defaults to `interactive()`.
#' @param ... Additional arguments reserved for future use.
#'
#' @return If `n = 1` and `labelErrors = FALSE`, a character scalar.
#'
#'   If `n > 1` and `labelErrors = FALSE`, a named character vector.
#'
#'   If `labelErrors = TRUE`, a named list containing explanations, error type
#'   labels, and severity labels.
#' @export
generateBadExplanation.wmfmModel = function(
    x,
    explanation = NULL,
    type = "auto",
    severity = c("subtle", "moderate", "severe"),
    n = 1,
    mixTypes = FALSE,
    labelErrors = FALSE,
    provider = NULL,
    showProgress = interactive(),
    showTiming = interactive(),
    ...
) {

  validateBadExplanationRequest(
    x = x,
    explanation = explanation,
    type = type,
    severity = severity[1],
    n = n,
    mixTypes = mixTypes,
    labelErrors = labelErrors
  )

  if (!is.logical(showProgress) || length(showProgress) != 1 || is.na(showProgress)) {
    stop("`showProgress` must be TRUE or FALSE.", call. = FALSE)
  }

  if (!is.logical(showTiming) || length(showTiming) != 1 || is.na(showTiming)) {
    stop("`showTiming` must be TRUE or FALSE.", call. = FALSE)
  }

  cliTracker = newWmfmCliStageTracker(
    showProgress = showProgress,
    showTiming = showTiming,
    taskLabel = "Bad explanation generation"
  )

  on.exit(
    finishWmfmCliStageTracker(cliTracker),
    add = TRUE
  )

  severity = match.arg(severity)

  baseExplanation = runWmfmCliStage(
    cliTracker = cliTracker,
    stageLabel = "Resolving base explanation",
    code = function() {
      resolveBadExplanationSource(
        x = x,
        explanation = explanation
      )
    }
  )

  plan = runWmfmCliStage(
    cliTracker = cliTracker,
    stageLabel = "Preparing generation plan",
    code = function() {
      buildBadExplanationPlan(
        x = x,
        type = type,
        severity = severity,
        n = n,
        mixTypes = mixTypes
      )
    }
  )

  chat = runWmfmCliStage(
    cliTracker = cliTracker,
    stageLabel = "Resolving chat provider",
    code = function() {
      provider %||% getChatProvider()
    }
  )

  rawResponse = runWmfmCliStage(
    cliTracker = cliTracker,
    stageLabel = paste0("Generating ", n, " bad explanation", if (n == 1) "" else "s"),
    code = function() {
      generateBadExplanationWithLlm(
        x = x,
        baseExplanation = baseExplanation,
        plan = plan,
        chat = chat,
        showProgress = showProgress
      )
    }
  )

  parsed = runWmfmCliStage(
    cliTracker = cliTracker,
    stageLabel = "Parsing LLM response",
    code = function() {
      parseBadExplanationResponse(rawResponse)
    }
  )

  validated = runWmfmCliStage(
    cliTracker = cliTracker,
    stageLabel = "Validating generated explanations",
    code = function() {
      validateBadExplanationResponse(
        parsed = parsed,
        plan = plan
      )
    }
  )

  out = runWmfmCliStage(
    cliTracker = cliTracker,
    stageLabel = "Formatting output",
    code = function() {
      formatBadExplanationOutput(
        parsed = validated,
        labelErrors = labelErrors
      )
    }
  )

  out
}
