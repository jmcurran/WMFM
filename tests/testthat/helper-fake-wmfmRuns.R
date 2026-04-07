makeFakeWmfmRunRecord = function(
    runId = 1L,
    explanationText = paste(
      "Attendance is associated with higher exam scores.",
      "Compared with the baseline, scores increase by about 3.5 points."
    ),
    equationsText = "Exam = 6.62 + 3.52 * Attend",
    formula = "Exam ~ Attend",
    exampleName = "Course",
    package = "WMFM",
    modelType = "lm",
    errorMessage = NA_character_,
    interactionTerms = character(0),
    interactionMinPValue = NA_real_,
    interactionAlpha = 0.05,
    runElapsedSeconds = 0.25,
    overrides = list()
) {
  out = buildWmfmRunRecord(
    runId = runId,
    exampleName = exampleName,
    package = package,
    modelType = modelType,
    formula = formula,
    equationsText = equationsText,
    explanationText = explanationText,
    errorMessage = errorMessage,
    interactionTerms = interactionTerms,
    interactionMinPValue = interactionMinPValue,
    interactionAlpha = interactionAlpha
  )

  out$runElapsedSeconds = runElapsedSeconds

  if (length(overrides) > 0) {
    out = utils::modifyList(out, overrides)
  }

  out
}

makeFakeWmfmRuns = function(
    runs = NULL,
    nRuns = 3L,
    explanations = NULL,
    package = "WMFM",
    exampleName = "Course",
    formula = "Exam ~ Attend",
    modelType = "lm"
) {
  if (is.null(runs)) {
    if (is.null(explanations)) {
      explanations = c(
        "Attendance is associated with higher exam scores.",
        "Higher attendance predicts about 3.5 more exam points.",
        "The data suggest exam scores are higher with attendance."
      )
    }

    nRuns = length(explanations)

    runs = lapply(seq_len(nRuns), function(i) {
      makeFakeWmfmRunRecord(
        runId = i,
        explanationText = explanations[[i]],
        formula = formula,
        exampleName = exampleName,
        package = package,
        modelType = modelType,
        runElapsedSeconds = i / 10
      )
    })
  }

  out = list(
    exampleName = exampleName,
    package = package,
    spec = list(
      formula = formula,
      modelType = modelType
    ),
    dataContext = "Fake example context for tests.",
    runs = runs,
    meta = list(
      nRuns = length(runs),
      createdAt = as.character(Sys.time()),
      startedAt = as.character(Sys.time()),
      elapsedSeconds = sum(vapply(runs, function(x) {
        as.numeric(x$runElapsedSeconds %||% 0)
      }, numeric(1))),
      averageRunSeconds = mean(vapply(runs, function(x) {
        as.numeric(x$runElapsedSeconds %||% 0)
      }, numeric(1))),
      runSeconds = vapply(runs, function(x) {
        as.numeric(x$runElapsedSeconds %||% 0)
      }, numeric(1)),
      useExplanationCache = FALSE,
      interactionAlpha = 0.05
    )
  )

  class(out) = c("wmfmRuns", "list")
  out
}
