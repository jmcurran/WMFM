makeFakeWmfmScores = function(longDf, methods = unique(longDf$method)) {
  methods = as.character(methods)
  runIds = sort(unique(longDf$runId))

  scores = setNames(vector("list", length(methods)), methods)

  for (method in methods) {
    methodDf = longDf[longDf$method == method, , drop = FALSE]
    methodScores = vector("list", length(runIds))

    for (i in seq_along(runIds)) {
      runId = runIds[i]
      rowDf = methodDf[methodDf$runId == runId, , drop = FALSE]

      if (nrow(rowDf) == 0) {
        methodScores[[i]] = NULL
      } else {
        rowDf = rowDf[1, setdiff(names(rowDf), c("runId", "method")), drop = FALSE]
        rownames(rowDf) = NULL
        methodScores[[i]] = rowDf
      }
    }

    scores[[method]] = methodScores
  }

  structure(
    list(
      runIds = runIds,
      methods = methods,
      scores = scores,
      meta = list(
        createdAt = as.character(Sys.time()),
        sourceClass = "fakeWmfmRuns",
        nRuns = length(runIds)
      )
    ),
    class = c("wmfmScores", "list")
  )
}

makeDefaultRegistry = function() {
  getWmfmMetricRegistry()
}

makeMetricRow = function(metricName) {
  registry = getWmfmMetricRegistry()
  registry[registry$metricName == metricName, , drop = FALSE]
}

makeLongScoreDf = function() {
  data.frame(
    runId = c(1, 2, 3, 1, 2, 3),
    method = c(
      "deterministic", "deterministic", "deterministic",
      "llm", "llm", "llm"
    ),
    overallScore = c(1.0, 2.0, 3.0, 1.2, 1.8, 3.2),
    clarityAdequate = c(
      "adequate", "mixed_or_unclear", "inadequate",
      "adequate", "adequate", "mixed_or_unclear"
    ),
    effectDirectionCorrect = c(
      "correct", "mixed_or_unclear", "incorrect",
      "correct", "incorrect", "incorrect"
    ),
    interactionDirectionCorrect = c(
      "correct", "mixed_or_unclear", "incorrect",
      "correct", "mixed_or_unclear", "incorrect"
    ),
    uncertaintyAppropriate = c(TRUE, FALSE, TRUE, TRUE, TRUE, FALSE),
    stringsAsFactors = FALSE
  )
}
