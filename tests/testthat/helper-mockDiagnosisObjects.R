makeMockScores = function(detValues,
                          llmValues,
                          metric = "numericExpressionAdequate") {
  stopifnot(length(detValues) == length(llmValues))

  nRuns = length(detValues)

  structure(
    list(
      scores = list(
        deterministic = lapply(seq_len(nRuns), function(i) {
          stats = list()
          stats[[metric]] = detValues[[i]]
          stats
        }),
        llm = lapply(seq_len(nRuns), function(i) {
          stats = list()
          stats[[metric]] = llmValues[[i]]
          stats
        })
      )
    ),
    class = "wmfmScores"
  )
}

makeMockRuns = function(explanations = NULL,
                        effectScaleClaim = NULL,
                        percentLanguageMention = NULL,
                        comparisonLanguageMention = NULL,
                        conditionalLanguageMention = NULL,
                        referenceGroupMention = NULL,
                        uncertaintyMention = NULL) {
  nRuns = length(explanations)

  out = data.frame(
    runId = seq_len(nRuns),
    explanationText = explanations,
    stringsAsFactors = FALSE
  )

  if (!is.null(effectScaleClaim)) {
    out$effectScaleClaim = effectScaleClaim
  }

  if (!is.null(percentLanguageMention)) {
    out$percentLanguageMention = percentLanguageMention
  }

  if (!is.null(comparisonLanguageMention)) {
    out$comparisonLanguageMention = comparisonLanguageMention
  }

  if (!is.null(conditionalLanguageMention)) {
    out$conditionalLanguageMention = conditionalLanguageMention
  }

  if (!is.null(referenceGroupMention)) {
    out$referenceGroupMention = referenceGroupMention
  }

  if (!is.null(uncertaintyMention)) {
    out$uncertaintyMention = uncertaintyMention
  }

  out
}
