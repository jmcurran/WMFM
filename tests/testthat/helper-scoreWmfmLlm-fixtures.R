makeRawRunRecordForScoring = function(
    explanationText = paste(
      "Attendance is associated with higher exam scores.",
      "Compared with the baseline, scores increase by about 3.5 points."
    ),
    hasError = FALSE,
    interactionTerms = character(0),
    interactionMinPValue = NA_real_,
    overrides = list()
) {
  out = makeFakeWmfmRunRecord(
    runId = 1L,
    explanationText = explanationText,
    equationsText = "Exam = 6.62 + 3.52 * Attend",
    formula = "Exam ~ Attend",
    exampleName = "Course",
    package = "WMFM",
    modelType = "lm",
    errorMessage = if (isTRUE(hasError)) {
      "Synthetic test error"
    } else {
      NA_character_
    },
    interactionTerms = interactionTerms,
    interactionMinPValue = interactionMinPValue,
    overrides = overrides
  )

  out
}

makeFakeChat = function(rawResponse) {
  structure(
    list(
      chat = function(prompt) {
        rawResponse
      }
    ),
    class = "ProviderFake"
  )
}

makeCountingFakeChat = function(rawResponse) {
  counter = new.env(parent = emptyenv())
  counter$n = 0L

  provider = structure(
    list(
      chat = function(prompt) {
        counter$n = counter$n + 1L
        rawResponse
      }
    ),
    class = "ProviderFake"
  )

  attr(provider, "counter") = counter
  provider
}

getFakeChatCallCount = function(chat) {
  attr(chat, "counter")$n
}
