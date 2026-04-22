# Helper fixtures for LLM scoring tests

# ------------------------------------------------------------------
# Fake chat provider that returns valid scoring JSON
# ------------------------------------------------------------------

makeFakeChat = function(responseText = NULL) {

  if (is.null(responseText)) {
    responseText = paste0(
      "{",
      "\"effectDirectionCorrect\": 2,",
      "\"effectScaleAppropriate\": 2,",
      "\"numericExpressionAdequate\": 2,",
      "\"uncertaintyExpressionAdequate\": 2,",
      "\"causalLanguageAppropriate\": 2,",
      "\"overallScore\": 100",
      "}"
    )
  }

  structure(
    list(
      chat = function(...) {
        responseText
      }
    ),
    class = "ProviderFake"   # IMPORTANT: matches expected test value
  )
}

# ------------------------------------------------------------------
# Fake raw run record consistent with current WMFM schema
# ------------------------------------------------------------------

makeRawRunRecordForScoring = function(
    hasError = FALSE,
    interactionTerms = character(),
    interactionMinPValue = NA_real_
) {

  hasInteractionTerms = length(interactionTerms) > 0

  list(
    runId = 1L,

    # --- Required metadata ---
    exampleName = "test_example",
    package = "WMFM",
    modelType = "lm",

    # --- Model specification ---
    formula = "Exam ~ Attend + Test",
    responseVar = "Exam",
    predictorVars = c("Attend", "Test"),

    # --- Model outputs ---
    equationsText = "Exam = 6.62 + 8.01 * AttendYes + 3.52 * Test",
    explanationText = "Higher test scores are associated with higher exam marks.",

    # --- Interaction metadata ---
    hasInteractionTerms = hasInteractionTerms,
    nInteractionTerms = length(interactionTerms),
    interactionTerms = interactionTerms,
    interactionMinPValue = interactionMinPValue,
    interactionAlpha = 0.05,

    # --- Error handling ---
    hasError = hasError,
    errorMessage = if (hasError) "example error" else NULL
  )
}
