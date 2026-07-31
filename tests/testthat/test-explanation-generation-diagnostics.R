testthat::test_that("LLM explanation diagnostics preserve each text boundary", {
  data = data.frame(
    outcome = c(1, 2.4, 2.8, 4.3, 5.1),
    testMark = c(10, 20, 30, 40, 50)
  )
  model = stats::lm(outcome ~ testMark, data = data)
  diagnostics = new.env(parent = emptyenv())
  rawText = "For each an increase of one unit in test mark, outcome rises."
  chat = list(
    chat = function(prompt) {
      rawText
    }
  )

  explanation = lmExplanation(
    model = model,
    chat = chat,
    useCache = FALSE,
    diagnostics = diagnostics
  )

  testthat::expect_false(diagnostics$cacheHit)
  testthat::expect_true(diagnostics$llmCalled)
  testthat::expect_true(nzchar(diagnostics$promptText))
  testthat::expect_identical(diagnostics$rawLlmText, rawText)
  testthat::expect_identical(
    diagnostics$normalisedLlmText,
    normaliseNumericExpressions(rawText)
  )
  testthat::expect_identical(
    diagnostics$assembledExplanationText,
    explanation
  )
})


testthat::test_that("cached explanations are identified without inventing raw LLM text", {
  rm(list = ls(envir = .env_cache), envir = .env_cache)
  on.exit(rm(list = ls(envir = .env_cache), envir = .env_cache), add = TRUE)

  data = data.frame(
    outcome = c(1, 2.4, 2.8, 4.3, 5.1),
    testMark = c(10, 20, 30, 40, 50)
  )
  model = stats::lm(outcome ~ testMark, data = data)
  chat = list(chat = function(prompt) "A generated explanation.")

  firstDiagnostics = new.env(parent = emptyenv())
  secondDiagnostics = new.env(parent = emptyenv())

  first = lmExplanation(
    model = model,
    chat = chat,
    useCache = TRUE,
    diagnostics = firstDiagnostics
  )
  second = lmExplanation(
    model = model,
    chat = chat,
    useCache = TRUE,
    diagnostics = secondDiagnostics
  )

  testthat::expect_identical(first, second)
  testthat::expect_false(firstDiagnostics$cacheHit)
  testthat::expect_true(secondDiagnostics$cacheHit)
  testthat::expect_false(secondDiagnostics$llmCalled)
  testthat::expect_null(secondDiagnostics$rawLlmText)
  testthat::expect_identical(
    secondDiagnostics$assembledExplanationText,
    second
  )
})


testthat::test_that("evaluation diagnostics JSON exposes generation boundaries", {
  diagnostics = list(
    explanationGenerationDiagnostics = list(
      cacheHit = FALSE,
      llmCalled = TRUE,
      promptText = "PROMPT",
      rawLlmText = "RAW",
      normalisedLlmText = "NORMALISED",
      assembledExplanationText = "ASSEMBLED",
      postProcessedExplanationText = "POST PROCESSED",
      finalExplanationText = "FINAL"
    ),
    generatedExplanation = "FINAL",
    assembledPrompt = "PROMPT"
  )

  output = jsonlite::fromJSON(
    buildExplanationPromptDiagnosticsJson(diagnostics),
    simplifyVector = FALSE
  )
  generation = output$explanationGeneration

  testthat::expect_false(generation$cacheHit)
  testthat::expect_true(generation$llmCalled)
  testthat::expect_identical(generation$promptText, "PROMPT")
  testthat::expect_identical(generation$rawLlmText, "RAW")
  testthat::expect_identical(generation$normalisedLlmText, "NORMALISED")
  testthat::expect_identical(generation$assembledExplanationText, "ASSEMBLED")
  testthat::expect_identical(generation$postProcessedExplanationText, "POST PROCESSED")
  testthat::expect_identical(generation$finalExplanationText, "FINAL")
})


testthat::test_that("diagnostics JSON tolerates missing explanation-generation diagnostics", {
  diagnostics = list(
    generatedExplanation = "A completed explanation.",
    assembledPrompt = "PROMPT"
  )

  output = jsonlite::fromJSON(
    buildExplanationPromptDiagnosticsJson(diagnostics),
    simplifyVector = FALSE
  )

  testthat::expect_false(output$explanationGeneration$cacheHit)
  testthat::expect_false(output$explanationGeneration$llmCalled)
  testthat::expect_identical(output$explanationGeneration$promptText, "")
  testthat::expect_identical(output$explanationGeneration$rawLlmText, "")
  testthat::expect_identical(output$explanationGeneration$finalExplanationText, "")
})
