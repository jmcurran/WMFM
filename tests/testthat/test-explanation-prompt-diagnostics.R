testthat::test_that("fit-model server stores explanation prompt diagnostics", {
  path = normalizePath("R/app-server-fit-model.R", winslash = "/", mustWork = TRUE)
  text = paste(readLines(path, warn = FALSE), collapse = "\n")

  testthat::expect_match(text, "rv\\$explanationPromptDiagnostics", perl = TRUE)
  testthat::expect_match(text, "followupPayload = followupClassification", fixed = TRUE)
  testthat::expect_match(text, "assembledPrompt = promptPreview", fixed = TRUE)
  testthat::expect_match(text, "hasPredictionPayloadInPrompt", fixed = TRUE)
  testthat::expect_match(text, "hasSeparateFollowupParagraphInstruction", fixed = TRUE)
})


testthat::test_that("developer mode UI exposes explanation prompt diagnostics panel", {
  path = normalizePath("R/app-server-explanation.R", winslash = "/", mustWork = TRUE)
  text = paste(readLines(path, warn = FALSE), collapse = "\n")

  testthat::expect_match(text, "Explanation prompt diagnostics", fixed = TRUE)
  testthat::expect_match(text, "followupPayload", fixed = TRUE)
  testthat::expect_match(text, "assembledPrompt", fixed = TRUE)
})
