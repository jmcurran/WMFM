resolve_package_file <- function(...) {
  testthat::test_path("..", "..", ...)
}

read_package_source <- function(...) {
  path <- resolve_package_file(...)
  testthat::expect_true(file.exists(path), info = paste("Expected source file to exist:", path))
  paste(readLines(path, warn = FALSE), collapse = "\n")
}

testthat::test_that("fit-model server stores explanation prompt diagnostics", {
  text <- read_package_source("R", "app-server-fit-model.R")

  testthat::expect_match(text, "rv\\$explanationPromptDiagnostics", perl = TRUE)
  testthat::expect_match(text, "followupPayload = followupClassification", fixed = TRUE)
  testthat::expect_match(text, "assembledPrompt = promptPreview", fixed = TRUE)
  testthat::expect_match(text, "hasPredictionPayloadInPrompt", fixed = TRUE)
  testthat::expect_match(text, "hasSeparateFollowupParagraphInstruction", fixed = TRUE)
})


testthat::test_that("developer mode UI exposes explanation prompt diagnostics panel", {
  text <- read_package_source("R", "app-server-explanation.R")

  testthat::expect_match(text, "Explanation prompt diagnostics", fixed = TRUE)
  testthat::expect_match(text, "followupPayload", fixed = TRUE)
  testthat::expect_match(text, "assembledPrompt", fixed = TRUE)
})
