testthat::test_that("README documents provider setup for new users", {
  readmeText = paste(readLines(testthat::test_path("..", "..", "README.md"), warn = FALSE), collapse = "\n")

  testthat::expect_match(readmeText, "Configuring an AI provider", fixed = TRUE)
  testthat::expect_match(readmeText, "ANTHROPIC_API_KEY", fixed = TRUE)
  testthat::expect_match(readmeText, "OPENAI_API_KEY", fixed = TRUE)
  testthat::expect_match(readmeText, "~/.Renviron", fixed = TRUE)
  testthat::expect_match(readmeText, "API credits", fixed = TRUE)
  testthat::expect_match(readmeText, "ChatGPT Plus subscription", fixed = TRUE)
  testthat::expect_match(readmeText, "Local Ollama", fixed = TRUE)
  testthat::expect_match(readmeText, "WMFM never stores API keys", fixed = TRUE)
  testthat::expect_false(grepl("Stage 20.16", readmeText, fixed = TRUE))
  testthat::expect_false(grepl("WMFM_SHOW_DEVELOPER_MODE", readmeText, fixed = TRUE))
})
