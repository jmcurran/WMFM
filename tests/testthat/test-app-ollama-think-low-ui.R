test_that("settings page includes an Ollama low-thinking switch", {
  uiText = readPackageText("R", "app-ui.R")

  expect_match(uiText, "checkboxInput\\(\\s*inputId = \"ollama_think_low\"", perl = TRUE)
  expect_match(uiText, "Use low thinking effort for Ollama", fixed = TRUE)
  expect_match(uiText, "style = \"margin-bottom: 6px;\"", fixed = TRUE)
  expect_match(uiText, "think = \\\"low\\\"", fixed = TRUE)
})

test_that("app server passes the low-thinking setting to the chat provider", {
  serverText = readPackageText("R", "app-server.R")
  reactiveStateText = readPackageText("R", "app-server-reactive-state.R")
  chatProviderText = readPackageText("R", "app-server-chat-provider.R")
  combinedServerText = paste(serverText, reactiveStateText, chatProviderText, sep = "\n")

  expect_match(combinedServerText, "providerDefaults = wmfmProviderDefaults()", fixed = TRUE)
  expect_match(
    combinedServerText,
    "activeOllamaThinkLow = providerDefaults$ollamaThinkLow",
    fixed = TRUE
  )
  expect_match(
    combinedServerText,
    "rv$activeOllamaThinkLow = isTRUE(input$ollama_think_low)",
    fixed = TRUE
  )
  expect_match(
    combinedServerText,
    "ollamaThinkLow = isTRUE(rv$activeOllamaThinkLow)",
    fixed = TRUE
  )
})

test_that("getChatProvider sends think low through ellmer params", {
  utilsText = readPackageText("R", "utils-llm.R")

  expect_match(utilsText, "ollamaThinkLow", fixed = TRUE)
  expect_match(utilsText, "params\\(think = \"low\"\\)", perl = TRUE)
  expect_match(utilsText, "do.call\\(chat_ollama, ollamaArgs\\)", perl = TRUE)
})
