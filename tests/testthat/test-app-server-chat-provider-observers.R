test_that("chat provider observer registration is extracted from app server", {
  appServerText = readPackageText("R", "app-server.R")
  chatProviderText = readPackageText("R", "app-server-chat-provider.R")

  expect_match(appServerText, "registerChatProviderObservers", fixed = TRUE)
  expect_false(grepl("refreshOllamaModelChoices = function", appServerText, fixed = TRUE))

  expect_match(chatProviderText, "registerChatProviderObservers = function", fixed = TRUE)
  expect_match(chatProviderText, "refreshOllamaModelChoices = function", fixed = TRUE)
  expect_false(grepl("verifyProviderSwitchPassword", chatProviderText, fixed = TRUE))
  expect_match(chatProviderText, "saveProviderConfigBtn", fixed = TRUE)
  expect_false(grepl("buildClaudeProviderIncorrectPasswordMessage", chatProviderText, fixed = TRUE))
  expect_false(grepl("observeEvent(input$applyChatProviderBtn", chatProviderText, fixed = TRUE))
  expect_false(grepl("Cannot apply provider: required credentials are missing.", chatProviderText, fixed = TRUE))
  expect_match(chatProviderText, "showProviderConfigurationMessage", fixed = TRUE)
  expect_match(chatProviderText, "saveNonSecretProviderConfig(providerConfig)", fixed = TRUE)
})


test_that("provider config save no longer depends on Claude password verification", {
  chatProviderText = readPackageText("R", "app-server-chat-provider.R")

  saveBlockMatch = regexpr(
    'observeEvent\\(input\\$saveProviderConfigBtn, \\{[\\s\\S]*?\\n  \\}, ignoreInit = TRUE\\)',
    chatProviderText,
    perl = TRUE
  )
  expect_gt(as.integer(saveBlockMatch), 0)

  saveBlock = regmatches(chatProviderText, saveBlockMatch)
  writePos = regexpr('saveNonSecretProviderConfig\\(configToSave\\)', saveBlock, perl = TRUE)[1]
  missingStatusPos = regexpr('Provider config was not saved because required credentials are missing.', saveBlock, fixed = TRUE)[1]

  expect_gt(writePos, 0)
  expect_gt(missingStatusPos, 0)
  expect_lt(missingStatusPos, writePos)
  expect_false(grepl("verifyProviderSwitchPassword", saveBlock, fixed = TRUE))
  expect_false(grepl("providerSwitchPassword", saveBlock, fixed = TRUE))
})

test_that("Ollama model refresh is capability-aware and keeps failure fallback", {
  chatProviderText = readPackageText("R", "app-server-chat-provider.R")

  expect_match(chatProviderText, "resolveSelectedProvider = function", fixed = TRUE)
  expect_match(chatProviderText, "if (!identical(activeProvider, \"ollama\"))", fixed = TRUE)
  expect_match(chatProviderText, "isWmfmProviderReadyForStartup", fixed = TRUE)
  expect_match(chatProviderText, "Configure an AI provider", fixed = TRUE)
  expect_match(chatProviderText, "Model discovery is only available for Ollama.", fixed = TRUE)
  expect_match(chatProviderText, "Using current/default choices", fixed = TRUE)
  expect_match(chatProviderText, "fallback = rv$availableOllamaModels", fixed = TRUE)
})

test_that("Ollama model discovery is not attempted by the startup observer", {
  chatProviderText = readPackageText("R", "app-server-chat-provider.R")

  startupObserverMatch = regexpr(
    'observe\\(\\{\\n    selectedProvider = resolveSelectedProvider\\(\\)[\\s\\S]*?\\n  \\}\\)',
    chatProviderText,
    perl = TRUE
  )

  expect_gt(as.integer(startupObserverMatch), 0)
  startupObserver = regmatches(chatProviderText, startupObserverMatch)

  expect_match(startupObserver, "syncProviderSpecificControlState(selectedProvider)", fixed = TRUE)
  expect_false(grepl("refreshOllamaModelChoices", startupObserver, fixed = TRUE))
  expect_false(grepl("models_ollama", startupObserver, fixed = TRUE))
})
