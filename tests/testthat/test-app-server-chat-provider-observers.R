test_that("chat provider observer registration is extracted from app server", {
  appServerText = readPackageText("R", "app-server.R")
  chatProviderText = readPackageText("R", "app-server-chat-provider.R")

  expect_match(appServerText, "registerChatProviderObservers", fixed = TRUE)
  expect_false(grepl("refreshOllamaModelChoices = function", appServerText, fixed = TRUE))

  expect_match(chatProviderText, "registerChatProviderObservers = function", fixed = TRUE)
  expect_match(chatProviderText, "refreshOllamaModelChoices = function", fixed = TRUE)
  expect_match(chatProviderText, "verifyProviderSwitchPassword", fixed = TRUE)
  expect_match(chatProviderText, "saveProviderConfigBtn", fixed = TRUE)
  expect_match(chatProviderText, "buildClaudeProviderIncorrectPasswordMessage", fixed = TRUE)
  expect_match(chatProviderText, "buildChatProviderSetMessage", fixed = TRUE)
})


test_that("Claude save verifies password before writing provider config", {
  chatProviderText = readPackageText("R", "app-server-chat-provider.R")

  saveBlockMatch = regexpr(
    'observeEvent\(input\$saveProviderConfigBtn, \{[\s\S]*?\n  \}, ignoreInit = TRUE\)',
    chatProviderText,
    perl = TRUE
  )
  expect_gt(as.integer(saveBlockMatch), 0)

  saveBlock = regmatches(chatProviderText, saveBlockMatch)
  verifyPos = regexpr('verifyProviderSwitchPassword', saveBlock, fixed = TRUE)[1]
  writePos = regexpr('saveNonSecretProviderConfig\(configToSave\)', saveBlock, perl = TRUE)[1]
  failStatusPos = regexpr('Provider config was not saved because Claude password verification failed\.', saveBlock, fixed = TRUE)[1]

  expect_gt(verifyPos, 0)
  expect_gt(writePos, 0)
  expect_gt(failStatusPos, 0)
  expect_lt(verifyPos, writePos)
  expect_lt(failStatusPos, writePos)
})
