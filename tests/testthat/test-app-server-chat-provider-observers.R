test_that("chat provider observer registration is extracted from app server", {
  appServerText = readPackageText("R", "app-server.R")
  chatProviderText = readPackageText("R", "app-server-chat-provider.R")

  expect_match(appServerText, "registerChatProviderObservers", fixed = TRUE)
  expect_false(grepl("refreshOllamaModelChoices = function", appServerText, fixed = TRUE))

  expect_match(chatProviderText, "registerChatProviderObservers = function", fixed = TRUE)
  expect_match(chatProviderText, "refreshOllamaModelChoices = function", fixed = TRUE)
  expect_match(chatProviderText, "verifyProviderSwitchPassword", fixed = TRUE)
  expect_match(chatProviderText, "buildChatProviderSetMessage", fixed = TRUE)
})
