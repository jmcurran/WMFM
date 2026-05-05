test_that("startup observer registration is extracted from app server", {
  serverText = readPackageText("R", "app-server.R")
  startupObserverText = readPackageText("R", "app-server-startup.R")

  expect_match(serverText, "registerStartupDataChoiceObservers", fixed = TRUE)
  expect_match(startupObserverText, "registerStartupDataChoiceObservers = function", fixed = TRUE)
  expect_match(startupObserverText, "developerModeUnlocked = reactiveVal", fixed = TRUE)
  expect_match(startupObserverText, "exampleLoadStatus = reactiveVal", fixed = TRUE)
  expect_false(grepl("startupNotificationId = \"wmfm-startup-data-choices\"", serverText, fixed = TRUE))
})
