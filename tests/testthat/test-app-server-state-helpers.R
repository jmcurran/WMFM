test_that("app server state helpers are extracted from app server", {
  appServerText = readPackageText("R", "app-server.R")
  observerDependencyText = readPackageText("R", "app-server-observer-dependencies.R")
  stateHelperText = readPackageText("R", "app-server-state-helpers.R")

  expect_match(observerDependencyText, "createAppServerStateHelpers", fixed = TRUE)
  expect_match(appServerText, "createAppServerObserverDependencies", fixed = TRUE)
  expect_false(grepl("setBucketState = function", appServerText, fixed = TRUE))
  expect_false(grepl("resetModelPage = function", appServerText, fixed = TRUE))
  expect_false(grepl("applyLoadedExampleToInputs = function", appServerText, fixed = TRUE))

  expect_match(stateHelperText, "createAppServerStateHelpers = function", fixed = TRUE)
  expect_match(stateHelperText, "setBucketState = function", fixed = TRUE)
  expect_match(stateHelperText, "resetModelPage = function", fixed = TRUE)
  expect_match(stateHelperText, "applyLoadedExampleToInputs = function", fixed = TRUE)
})
