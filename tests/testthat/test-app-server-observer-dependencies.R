testthat::test_that("app server observer dependencies are created through a helper", {
  appServerText = readPackageText("R", "app-server.R")
  dependencyText = readPackageText("R", "app-server-observer-dependencies.R")

  testthat::expect_true(grepl("createAppServerObserverDependencies", appServerText, fixed = TRUE))
  testthat::expect_true(grepl("createAppServerObserverDependencies = function", dependencyText, fixed = TRUE))
  testthat::expect_true(grepl("createAppServerStateHelpers", dependencyText, fixed = TRUE))
  testthat::expect_true(grepl("setBucketState = serverStateHelpers$setBucketState", dependencyText, fixed = TRUE))
  testthat::expect_true(grepl("resetModelPage = serverStateHelpers$resetModelPage", dependencyText, fixed = TRUE))
  testthat::expect_true(grepl("applyLoadedExampleToInputs = serverStateHelpers$applyLoadedExampleToInputs", dependencyText, fixed = TRUE))
  testthat::expect_false(grepl("serverStateHelpers = createAppServerStateHelpers", appServerText, fixed = TRUE))
})
