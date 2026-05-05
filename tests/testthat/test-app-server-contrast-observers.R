test_that("contrast observer registration is extracted from app server", {
  appServerText = readPackageText("R", "app-server.R")
  contrastObserverText = readPackageText("R", "app-server-contrasts.R")

  expect_true(grepl("registerContrastObservers", appServerText, fixed = TRUE))
  expect_true(grepl("registerContrastObservers = function", contrastObserverText, fixed = TRUE))
  expect_true(grepl("rv = rv", appServerText, fixed = TRUE))
  expect_true(grepl("setBucketState = setBucketState", appServerText, fixed = TRUE))
  expect_true(grepl("output$contrasts_content_ui", contrastObserverText, fixed = TRUE))
  expect_true(grepl("output$contrastUi", contrastObserverText, fixed = TRUE))
  expect_true(grepl("output$contrastResult", contrastObserverText, fixed = TRUE))
  expect_false(grepl("output$contrastUi = renderUI", appServerText, fixed = TRUE))
})
