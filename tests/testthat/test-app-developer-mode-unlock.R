testthat::test_that("startup observers wire developer mode unlock and lock controls", {
  startupObserverText = paste(
    deparse(body(registerStartupDataChoiceObservers)),
    collapse = "\n"
  )

  testthat::expect_match(
    startupObserverText,
    "output$developerModeStatus",
    fixed = TRUE
  )
  testthat::expect_match(
    startupObserverText,
    "input$unlockDeveloperModeBtn",
    fixed = TRUE
  )
  testthat::expect_match(
    startupObserverText,
    "verifyDeveloperModePassword(password)",
    fixed = TRUE
  )
  testthat::expect_match(
    startupObserverText,
    "developerModeUnlocked(TRUE)",
    fixed = TRUE
  )
  testthat::expect_match(
    startupObserverText,
    "input$lockDeveloperModeBtn",
    fixed = TRUE
  )
  testthat::expect_match(
    startupObserverText,
    "developerModeUnlocked(FALSE)",
    fixed = TRUE
  )
})
