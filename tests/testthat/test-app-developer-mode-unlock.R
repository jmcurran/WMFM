testthat::test_that("developer mode status helpers distinguish locked, unlocked, and unlock errors", {
  testthat::expect_identical(
    buildDeveloperModeStatus(FALSE),
    "Developer mode is locked."
  )
  testthat::expect_identical(
    buildDeveloperModeStatus(TRUE),
    "Developer mode is unlocked."
  )
  testthat::expect_identical(
    buildDeveloperModeUnlockErrorStatus("WMFM_DEVELOPER_MODE_PASSWORD_HASH is not set."),
    "Developer mode is locked. WMFM_DEVELOPER_MODE_PASSWORD_HASH is not set."
  )
})

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
    "output$developerModeSessionState",
    fixed = TRUE
  )
  testthat::expect_match(
    startupObserverText,
    "session$userData$developerModeUnlocked = developerModeUnlocked",
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
    "developerModeStatus(buildDeveloperModeStatus(TRUE))",
    fixed = TRUE
  )
  testthat::expect_match(
    startupObserverText,
    "buildDeveloperModeUnlockErrorStatus(unlockError)",
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
