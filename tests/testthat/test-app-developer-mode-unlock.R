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
    "input$developerModeToggle",
    fixed = TRUE
  )
  testthat::expect_match(
    startupObserverText,
    "showModal",
    fixed = TRUE
  )
  testthat::expect_match(
    startupObserverText,
    "confirmDeveloperModeUnlockBtn",
    fixed = TRUE
  )
  testthat::expect_match(
    startupObserverText,
    "cancelDeveloperModeUnlockBtn",
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
    "updateCheckboxInput",
    fixed = TRUE
  )
  testthat::expect_match(
    startupObserverText,
    "developerModeToggle",
    fixed = TRUE
  )
  testthat::expect_match(
    startupObserverText,
    "value = TRUE",
    fixed = TRUE
  )
  testthat::expect_match(
    startupObserverText,
    "buildDeveloperModeUnlockErrorStatus(unlockError)",
    fixed = TRUE
  )
  testthat::expect_match(
    startupObserverText,
    "requestedUnlocked = isTRUE(input$developerModeToggle)",
    fixed = TRUE
  )
  testthat::expect_match(
    startupObserverText,
    "developerModeUnlocked(FALSE)",
    fixed = TRUE
  )
  testthat::expect_false(grepl(
    "showNotification(buildDeveloperModeUnlockedMessage()",
    startupObserverText,
    fixed = TRUE
  ))
})


testthat::test_that("developer mode UI uses a styled toggle rather than lock buttons", {
  uiText = paste(
    deparse(body(appUI)),
    collapse = "\n"
  )

  testthat::expect_match(
    uiText,
    'inputId = "developerModeToggle"',
    fixed = TRUE
  )
  testthat::expect_match(
    uiText,
    "wmfm-developer-mode-toggle-control",
    fixed = TRUE
  )
  testthat::expect_match(
    uiText,
    "background-color: #d9534f",
    fixed = TRUE
  )
  testthat::expect_match(
    uiText,
    "background-color: #2e7d32",
    fixed = TRUE
  )
  testthat::expect_false(grepl(
    "unlockDeveloperModeBtn",
    uiText,
    fixed = TRUE
  ))
  testthat::expect_false(grepl(
    "lockDeveloperModeBtn",
    uiText,
    fixed = TRUE
  ))
})
