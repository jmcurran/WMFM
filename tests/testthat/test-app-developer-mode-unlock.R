testthat::test_that("developer mode status helpers distinguish locked and unlocked states", {
  testthat::expect_identical(
    buildDeveloperModeStatus(FALSE),
    "Developer mode is locked."
  )
  testthat::expect_identical(
    buildDeveloperModeStatus(TRUE),
    "Developer mode is unlocked."
  )
})

testthat::test_that("developer mode UI is hidden unless explicitly enabled", {
  withr::local_envvar(list(WMFM_SHOW_DEVELOPER_MODE = ""), .local_envir = parent.frame())
  html = as.character(appUI())

  testthat::expect_false(grepl("Developer mode:", html, fixed = TRUE))
  testthat::expect_false(grepl("developerModeToggle", html, fixed = TRUE))
  testthat::expect_match(html, "Provider settings", fixed = TRUE)
})

testthat::test_that("developer mode UI uses an opt-in styled toggle", {
  withr::local_envvar(list(WMFM_SHOW_DEVELOPER_MODE = "1"), .local_envir = parent.frame())

  uiText = paste(
    deparse(body(appUI)),
    collapse = "\n"
  )
  html = as.character(appUI())

  testthat::expect_match(
    uiText,
    "isDeveloperModeUiEnabled()",
    fixed = TRUE
  )
  testthat::expect_match(
    html,
    "developerModeToggle",
    fixed = TRUE
  )
  testthat::expect_match(
    html,
    "wmfm-developer-mode-toggle-row",
    fixed = TRUE
  )
  testthat::expect_match(
    html,
    "wmfm-developer-mode-switch",
    fixed = TRUE
  )
  testthat::expect_match(
    html,
    "wmfm-developer-mode-slider",
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
    "checkboxInput(inputId = \"developerModeToggle\"",
    uiText,
    fixed = TRUE
  ))
  testthat::expect_false(grepl(
    "Developer mode exposes diagnostic controls",
    html,
    fixed = TRUE
  ))
})

testthat::test_that("startup observers wire password-protected developer toggle controls", {
  startupObserverText = paste(
    deparse(body(registerStartupDataChoiceObservers)),
    collapse = "\n"
  )

  testthat::expect_match(
    startupObserverText,
    "isDeveloperModeUiEnabled()",
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
    "showModal(modalDialog",
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
    "saveDeveloperModePreference(TRUE)",
    fixed = TRUE
  )
  testthat::expect_match(
    startupObserverText,
    "developerModeUnlocked(FALSE)",
    fixed = TRUE
  )
  testthat::expect_match(
    startupObserverText,
    "saveDeveloperModePreference(FALSE)",
    fixed = TRUE
  )
  testthat::expect_false(grepl(
    "buildDeveloperModeUnlockedMessage()",
    startupObserverText,
    fixed = TRUE
  ))
})
