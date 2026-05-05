test_that("developer mode status helper reports locked and unlocked states", {
  expect_equal(
    buildDeveloperModeStatus(TRUE),
    "Developer mode is unlocked."
  )

  expect_equal(
    buildDeveloperModeStatus(FALSE),
    "Developer mode is locked."
  )

  expect_equal(
    buildDeveloperModeStatus(NULL),
    "Developer mode is locked."
  )
})

test_that("developer mode notification helpers centralise user-facing text", {
  expect_equal(
    buildDeveloperModeIncorrectPasswordMessage(),
    "Incorrect password. Developer mode remains locked."
  )

  expect_equal(
    buildDeveloperModeUnlockedMessage(),
    "Developer mode unlocked."
  )

  expect_equal(
    buildDeveloperModeLockedMessage(),
    "Developer mode locked."
  )
})
