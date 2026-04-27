test_that("buildAppOutputMessages uses deterministic wording by default", {

  out = buildAppOutputMessages(
    equationMethod = "deterministic",
    explanationAvailable = FALSE,
    explanationRequested = TRUE
  )

  expect_identical(out$progressMessage, "Building fitted-model outputs...")
  expect_identical(out$equationDetail, "Building deterministic equations...")
  expect_identical(out$equationCompleteDetail, "Deterministic equations built.")
  expect_identical(out$explanationDetail, "Generating narrative explanation...")
  expect_identical(out$updateDetail, "Updating app...")
  expect_identical(out$finishDetail, "Explanation unavailable. Finishing...")
  expect_identical(out$doneDetail, "Done.")
  expect_null(out$fallbackNotification)
})

test_that("buildAppOutputMessages supports no-explanation wording", {

  out = buildAppOutputMessages(
    equationMethod = "deterministic",
    explanationAvailable = FALSE,
    explanationRequested = FALSE
  )

  expect_identical(out$explanationDetail, "Skipping narrative explanation...")
  expect_identical(out$finishDetail, "No explanation requested. Finishing...")
})

test_that("buildAppOutputMessages keeps llm wording only for explicit llm requests", {

  out = buildAppOutputMessages(
    equationMethod = "llm",
    explanationAvailable = TRUE,
    explanationRequested = TRUE
  )

  expect_identical(
    out$equationDetail,
    "Requesting equations from the language model..."
  )
  expect_identical(out$equationCompleteDetail, "Equation response received.")
  expect_identical(out$explanationDetail, "Generating narrative explanation...")
  expect_identical(out$finishDetail, "Explanation received. Finishing...")
  expect_match(out$fallbackNotification, "switched to deterministic equations")
})
