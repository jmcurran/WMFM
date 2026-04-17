test_that("buildAppOutputMessages uses deterministic wording by default", {

  out = buildAppOutputMessages(
    equationMethod = "deterministic",
    explanationAvailable = FALSE
  )

  expect_identical(out$progressMessage, "Building fitted-model outputs...")
  expect_identical(out$equationDetail, "Building deterministic equations...")
  expect_identical(out$updateDetail, "Updating app...")
  expect_identical(out$finishDetail, "Explanation unavailable. Finishing...")
  expect_identical(out$doneDetail, "Done.")
  expect_null(out$fallbackNotification)
})

test_that("buildAppOutputMessages keeps llm wording only for explicit llm requests", {

  out = buildAppOutputMessages(
    equationMethod = "llm",
    explanationAvailable = TRUE
  )

  expect_identical(
    out$equationDetail,
    "Requesting equations from the language model..."
  )
  expect_identical(out$finishDetail, "Explanation received. Finishing...")
  expect_match(out$fallbackNotification, "switched to deterministic equations")
})
