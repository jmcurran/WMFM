testthat::test_that("postProcessExplanationText preserves cleanExplanationText behaviour", {
  testthat::expect_identical(
    postProcessExplanationText("Answer: On average, exam marks increase."),
    "On average, exam marks increase."
  )

  testthat::expect_identical(postProcessExplanationText(NA_character_), NA_character_)
  testthat::expect_null(postProcessExplanationText(NULL))
})


testthat::test_that("postProcessExplanationText standardises unit-change wording", {
  text = "A one-magnitude rise multiplies the expected count by 0.21."

  out = postProcessExplanationText(text)

  testthat::expect_identical(
    out,
    "If the magnitude increases by one, the expected count is multiplied by 0.21."
  )
  testthat::expect_false(grepl("one-magnitude rise", out, ignore.case = TRUE))
  testthat::expect_true(grepl("0.21", out, fixed = TRUE))
})


testthat::test_that("postProcessExplanationText removes verbal fractions", {
  text = paste(
    "The lower count is only one-third of the original value.",
    "The upper count is about three-quarters of the original value."
  )

  out = postProcessExplanationText(text)

  testthat::expect_false(grepl("one-third|three-quarters", out, ignore.case = TRUE))
  testthat::expect_true(grepl("about 33%", out, fixed = TRUE))
  testthat::expect_true(grepl("about 75%", out, fixed = TRUE))
})


testthat::test_that("postProcessExplanationText reduces recurring model-mechanism language", {
  text = paste(
    "The interaction term makes the decrease with magnitude steeper in Washington than in California.",
    "The coefficient is negative in the fitted model."
  )

  out = postProcessExplanationText(text)

  testthat::expect_false(grepl("interaction term", out, ignore.case = TRUE))
  testthat::expect_false(grepl("coefficient", out, ignore.case = TRUE))
  testthat::expect_false(grepl("fitted model", out, ignore.case = TRUE))
  testthat::expect_true(grepl("decrease with magnitude is steeper", out, fixed = TRUE))
})


testthat::test_that("postProcessExplanationText splits supported long sentence patterns", {
  text = "For a student with score 12, the expected mark is 65.2, with values between 60.1 and 70.3."

  out = postProcessExplanationText(text)

  testthat::expect_identical(
    out,
    "For a student with score 12, the expected mark is 65.2. This estimate could plausibly lie between 60.1 and 70.3."
  )
  testthat::expect_true(grepl("12", out, fixed = TRUE))
  testthat::expect_true(grepl("65.2", out, fixed = TRUE))
  testthat::expect_true(grepl("60.1", out, fixed = TRUE))
  testthat::expect_true(grepl("70.3", out, fixed = TRUE))
})


testthat::test_that("postProcessExplanationText rejects invalid inputs", {
  testthat::expect_error(
    postProcessExplanationText(1),
    "`text` must be a character vector or NULL.",
    fixed = TRUE
  )
})
