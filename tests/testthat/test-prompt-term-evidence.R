testthat::test_that("lmToExplanationPrompt includes weak additive term guidance", {
  dat = data.frame(
    Exam = c(42, 46, 45, 49, 57, 61, 59, 63, 41, 45, 44, 48, 56, 60, 58, 62),
    Attend = factor(rep(c("No", "Yes"), each = 4, times = 2)),
    Gender = factor(rep(c("Female", "Male"), each = 8))
  )

  fit = stats::lm(Exam ~ Attend + Gender, data = dat)
  prompt = suppressWarnings(lmToExplanationPrompt(fit))

  testthat::expect_match(prompt, "Term-level evidence guidance for the explanation:", fixed = TRUE)
  testthat::expect_match(prompt, "Attend: clear term-level evidence", fixed = TRUE)
  testthat::expect_match(prompt, "Gender: weak term-level evidence", fixed = TRUE)
  testthat::expect_match(prompt, "For weak additive terms (Gender)", fixed = TRUE)
  testthat::expect_match(prompt, "does not show a clear difference or effect", fixed = TRUE)
  testthat::expect_match(prompt, "do not mention ANOVA, F-tests, or p-values", fixed = TRUE)
})

testthat::test_that("lmToExplanationPrompt includes weak interaction guidance", {
  dat = data.frame(
    Exam = c(42, 46, 45, 49, 57, 61, 59, 63, 41, 45, 44, 48, 58, 62, 60, 64),
    Attend = factor(rep(c("No", "Yes"), each = 4, times = 2)),
    Gender = factor(rep(c("Female", "Male"), each = 8))
  )

  fit = stats::lm(Exam ~ Attend * Gender, data = dat)
  prompt = suppressWarnings(lmToExplanationPrompt(fit))

  testthat::expect_match(prompt, "Term-level evidence guidance for the explanation:", fixed = TRUE)
  testthat::expect_match(prompt, "Attend: clear term-level evidence", fixed = TRUE)
  testthat::expect_match(prompt, "Gender: weak term-level evidence", fixed = TRUE)
  testthat::expect_match(prompt, "Attend:Gender: weak term-level evidence", fixed = TRUE)
  testthat::expect_match(prompt, "For weak interaction terms (Attend:Gender)", fixed = TRUE)
  testthat::expect_match(prompt, "no clear evidence that the effect of one predictor differs across the other predictor", fixed = TRUE)
  testthat::expect_match(prompt, "avoid implying that the difference between those simple comparisons is meaningful", fixed = TRUE)
})
