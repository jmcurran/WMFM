testthat::test_that("unit-change request is classified as supported", {
  out = classifyModelFollowupQuestion("Explain this for a 10-unit increase in Test")
  testthat::expect_identical(out$category, "alternative_unit_change")
  testthat::expect_true(out$supported)
})

testthat::test_that("lm transformed effect and interval scale by requested unit change", {
  df = data.frame(Exam = c(42, 58, 81, 86, 35, 72), Test = c(9.1, 13.6, 14.5, 19.1, 8.2, 12.7))
  model = stats::lm(Exam ~ Test, data = df)

  out = computeDeterministicUnitChange(model, "Explain this for a 10-unit increase in Test")
  ci = stats::confint(model)["Test", ]

  testthat::expect_identical(out$status, "ok")
  testthat::expect_equal(out$transformedEffect, out$oneUnitEffect * 10)
  testthat::expect_equal(out$confidenceInterval$transformed$lwr, unname(ci[[1]]) * 10)
  testthat::expect_equal(out$confidenceInterval$transformed$upr, unname(ci[[2]]) * 10)
})

testthat::test_that("negative lm slope preserves direction after unit transform", {
  df = data.frame(Y = c(10, 8, 6, 4, 2), X = c(1, 2, 3, 4, 5))
  model = stats::lm(Y ~ X, data = df)

  out = computeDeterministicUnitChange(model, "Use a 5-unit change in X")

  testthat::expect_identical(out$status, "ok")
  testthat::expect_lt(out$oneUnitEffect, 0)
  testthat::expect_lt(out$transformedEffect, 0)
})

testthat::test_that("ambiguous unit-change request fails safely", {
  df = data.frame(Exam = c(42, 58, 81, 86, 35, 72), Test = c(9.1, 13.6, 14.5, 19.1, 8.2, 12.7))
  model = stats::lm(Exam ~ Test, data = df)

  out = computeDeterministicUnitChange(model, "Interpret this for a unit increase")
  testthat::expect_identical(out$status, "unsupported")
})

testthat::test_that("glm unit-change request fails safely", {
  df = data.frame(Y = c(0, 1, 0, 1, 1, 0), X = c(1, 2, 3, 4, 5, 6))
  model = stats::glm(Y ~ X, data = df, family = stats::binomial())

  out = computeDeterministicUnitChange(model, "Interpret the effect for a 5 unit change in X")
  testthat::expect_identical(out$status, "unsupported")
})

testthat::test_that("prompt includes deterministic alternative-unit payload and forbids recomputation", {
  df = data.frame(Exam = c(42, 58, 81, 86, 35, 72), Test = c(9.1, 13.6, 14.5, 19.1, 8.2, 12.7))
  model = stats::lm(Exam ~ Test, data = df)

  payload = classifyModelFollowupQuestion("Explain this for a 10-unit increase in Test")
  payload = enrichFollowupPayloadWithUnitChange(model, payload)
  attr(model, "wmfm_model_followup_payload") = payload

  prompt = lmToExplanationPrompt(model)
  testthat::expect_match(prompt, "WMFM deterministic alternative-unit payload", fixed = TRUE)
  testthat::expect_match(prompt, "Use these values directly", fixed = TRUE)
  testthat::expect_match(prompt, "Do not recompute, round further, or invent values", fixed = TRUE)
})
