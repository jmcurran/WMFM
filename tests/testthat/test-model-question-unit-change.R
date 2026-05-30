testthat::test_that("lm unit-change payload scales slope and confidence interval", {
  df = data.frame(
    Price = c(100, 132, 159, 191, 218, 252),
    Carat = c(0.20, 0.25, 0.30, 0.35, 0.40, 0.45)
  )
  model = stats::lm(Price ~ Carat, data = df)
  payload = classifyModelFollowupQuestion("Explain the Carat effect for a 0.1-unit increase")
  payload = enrichFollowupPayloadWithUnitChange(model = model, followupPayload = payload)

  refCoef = unname(stats::coef(model)[["Carat"]])
  refCi = stats::confint(model, parm = "Carat")

  testthat::expect_identical(payload$category, "unit_change_request")
  testthat::expect_true(payload$requiresDeterministicComputation)
  testthat::expect_identical(payload$unitChangeResult$status, "ok")
  testthat::expect_equal(payload$unitChangeResult$requestedUnitChange, 0.1)
  testthat::expect_equal(payload$unitChangeResult$oneUnitEffect, refCoef)
  testthat::expect_equal(payload$unitChangeResult$transformedEstimate, refCoef * 0.1)
  testthat::expect_equal(payload$unitChangeResult$confidenceInterval$lwr, unname(refCi[1, 1]) * 0.1)
  testthat::expect_equal(payload$unitChangeResult$confidenceInterval$upr, unname(refCi[1, 2]) * 0.1)
})

testthat::test_that("unit-change payload asks for clarification when multiple numeric predictors are unnamed", {
  df = data.frame(
    Y = c(1, 3, 4, 7, 9, 12),
    X1 = c(1, 2, 3, 4, 5, 6),
    X2 = c(2, 1, 4, 3, 6, 5)
  )
  model = stats::lm(Y ~ X1 + X2, data = df)
  payload = classifyModelFollowupQuestion("Explain this for a 2-unit increase")
  payload = enrichFollowupPayloadWithUnitChange(model = model, followupPayload = payload)

  testthat::expect_identical(payload$unitChangeResult$status, "clarification_required")
  testthat::expect_identical(payload$unitChangeResult$reason, "ambiguous_predictor")
  testthat::expect_equal(payload$unitChangeResult$candidatePredictors, c("X1", "X2"))
})

testthat::test_that("unit-change prompt includes deterministic payload and avoids prediction paragraph instruction", {
  df = data.frame(
    Price = c(100, 132, 159, 191, 218, 252),
    Carat = c(0.20, 0.25, 0.30, 0.35, 0.40, 0.45)
  )
  model = stats::lm(Price ~ Carat, data = df)
  payload = classifyModelFollowupQuestion("Explain the Carat effect for a 0.1-unit increase")
  payload = enrichFollowupPayloadWithUnitChange(model = model, followupPayload = payload)
  attr(model, "wmfm_model_followup_payload") = payload
  attr(model, "wmfm_model_followup_question") = payload$originalText

  prompt = lmToExplanationPrompt(model)

  testthat::expect_match(prompt, "WMFM deterministic requested unit-change interpretation", fixed = TRUE)
  testthat::expect_match(prompt, "Requested unit change: 0.1", fixed = TRUE)
  testthat::expect_match(prompt, "Requested predictor: Carat", fixed = TRUE)
  testthat::expect_match(prompt, "Do not also append a separate prediction-style follow-up paragraph", fixed = TRUE)
  testthat::expect_no_match(prompt, "WMFM deterministic prediction payload", fixed = TRUE)
})

testthat::test_that("Poisson unit-change payload scales on link scale and exponentiates", {
  df = data.frame(
    Y = c(1, 2, 4, 7, 12, 20),
    x = c(0, 1, 2, 3, 4, 5)
  )
  model = stats::glm(Y ~ x, data = df, family = stats::poisson())
  payload = classifyModelFollowupQuestion("Explain x for a 2-unit increase")
  payload = enrichFollowupPayloadWithUnitChange(model = model, followupPayload = payload)
  coefX = unname(stats::coef(model)[["x"]])

  testthat::expect_identical(payload$unitChangeResult$status, "ok")
  testthat::expect_identical(payload$unitChangeResult$modelType, "glm")
  testthat::expect_identical(payload$unitChangeResult$effectScale, "expected_count_multiplier")
  testthat::expect_equal(payload$unitChangeResult$transformedEstimate, exp(coefX * 2))
})
