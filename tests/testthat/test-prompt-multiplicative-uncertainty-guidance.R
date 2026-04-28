testthat::test_that("shared prompt contract cautions about multiplicative intervals crossing 1", {
  contract = buildWmfmLanguageContractText(context = "summary")

  testthat::expect_match(
    contract,
    "If a multiplicative confidence interval includes 1",
    fixed = TRUE
  )
  testthat::expect_match(
    contract,
    "prefer omitting the point estimate unless the research question specifically asks for that comparison",
    fixed = TRUE
  )
  testthat::expect_match(
    contract,
    "the model does not show a clear difference for that comparison",
    fixed = TRUE
  )
})

testthat::test_that("lmToExplanationPrompt includes multiplicative interval caution", {
  dat = data.frame(
    y = c(1, 0, 1, 1, 0, 0, 1, 0),
    group = factor(rep(c("A", "B"), each = 4)),
    x = c(1, 2, 3, 4, 1, 2, 3, 4)
  )

  fit = stats::glm(y ~ group + x, family = stats::binomial(), data = dat)
  attr(fit, "wmfm_research_question") = "How do group and x relate to the odds of y?"
  prompt = suppressWarnings(lmToExplanationPrompt(fit))

  testthat::expect_match(
    prompt,
    "If a multiplicative confidence interval includes 1",
    fixed = TRUE
  )
  testthat::expect_match(
    prompt,
    "weak or uncertain",
    fixed = TRUE
  )
  testthat::expect_match(
    prompt,
    "prefer omitting the point estimate unless the research question specifically asks for that comparison",
    fixed = TRUE
  )
})

testthat::test_that("logistic comparison control suppresses unclear secondary odds-ratio comparisons", {
  dat = data.frame(
    y = c(1, 0, 1, 1, 0, 0, 1, 0),
    group = factor(rep(c("A", "B"), each = 4)),
    x = c(1, 2, 3, 4, 1, 2, 3, 4)
  )

  fit = stats::glm(y ~ group + x, family = stats::binomial(), data = dat)
  promptBlock = suppressWarnings(buildComparisonControlPromptBlock(fit))

  testthat::expect_match(
    promptBlock,
    "If a direct odds-ratio confidence interval includes 1",
    fixed = TRUE
  )
  testthat::expect_match(
    promptBlock,
    "prefer omitting the odds-ratio point estimate",
    fixed = TRUE
  )
  testthat::expect_match(
    promptBlock,
    "the model does not show a clear group difference for that comparison",
    fixed = TRUE
  )
})
