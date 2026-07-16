test_that("post-fit Quarto sections follow the agreed analysis order", {
  modelData = data.frame(outcome = c(2, 4, 6, 8), predictor = 1:4)
  recipe = buildAnalysisRecipeFromFit(
    lm(outcome ~ predictor, data = modelData),
    dataSource = "package",
    packageName = "examplePackage",
    datasetName = "modelData"
  )

  lines = renderAnalysisRecipeCoreQuarto(recipe)

  expect_true(match("# Model summary", lines) < match("# Analysis of variance", lines))
  expect_true(match("# Analysis of variance", lines) < match("# Confidence intervals", lines))
  expect_true(match("# Confidence intervals", lines) < match("# Diagnostic plots", lines))
  expect_true(match("# Diagnostic plots", lines) < match("# Model plot", lines))
})

test_that("summary and ANOVA rendering use authoritative model methods", {
  linearData = data.frame(outcome = c(2, 4, 5, 8), predictor = 1:4)
  binomialData = data.frame(
    outcome = c(0, 1, 0, 1, 0, 1, 1, 0),
    predictor = 1:8
  )

  linearRecipe = buildAnalysisRecipeFromFit(
    lm(outcome ~ predictor, data = linearData),
    dataSource = "package",
    packageName = "examplePackage",
    datasetName = "linearData"
  )
  logisticRecipe = buildAnalysisRecipeFromFit(
    glm(outcome ~ predictor, data = binomialData, family = binomial()),
    dataSource = "package",
    packageName = "examplePackage",
    datasetName = "binomialData"
  )

  expect_match(paste(renderAnalysisRecipeSummaryChunk(linearRecipe), collapse = "\n"), "summary(modelFit)", fixed = TRUE)
  expect_match(paste(renderAnalysisRecipeAnovaChunk(linearRecipe), collapse = "\n"), "anova(modelFit)", fixed = TRUE)
  expect_match(paste(renderAnalysisRecipeAnovaChunk(logisticRecipe), collapse = "\n"), 'anova(modelFit, test = "Chisq")', fixed = TRUE)
})

test_that("confidence interval rendering uses the WMFM interval pathway", {
  modelData = data.frame(outcome = c(2, 4, 5, 8), predictor = 1:4)
  recipe = buildAnalysisRecipeFromFit(
    lm(outcome ~ predictor, data = modelData),
    dataSource = "package",
    packageName = "examplePackage",
    datasetName = "modelData"
  )

  rendered = paste(renderAnalysisRecipeConfidenceIntervalChunk(recipe), collapse = "\n")

  expect_match(rendered, "modelConfidenceIntervals", fixed = TRUE)
  expect_match(rendered, "confidenceIntervals", fixed = TRUE)
})

test_that("diagnostic rendering is family aware", {
  linearData = data.frame(outcome = c(2, 4, 5, 8), predictor = 1:4)
  poissonData = data.frame(outcome = c(1, 2, 4, 5), predictor = 1:4)

  linearRecipe = buildAnalysisRecipeFromFit(
    lm(outcome ~ predictor, data = linearData),
    dataSource = "package",
    packageName = "examplePackage",
    datasetName = "linearData"
  )
  poissonRecipe = buildAnalysisRecipeFromFit(
    glm(outcome ~ predictor, data = poissonData, family = poisson()),
    dataSource = "package",
    packageName = "examplePackage",
    datasetName = "poissonData"
  )

  expect_match(paste(renderAnalysisRecipeDiagnosticChunk(linearRecipe), collapse = "\n"), "plot(modelFit)", fixed = TRUE)
  expect_match(paste(renderAnalysisRecipeDiagnosticChunk(poissonRecipe), collapse = "\n"), 'residuals(modelFit, type = "deviance")', fixed = TRUE)
})

test_that("model plot rendering records deterministic plot settings", {
  modelData = data.frame(outcome = c(2, 4, 5, 8), predictor = 1:4)
  recipe = buildAnalysisRecipeFromFit(
    lm(outcome ~ predictor, data = modelData),
    dataSource = "package",
    packageName = "examplePackage",
    datasetName = "modelData"
  )

  rendered = paste(renderAnalysisRecipeModelPlotChunk(recipe), collapse = "\n")

  expect_match(rendered, "plotModelPlot(", fixed = TRUE)
  expect_match(rendered, 'plotType = "observedFitted"', fixed = TRUE)
  expect_match(rendered, "showSmoothTrend = TRUE", fixed = TRUE)
})

test_that("model confidence interval helper returns the WMFM display table", {
  modelData = data.frame(outcome = c(2, 4, 5, 8), predictor = 1:4)
  modelFit = lm(outcome ~ predictor, data = modelData)

  result = modelConfidenceIntervals(modelFit)

  expect_s3_class(result, "data.frame")
  expect_true(all(c("quantity", "estimate", "lower", "upper") %in% names(result)))
})
