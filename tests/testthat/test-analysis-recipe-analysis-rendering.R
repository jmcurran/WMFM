test_that("post-fit Quarto sections follow the agreed analysis order", {
  modelData = data.frame(outcome = c(2, 4, 6, 8), predictor = 1:4)
  recipe = buildAnalysisRecipeFromFit(
    lm(outcome ~ predictor, data = modelData),
    dataSource = "package",
    packageName = "examplePackage",
    datasetName = "modelData"
  )

  lines = renderAnalysisRecipeCoreQuarto(recipe)

  expect_true(match("# Model summary", lines) < match("# Fitted equation", lines))
  expect_true(match("# Fitted equation", lines) < match("# Analysis of variance", lines))
  expect_true(match("# Analysis of variance", lines) < match("# Diagnostic plots", lines))
  expect_true(match("# Diagnostic plots", lines) < match("# Model plot", lines))
  expect_false("# Confidence intervals" %in% lines)
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

test_that("fitted equation rendering uses model coefficients explicitly", {
  modelData = data.frame(outcome = c(2, 4, 5, 8), predictor = 1:4)
  recipe = buildAnalysisRecipeFromFit(
    lm(outcome ~ predictor, data = modelData),
    dataSource = "package",
    packageName = "examplePackage",
    datasetName = "modelData"
  )

  rendered = paste(renderAnalysisRecipeEquationChunk(recipe), collapse = "\n")

  expect_match(rendered, "modelCoefficients = coef(modelFit)", fixed = TRUE)
  expect_match(rendered, 'equationText = paste0("E(outcome) = "', fixed = TRUE)
})

test_that("factor-only linear models render fitted means with confidence intervals", {
  modelData = data.frame(
    outcome = c(2, 4, 5, 8, 6, 9),
    group = factor(c("A", "A", "B", "B", "C", "C"))
  )
  recipe = buildAnalysisRecipeFromFit(
    lm(outcome ~ group, data = modelData),
    dataSource = "package",
    packageName = "examplePackage",
    datasetName = "modelData"
  )

  rendered = paste(renderAnalysisRecipeFittedModelSection(recipe), collapse = "\n")

  expect_match(rendered, "# Fitted means", fixed = TRUE)
  expect_match(rendered, "fittedMeanData = expand.grid(", fixed = TRUE)
  expect_match(rendered, 'interval = "confidence"', fixed = TRUE)
  expect_match(rendered, "fittedMeans = cbind(", fixed = TRUE)
  expect_false(grepl("coefficientIntervals", rendered, fixed = TRUE))
})

test_that("factor-only GLMs calculate response-scale fitted mean intervals", {
  modelData = data.frame(
    outcome = c(0, 1, 0, 1, 1, 1, 0, 0),
    group = factor(rep(c("A", "B"), each = 4))
  )
  recipe = buildAnalysisRecipeFromFit(
    glm(outcome ~ group, data = modelData, family = binomial()),
    dataSource = "package",
    packageName = "examplePackage",
    datasetName = "modelData"
  )

  rendered = paste(renderAnalysisRecipeFittedMeansChunk(recipe), collapse = "\n")

  expect_match(rendered, 'type = "link"', fixed = TRUE)
  expect_match(rendered, "inverseLink = family(modelFit)$linkinv", fixed = TRUE)
  expect_match(rendered, "fit = inverseLink(linkPredictions$fit)", fixed = TRUE)
})

test_that("coefficient confidence intervals are disabled in new recipes", {
  modelData = data.frame(outcome = c(2, 4, 5, 8), predictor = 1:4)
  recipe = buildAnalysisRecipeFromFit(
    lm(outcome ~ predictor, data = modelData),
    dataSource = "package",
    packageName = "examplePackage",
    datasetName = "modelData"
  )

  expect_false(recipe$sections$confidenceIntervals$enabled)
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

test_that("model plot rendering emits standalone ggplot2 code", {
  modelData = data.frame(outcome = c(2, 4, 5, 8), predictor = 1:4)
  recipe = buildAnalysisRecipeFromFit(
    lm(outcome ~ predictor, data = modelData),
    dataSource = "package",
    packageName = "examplePackage",
    datasetName = "modelData"
  )

  rendered = paste(renderAnalysisRecipeModelPlotChunk(recipe), collapse = "\n")

  expect_match(rendered, "modelPlotData = data.frame(", fixed = TRUE)
  expect_match(rendered, "model.response(model.frame(modelFit))", fixed = TRUE)
  expect_match(rendered, "modelPlot = ggplot(", fixed = TRUE)
  expect_match(rendered, "geom_abline(", fixed = TRUE)
  expect_false(grepl("plotModelPlot", rendered, fixed = TRUE))
})

test_that("generated standalone analysis does not require WMFM", {
  modelData = data.frame(outcome = c(2, 4, 5, 8), predictor = 1:4)
  recipe = buildAnalysisRecipeFromFit(
    lm(outcome ~ predictor, data = modelData),
    dataSource = "package",
    packageName = "examplePackage",
    datasetName = "modelData"
  )

  rendered = paste(renderAnalysisRecipeCoreQuarto(recipe), collapse = "\n")

  expect_match(rendered, "library(ggplot2)", fixed = TRUE)
  expect_false(grepl("library(WMFM)", rendered, fixed = TRUE))
  expect_false(grepl("WMFM::", rendered, fixed = TRUE))
  expect_false(grepl("modelConfidenceIntervals", rendered, fixed = TRUE))
  expect_false(grepl("plotModelPlot", rendered, fixed = TRUE))
})
