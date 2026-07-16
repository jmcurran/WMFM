test_that("core Quarto rendering follows the statistical workflow", {
  modelData = data.frame(
    outcome = c(2, 4, 6, 8),
    predictor = c(1, 2, 3, 4)
  )
  recipe = buildAnalysisRecipeFromFit(
    model = lm(outcome ~ predictor, data = modelData),
    dataSource = "package",
    packageName = "examplePackage",
    datasetName = "exampleData"
  )

  quartoLines = renderAnalysisRecipeCoreQuarto(recipe)
  quartoText = paste(quartoLines, collapse = "\n")

  expect_true(match("# Load packages", quartoLines) < match("# Load the data", quartoLines))
  expect_true(match("# Load the data", quartoLines) < match("# Prepare the data", quartoLines))
  expect_true(match("# Prepare the data", quartoLines) < match("# Fit the model", quartoLines))
  expect_match(quartoText, "library(examplePackage)", fixed = TRUE)
  expect_match(
    quartoText,
    'data(list = "exampleData", package = "examplePackage")',
    fixed = TRUE
  )
  expect_match(quartoText, 'analysisData = get("exampleData")', fixed = TRUE)
  expect_match(quartoText, "modelFit = lm(", fixed = TRUE)
  expect_match(quartoText, "formula = outcome ~ predictor", fixed = TRUE)
})

test_that("CSV upload rendering reproduces the WMFM delimited-file reader", {
  modelData = data.frame(
    outcome = c(2, 4, 6, 8),
    predictor = c(1, 2, 3, 4)
  )
  recipe = buildAnalysisRecipeFromFit(
    model = lm(outcome ~ predictor, data = modelData),
    dataSource = "upload",
    uploadedFileName = "student data.csv"
  )

  renderedText = paste(renderAnalysisRecipeDataChunk(recipe), collapse = "\n")

  expect_match(renderedText, '"data/student data.csv"', fixed = TRUE)
  expect_match(renderedText, 'sep = ","', fixed = TRUE)
  expect_match(renderedText, "stringsAsFactors = FALSE", fixed = TRUE)
  expect_match(renderedText, "check.names = TRUE", fixed = TRUE)
  expect_match(renderedText, "fill = TRUE", fixed = TRUE)
})

test_that("data preparation rendering uses recorded transformations and factors", {
  modelData = data.frame(
    outcome = c(2, 4, 6, 8),
    predictor = c(1, 2, 3, 4),
    group = c("A", "A", "B", "B")
  )
  transformation = createVariableTransformationRecord(
    variable = "logPredictor",
    rhs = quote(log(predictor)),
    data = modelData
  )
  modelData$logPredictor = log(modelData$predictor)

  recipe = buildAnalysisRecipeFromFit(
    model = lm(outcome ~ logPredictor + group, data = modelData),
    dataSource = "upload",
    uploadedFileName = "analysis.csv",
    variableTransformations = list(logPredictor = transformation),
    factorVariables = "group"
  )

  renderedText = paste(renderAnalysisRecipePreparationChunk(recipe), collapse = "\n")

  expect_match(
    renderedText,
    "analysisData$logPredictor = with(analysisData, log(predictor))",
    fixed = TRUE
  )
  expect_match(
    renderedText,
    "analysisData$group = factor(analysisData$group)",
    fixed = TRUE
  )
})

test_that("model rendering distinguishes supported model families", {
  linearData = data.frame(outcome = c(2, 4, 6, 8), predictor = 1:4)
  binomialData = data.frame(
    outcome = c(0, 1, 0, 1, 0, 1, 1, 0),
    predictor = 1:8
  )
  poissonData = data.frame(outcome = c(1, 2, 4, 8), predictor = 1:4)

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
  poissonRecipe = buildAnalysisRecipeFromFit(
    glm(outcome ~ predictor, data = poissonData, family = poisson()),
    dataSource = "package",
    packageName = "examplePackage",
    datasetName = "poissonData"
  )

  expect_match(
    paste(renderAnalysisRecipeModelChunk(linearRecipe), collapse = "\n"),
    "modelFit = lm(",
    fixed = TRUE
  )
  expect_match(
    paste(renderAnalysisRecipeModelChunk(logisticRecipe), collapse = "\n"),
    'family = binomial(link = "logit")',
    fixed = TRUE
  )
  expect_match(
    paste(renderAnalysisRecipeModelChunk(poissonRecipe), collapse = "\n"),
    'family = poisson(link = "log")',
    fixed = TRUE
  )
})

test_that("unsupported upload formats fail rather than inventing loading code", {
  modelData = data.frame(outcome = c(2, 4, 6, 8), predictor = 1:4)
  recipe = buildAnalysisRecipeFromFit(
    lm(outcome ~ predictor, data = modelData),
    dataSource = "upload",
    uploadedFileName = "analysis.rda"
  )

  expect_error(
    renderAnalysisRecipeDataChunk(recipe),
    "currently supports package data and CSV uploads"
  )
})
