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
  expect_match(quartoText, "data(exampleData)", fixed = TRUE)
  expect_false(grepl("analysisData = get", quartoText, fixed = TRUE))
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
    factorVariables = "group",
    orderedFactorVariables = "group"
  )

  renderedText = paste(renderAnalysisRecipePreparationSection(recipe), collapse = "\n")

  expect_match(
    renderedText,
    "analysisData$logPredictor = with(analysisData, log(predictor))",
    fixed = TRUE
  )
  expect_match(
    renderedText,
    "The selected factor variables `group` are stored as ordered factors.",
    fixed = TRUE
  )
  expect_match(
    renderedText,
    'orderedFactors = c("group")',
    fixed = TRUE
  )
  expect_match(
    renderedText,
    "for (variable in orderedFactors) {",
    fixed = TRUE
  )
  expect_match(
    renderedText,
    "analysisData[[variable]] = factor(as.character(analysisData[[variable]]))",
    fixed = TRUE
  )
  expect_false(grepl("is.ordered", renderedText, fixed = TRUE))
})

test_that("ordered-factor preparation is compact for several selected variables", {
  modelData = data.frame(
    outcome = 1:4,
    first = ordered(c("low", "low", "high", "high")),
    second = ordered(c("A", "B", "A", "B"))
  )
  modelData$first = factor(as.character(modelData$first))
  modelData$second = factor(as.character(modelData$second))

  recipe = buildAnalysisRecipeFromFit(
    model = lm(outcome ~ first + second, data = modelData),
    dataSource = "package",
    packageName = "examplePackage",
    datasetName = "exampleData",
    factorVariables = c("first", "second"),
    orderedFactorVariables = c("first", "second")
  )

  renderedText = paste(renderAnalysisRecipePreparationSection(recipe), collapse = "\n")

  expect_match(
    renderedText,
    'orderedFactors = c("first", "second")',
    fixed = TRUE
  )
  expect_equal(
    lengths(regmatches(renderedText, gregexpr("for (variable in orderedFactors)", renderedText, fixed = TRUE))),
    1L
  )
  expect_false(grepl("exampleData$first", renderedText, fixed = TRUE))
  expect_false(grepl("exampleData$second", renderedText, fixed = TRUE))
})

test_that("ordinary selected factors do not receive ordered-factor commentary", {
  modelData = data.frame(
    outcome = 1:4,
    group = factor(c("A", "A", "B", "B"))
  )
  recipe = buildAnalysisRecipeFromFit(
    model = lm(outcome ~ group, data = modelData),
    dataSource = "package",
    packageName = "examplePackage",
    datasetName = "exampleData",
    factorVariables = "group"
  )

  renderedText = paste(renderAnalysisRecipePreparationSection(recipe), collapse = "\n")

  expect_false(grepl("ordered factors", renderedText, fixed = TRUE))
  expect_match(
    renderedText,
    "exampleData$group = factor(exampleData$group)",
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


test_that("package data keep their original object name throughout", {
  courseData = data.frame(Exam = 1:4, Attend = c("No", "Yes", "No", "Yes"), Test = 2:5)
  recipe = buildAnalysisRecipeFromFit(
    lm(Exam ~ factor(Attend) + Test, data = courseData),
    dataSource = "package",
    packageName = "s20x",
    datasetName = "course.df",
    factorVariables = "Attend"
  )

  renderedText = paste(renderAnalysisRecipeCoreQuarto(recipe), collapse = "\n")

  expect_match(renderedText, "data(course.df)", fixed = TRUE)
  expect_match(renderedText, "course.df$Attend", fixed = TRUE)
  expect_match(renderedText, "data = course.df", fixed = TRUE)
  expect_false(grepl("analysisData", renderedText, fixed = TRUE))
})
