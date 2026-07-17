test_that("analysis recipes store deterministic core analysis state", {
  modelData = data.frame(
    outcome = c(2, 4, 6, 8),
    predictor = c(1, 2, 3, 4),
    group = factor(c("A", "A", "B", "B"))
  )
  modelFit = lm(outcome ~ predictor + group, data = modelData)

  recipe = buildAnalysisRecipeFromFit(
    model = modelFit,
    dataSource = "package",
    packageName = "examplePackage",
    datasetName = "exampleData",
    factorVariables = "group"
  )

  expect_s3_class(recipe, "wmfmAnalysisRecipe")
  expect_identical(recipe$schemaVersion, 1L)
  expect_identical(recipe$data$source, "package")
  expect_identical(recipe$data$packageName, "examplePackage")
  expect_identical(recipe$data$datasetName, "exampleData")
  expect_identical(recipe$model$formula, "outcome ~ predictor + group")
  expect_identical(recipe$model$modelType, "lm")
  expect_identical(recipe$model$response, "outcome")
  expect_setequal(recipe$model$predictors, c("predictor", "group"))
  expect_identical(recipe$preparation$factorVariables, "group")
  expect_true(recipe$sections$summary$enabled)
  expect_true(recipe$sections$modelPlot$enabled)
})

test_that("analysis recipe model types reflect supported fitted models", {
  binomialData = data.frame(
    outcome = c(0, 1, 0, 1, 0, 1, 1, 0),
    predictor = seq_len(8)
  )
  poissonData = data.frame(
    outcome = c(1, 2, 4, 8),
    predictor = c(1, 2, 3, 4)
  )

  logisticRecipe = buildAnalysisRecipeFromFit(
    model = glm(outcome ~ predictor, data = binomialData, family = binomial()),
    dataSource = "upload",
    uploadedFileName = "binary.csv"
  )
  poissonRecipe = buildAnalysisRecipeFromFit(
    model = glm(outcome ~ predictor, data = poissonData, family = poisson()),
    dataSource = "upload",
    uploadedFileName = "counts.csv"
  )

  expect_identical(logisticRecipe$model$modelType, "logistic")
  expect_identical(logisticRecipe$model$family, "binomial")
  expect_identical(logisticRecipe$model$link, "logit")
  expect_identical(poissonRecipe$model$modelType, "poisson")
  expect_identical(poissonRecipe$model$family, "poisson")
  expect_identical(poissonRecipe$model$link, "log")
  expect_identical(poissonRecipe$data$uploadedFileName, "counts.csv")
})

test_that("analysis recipe sections update without replacing unrelated state", {
  recipe = newAnalysisRecipe(
    data = list(source = "package"),
    preparation = list(),
    model = list(
      formula = "y ~ x",
      modelType = "lm",
      response = "y",
      predictors = "x"
    )
  )

  updatedRecipe = updateAnalysisRecipeSection(
    recipe = recipe,
    sectionName = "predictions",
    section = list(list(question = "Predict y when x is 3"))
  )

  expect_identical(updatedRecipe$data, recipe$data)
  expect_identical(updatedRecipe$model, recipe$model)
  expect_length(updatedRecipe$sections$predictions, 1)
  expect_identical(
    updatedRecipe$sections$predictions[[1]]$question,
    "Predict y when x is 3"
  )
})

test_that("invalid analysis recipes fail with informative errors", {
  expect_error(
    validateAnalysisRecipe(list()),
    "not a WMFM analysis recipe"
  )

  invalidRecipe = structure(
    list(
      schemaVersion = 1L,
      data = list(),
      preparation = list(),
      model = list(),
      sections = list(),
      output = list()
    ),
    class = c("wmfmAnalysisRecipe", "list")
  )

  expect_error(
    validateAnalysisRecipe(invalidRecipe),
    "model metadata is missing"
  )
})

test_that("successful fits capture and resets clear the analysis recipe", {
  fitModelText = readPackageText("R", "app-server-fit-model.R")
  reactiveStateText = readPackageText("R", "app-server-reactive-state.R")
  stateHelperText = readPackageText("R", "app-server-state-helpers.R")

  expect_match(fitModelText, "rv$analysisRecipe = buildAnalysisRecipeFromFit", fixed = TRUE)
  expect_match(reactiveStateText, "analysisRecipe = NULL", fixed = TRUE)
  expect_match(stateHelperText, "rv$analysisRecipe = NULL", fixed = TRUE)
})

test_that("package-backed examples retain package data provenance", {
  exampleInfo = list(
    spec = list(
      dataSource = "package",
      dataPackage = "s20x",
      dataObject = "course.df"
    )
  )

  metadata = resolveAnalysisRecipeDataMetadata(
    dataSource = "upload",
    uploadedFileName = "temporary-example.csv",
    loadedExample = exampleInfo
  )

  expect_identical(metadata$source, "package")
  expect_identical(metadata$packageName, "s20x")
  expect_identical(metadata$datasetName, "course.df")
  expect_identical(metadata$uploadedFileName, "")
})

test_that("file-backed examples remain portable bundled-data downloads", {
  exampleInfo = list(
    spec = list(
      dataSource = "file",
      data = "example-data.csv"
    )
  )

  metadata = resolveAnalysisRecipeDataMetadata(
    dataSource = "upload",
    loadedExample = exampleInfo
  )

  expect_identical(metadata$source, "upload")
  expect_identical(metadata$packageName, "")
  expect_identical(metadata$datasetName, "")
  expect_identical(metadata$uploadedFileName, "example-data.csv")
})


test_that("analysis recipes retain originally ordered selected factors", {
  modelData = data.frame(
    outcome = 1:4,
    group = factor(c("A", "A", "B", "B"))
  )

  recipe = buildAnalysisRecipeFromFit(
    model = lm(outcome ~ group, data = modelData),
    dataSource = "package",
    packageName = "examplePackage",
    datasetName = "exampleData",
    factorVariables = "group",
    orderedFactorVariables = "group"
  )

  expect_identical(recipe$preparation$orderedFactorVariables, "group")
})
