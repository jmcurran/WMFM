test_that("completed predictions are appended without replacing earlier recipe state", {
  recipe = newAnalysisRecipe(
    data = list(source = "package"),
    preparation = list(),
    model = list(formula = "y ~ x", modelType = "lm", response = "y", predictors = "x")
  )
  updated = addAnalysisRecipePrediction(
    recipe,
    question = "What is the expected y when x is 3?",
    newData = list(x = 3),
    target = "mean_response",
    level = 0.95
  )
  expect_identical(updated$model, recipe$model)
  expect_length(updated$sections$predictions, 1)
  expect_identical(updated$sections$predictions[[1]]$newData$x, 3)
})

test_that("linear prediction rendering distinguishes confidence and prediction intervals", {
  recipe = newAnalysisRecipe(
    data = list(source = "package"),
    preparation = list(),
    model = list(formula = "y ~ x", modelType = "lm", response = "y", predictors = "x")
  )
  meanRecipe = addAnalysisRecipePrediction(recipe, "Mean?", list(x = 3), "mean_response")
  individualRecipe = addAnalysisRecipePrediction(recipe, "Individual?", list(x = 3), "individual_outcome")
  expect_match(paste(renderAnalysisRecipeDynamicQuarto(meanRecipe), collapse = "\n"), 'interval = "confidence"', fixed = TRUE)
  expect_match(paste(renderAnalysisRecipeDynamicQuarto(individualRecipe), collapse = "\n"), 'interval = "prediction"', fixed = TRUE)
})

test_that("generalised linear prediction rendering stays on the response scale", {
  recipe = newAnalysisRecipe(
    data = list(source = "package"),
    preparation = list(),
    model = list(formula = "y ~ x", modelType = "poisson", response = "y", predictors = "x")
  )
  recipe = addAnalysisRecipePrediction(recipe, "Expected count?", list(x = 2))
  rendered = paste(renderAnalysisRecipeDynamicQuarto(recipe), collapse = "\n")
  expect_match(rendered, 'type = "response"', fixed = TRUE)
  expect_match(rendered, "predictionData = data.frame", fixed = TRUE)
})

test_that("completed contrasts render only when present", {
  recipe = newAnalysisRecipe(
    data = list(source = "package"),
    preparation = list(),
    model = list(formula = "y ~ group", modelType = "lm", response = "y", predictors = "group")
  )
  expect_false(any(renderAnalysisRecipeDynamicQuarto(recipe) == "# Contrasts"))
  recipe = addAnalysisRecipeContrast(
    recipe,
    label = "A minus B",
    codeLines = c("contrastWeights = c(A = 1, B = -1)", "contrastWeights")
  )
  rendered = paste(renderAnalysisRecipeDynamicQuarto(recipe), collapse = "\n")
  expect_match(rendered, "# Contrasts", fixed = TRUE)
  expect_match(rendered, "contrastWeights = c(A = 1, B = -1)", fixed = TRUE)
})

test_that("the complete Quarto renderer places dynamic sections after core analysis", {
  recipe = newAnalysisRecipe(
    data = list(source = "package", packageName = "datasets", datasetName = "mtcars"),
    preparation = list(),
    model = list(formula = "mpg ~ wt", modelType = "lm", response = "mpg", predictors = "wt")
  )
  recipe = addAnalysisRecipePrediction(recipe, "Expected mpg?", list(wt = 3))
  rendered = paste(renderAnalysisRecipeCoreQuarto(recipe), collapse = "\n")
  expect_lt(regexpr("# Fit the model", rendered, fixed = TRUE), regexpr("# Predictions", rendered, fixed = TRUE))
})
