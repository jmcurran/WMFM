test_that("package-data recipes download as Quarto documents", {
  recipe = newAnalysisRecipe(
    data = list(source = "package", packageName = "datasets", datasetName = "mtcars"),
    preparation = list(),
    model = list(formula = "mpg ~ wt", modelType = "lm", response = "mpg", predictors = "wt")
  )

  outputPath = tempfile(fileext = ".qmd")
  writeAnalysisRecipeDownload(recipe, mtcars, outputPath)

  expect_identical(analysisRecipeDownloadExtension(recipe), "qmd")
  expect_true(file.exists(outputPath))
  outputText = paste(readLines(outputPath, warn = FALSE), collapse = "\n")
  expect_match(outputText, "# Load the data", fixed = TRUE)
  expect_match(outputText, "data(mtcars)", fixed = TRUE)
})

test_that("uploaded-data recipes download with portable CSV data", {
  skip_if(Sys.which("zip") == "", "A zip executable is required for this packaging test.")

  recipe = newAnalysisRecipe(
    data = list(source = "upload", uploadedFileName = "original.csv"),
    preparation = list(),
    model = list(formula = "y ~ x", modelType = "lm", response = "y", predictors = "x")
  )
  analysisData = data.frame(x = 1:4, y = c(2, 4, 5, 8))
  outputPath = tempfile(fileext = ".zip")

  writeAnalysisRecipeDownload(recipe, analysisData, outputPath)

  expect_identical(analysisRecipeDownloadExtension(recipe), "zip")
  archiveFiles = utils::unzip(outputPath, list = TRUE)$Name
  expect_setequal(archiveFiles, c("wmfm_analysis.qmd", "data/analysis_data.csv"))

  extractionDirectory = tempfile("wmfm-analysis-extract-")
  dir.create(extractionDirectory)
  utils::unzip(outputPath, exdir = extractionDirectory)

  quartoText = paste(
    readLines(file.path(extractionDirectory, "wmfm_analysis.qmd"), warn = FALSE),
    collapse = "\n"
  )
  restoredData = utils::read.csv(file.path(extractionDirectory, "data", "analysis_data.csv"))

  expect_match(quartoText, '"data/analysis_data.csv"', fixed = TRUE)
  expect_equal(restoredData, analysisData)
})

test_that("uploaded-data downloads require analysis data", {
  recipe = newAnalysisRecipe(
    data = list(source = "upload", uploadedFileName = "original.csv"),
    preparation = list(),
    model = list(formula = "y ~ x", modelType = "lm", response = "y", predictors = "x")
  )

  expect_error(
    writeAnalysisRecipeDownload(recipe, NULL, tempfile(fileext = ".zip")),
    "require the fitted analysis data"
  )
})

test_that("the app exposes the analysis download through recipe state", {
  uiText = readPackageText("R", "app-ui.R")
  serverText = readPackageText("R", "app-server.R")
  downloadText = readPackageText("R", "app-server-analysis-download.R")

  expect_match(uiText, 'outputId = "analysisDownload"', fixed = TRUE)
  expect_match(serverText, "registerAnalysisDownloadObserver", fixed = TRUE)
  expect_match(downloadText, "rv$analysisRecipe", fixed = TRUE)
  expect_match(downloadText, "writeAnalysisRecipeDownload", fixed = TRUE)
})
