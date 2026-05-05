test_that("data load helper messages preserve upload wording", {
  expect_identical(
    buildDelimitedFileReadFailedMessage(),
    "Failed to read file with the chosen separator."
  )

  expect_identical(
    buildNoDataFrameInRdaMessage(),
    "No data frame in RDA file."
  )

  expect_identical(
    buildUnsupportedUploadFileTypeMessage(),
    "Unsupported file type. Please upload CSV, TXT, or RDA."
  )

  expect_identical(
    buildMissingSeparatorMessage(),
    "Please specify a separator."
  )
})

test_that("data load helper messages preserve dataset wording", {
  expect_identical(
    buildPackageDatasetLoadFailedMessage("myData", "myPackage"),
    "Could not load dataset 'myData' from package 'myPackage'."
  )

  expect_identical(
    buildSelectedObjectNotDataFrameMessage(),
    "Selected object is not a data frame."
  )

  expect_identical(
    buildChooseExampleFirstMessage(),
    "Choose an example first."
  )
})


test_that("data load helper messages preserve loaded example status wording", {
  expect_identical(
    buildLoadedExampleStatus("linear-regression"),
    paste0(
      "Loaded example: linear-regression",
      ". The data, research question, and model settings are ready on the Model tab."
    )
  )
})

test_that("data load helper messages preserve data context wording", {
  expect_identical(
    buildLoadDataFirstMessage(),
    "Load a data set first."
  )

  expect_identical(
    buildDataDescriptionTitle("myData"),
    "Data description: myData"
  )

  expect_identical(
    buildDataDescriptionTitle(""),
    "Data description"
  )

  expect_identical(
    buildProvideDataContextTitle(),
    "Provide data context"
  )

  expect_match(
    buildProvideDataContextHelpText(),
    "Describe the dataset in a way that will help the model explanation",
    fixed = TRUE
  )

  expect_match(
    buildProvideDataContextPlaceholder(),
    "Each row is a student",
    fixed = TRUE
  )

  expect_identical(
    buildDataContextSavedMessage(),
    "Data context saved."
  )

  expect_identical(
    buildDataContextClearedMessage(),
    "Data context cleared."
  )
})
