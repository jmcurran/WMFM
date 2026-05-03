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
