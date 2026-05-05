test_that("startup status helpers use immediate s20x wording when available", {
  expect_equal(
    buildInitialPackageScanStatus("s20x"),
    "Showing s20x now while other installed packages are checked."
  )
})

test_that("startup status helpers use scanning wording when no package is immediate", {
  expect_equal(
    buildInitialPackageScanStatus(character(0)),
    "Checking installed packages for datasets."
  )
})

test_that("startup status helpers centralise default status text", {
  expect_equal(
    buildPackageDatasetChoiceStatus(),
    "Choose a package to see its available datasets."
  )

  expect_equal(
    buildInitialExampleLoadStatus(),
    "Loading the built-in examples."
  )
})

test_that("package scan completion status handles empty and unchanged scans", {
  expect_equal(
    buildPackageScanCompleteStatus(character(0), character(0)),
    "No installed packages with datasets were found."
  )

  expect_null(
    buildPackageScanCompleteStatus("s20x", "s20x")
  )
})

test_that("package scan completion status reports discovered package counts", {
  expect_equal(
    buildPackageScanCompleteStatus(c("s20x", "datasets"), "s20x"),
    "Found 2 installed packages with datasets."
  )

  expect_equal(
    buildPackageScanCompleteStatus("datasets", character(0)),
    "Found 1 installed package with datasets."
  )
})

test_that("package dataset status helpers centralise package scan text", {
  expect_equal(
    buildPackageDatasetCheckingStatus("s20x"),
    "Checking datasets in s20x."
  )

  expect_equal(
    buildPackageDatasetFindingMessage("datasets"),
    "Finding datasets in datasets."
  )
})

test_that("package dataset status helpers handle unavailable datasets", {
  expect_equal(
    buildS20xPackageMissingStatus(),
    "The s20x package is not installed."
  )

  expect_equal(
    buildS20xPackageMissingChoiceLabel(),
    "s20x is not installed"
  )

  expect_equal(
    buildPackageDatasetEmptyStatus("MASS"),
    "No datasets were found in MASS."
  )

  expect_equal(
    buildPackageDatasetEmptyChoiceLabel(),
    "No datasets found"
  )
})

test_that("package dataset found status helper handles singular and plural counts", {
  expect_equal(
    buildPackageDatasetFoundStatus("s20x", "fuel"),
    "Found 1 dataset in s20x."
  )

  expect_equal(
    buildPackageDatasetFoundStatus("datasets", c("airquality", "iris")),
    "Found 2 datasets in datasets."
  )
})

test_that("startup example helpers centralise loading and ready text", {
  expect_equal(
    buildLoadingExampleChoice(),
    c("Loading examples..." = "")
  )

  expect_equal(
    buildExampleReadyStatus(),
    "Choose a built-in example if you want the app to load a complete worked setup."
  )
})

test_that("startup package scan helpers centralise transient status text", {
  expect_equal(
    buildPackageDatasetPendingScanStatus(),
    "Dataset choices will appear once the package scan has finished."
  )

  expect_equal(
    buildStartupDataChoicesMessage(),
    "Preparing built-in examples and installed-package datasets."
  )

  expect_equal(
    buildNoPackageDatasetsStatus(),
    "No package datasets are available yet."
  )

  expect_equal(
    buildPackageListUpdatingStatus(),
    "Updating the package list."
  )
})
