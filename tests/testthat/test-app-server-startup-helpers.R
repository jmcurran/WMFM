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
