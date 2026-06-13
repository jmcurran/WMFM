testthat::test_that("durable test files do not use stage-numbered names", {
  testDir = testthat::test_path()
  testFiles = list.files(
    testDir,
    pattern = "^test-.*[.]R$",
    full.names = FALSE
  )

  stageNumberedFiles = grep(
    "^test-stage[0-9]",
    testFiles,
    value = TRUE
  )

  testthat::expect_identical(stageNumberedFiles, character())
})
