test_that("package test files are not named after development stages", {
  testFiles = list.files(
    file.path("tests", "testthat"),
    pattern = "[.]R$",
    full.names = FALSE
  )

  stageNamedFiles = grep(
    "(^|[-_])stage[0-9]+([._-]|$)",
    testFiles,
    value = TRUE,
    ignore.case = TRUE
  )

  expect_equal(stageNamedFiles, character())
})
