testthat::test_that("buildInterpretationModeLabel returns NULL without adjustments", {
  out = buildInterpretationModeLabel(character(0))
  testthat::expect_null(out)
})

testthat::test_that("buildInterpretationModeLabel builds compact adjusted-for banner", {
  out = buildInterpretationModeLabel(c("picture", "age", "picture"))
  testthat::expect_identical(
    out,
    "Interpretation mode: primary effects adjusted for picture, age"
  )
})
