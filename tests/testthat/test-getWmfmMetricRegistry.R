test_that("getWmfmMetricRegistry returns a valid registry", {
  registry = getWmfmMetricRegistry()

  expect_s3_class(registry, "data.frame")
  expect_true(nrow(registry) >= 1)
  expect_true(all(c(
    "metricName",
    "metricType",
    "label",
    "group",
    "includeInComparison",
    "includeInStability",
    "includeInPlots",
    "orderedLevels"
  ) %in% names(registry)))

  expect_true(any(registry$metricType == "continuous"))
  expect_true(any(registry$metricType == "ordinal"))
  expect_true(any(registry$metricType == "binary"))

  expect_invisible(validateWmfmMetricRegistry(registry))
})
