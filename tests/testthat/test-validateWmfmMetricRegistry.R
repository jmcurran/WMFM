test_that("validateWmfmMetricRegistry accepts a valid registry", {
  registry = getWmfmMetricRegistry()
  expect_invisible(validateWmfmMetricRegistry(registry))
})

test_that("validateWmfmMetricRegistry rejects duplicate metric names", {
  registry = getWmfmMetricRegistry()
  registry$metricName[2] = registry$metricName[1]

  expect_error(
    validateWmfmMetricRegistry(registry),
    "metricName"
  )
})

test_that("validateWmfmMetricRegistry rejects invalid metric types", {
  registry = getWmfmMetricRegistry()
  registry$metricType[1] = "categoricalish"

  expect_error(
    validateWmfmMetricRegistry(registry),
    "invalid metricType"
  )
})

test_that("validateWmfmMetricRegistry requires ordered levels for ordinal metrics", {
  registry = getWmfmMetricRegistry()
  row = which(registry$metricType == "ordinal")[1]
  registry$orderedLevels[row] = list(NULL)

  expect_error(
    validateWmfmMetricRegistry(registry),
    "Each ordinal metric"
  )
})

test_that("validateWmfmMetricRegistry requires NULL orderedLevels for non-ordinal metrics", {
  registry = getWmfmMetricRegistry()
  row = which(registry$metricType != "ordinal")[1]
  registry$orderedLevels[row] = list(c("a", "b"))

  expect_error(
    validateWmfmMetricRegistry(registry),
    "Non-ordinal metrics"
  )
})
