test_that("chooseExplanationAnchor uses the median as the default typical anchor", {
  d = data.frame(
    y = c(1, 2, 3, 4, 5),
    x = c(0, 10, 10, 10, 100)
  )

  out = chooseExplanationAnchor(mf = d)

  expect_identical(out$anchorReference, "median")
  expect_identical(out$table$anchorVariable, "x")
  expect_equal(out$table$anchorValue, 10)
  expect_identical(out$table$anchorDisplayValue, "10")
  expect_identical(out$table$anchorSource, "median")
  expect_match(out$table$anchorReason, "sample median", fixed = TRUE)
})

test_that("chooseExplanationAnchor can use the mean when requested", {
  d = data.frame(
    y = c(1, 2, 3, 4),
    x = c(5, 6, 7, 8)
  )

  out = chooseExplanationAnchor(mf = d, defaultSource = "mean")

  expect_identical(out$anchorReference, "mean")
  expect_equal(out$table$anchorValue, 6.5)
  expect_identical(out$table$anchorDisplayValue, "6.5")
})

test_that("chooseExplanationAnchor gives user anchors first priority", {
  d = data.frame(
    y = c(1, 2, 3, 4),
    x = c(5, 6, 7, 8)
  )

  out = chooseExplanationAnchor(
    mf = d,
    userAnchors = c(x = 7),
    meaningfulAnchors = c(x = 6)
  )

  expect_identical(out$anchorReference, "user")
  expect_equal(out$table$anchorValue, 7)
  expect_identical(out$table$anchorSource, "user")
  expect_match(out$promptText, "user supplied", fixed = TRUE)
})

test_that("chooseExplanationAnchor uses meaningful anchors before default typical anchors", {
  d = data.frame(
    y = c(1, 2, 3, 4),
    mark = c(0, 5, 10, 20)
  )

  out = chooseExplanationAnchor(
    mf = d,
    meaningfulAnchors = c(mark = 10)
  )

  expect_identical(out$anchorReference, "meaningful")
  expect_equal(out$table$anchorValue, 10)
  expect_identical(out$table$anchorSource, "meaningful")
  expect_match(out$table$anchorReason, "pedagogically meaningful", fixed = TRUE)
})

test_that("chooseExplanationAnchor records multiple numeric predictors", {
  d = data.frame(
    y = c(1, 2, 3, 4),
    x = c(5, 6, 7, 8),
    z = c(20, 22, 24, 26),
    g = factor(c("a", "a", "b", "b"))
  )

  out = chooseExplanationAnchor(mf = d)

  expect_identical(out$table$anchorVariable, c("x", "z"))
  expect_equal(out$table$anchorValue, c(6.5, 23))
  expect_identical(out$table$anchorDisplayValue, c("6.5", "23"))
  expect_match(out$cacheKey, "x=median=", fixed = TRUE)
  expect_match(out$cacheKey, "z=median=", fixed = TRUE)
})

test_that("chooseExplanationAnchor returns empty metadata when no numeric predictors exist", {
  d = data.frame(
    y = c(1, 2, 3, 4),
    g = factor(c("a", "a", "b", "b"))
  )

  out = chooseExplanationAnchor(mf = d)

  expect_identical(out$anchorReference, "none")
  expect_equal(nrow(out$table), 0)
  expect_identical(out$cacheKey, "no_numeric_anchors")
})

test_that("buildAnchorAuditEntry preserves raw and displayed anchor values", {
  d = data.frame(
    y = c(1, 2, 3, 4),
    x = c(5.25, 5.5, 5.75, 6)
  )

  anchorInfo = chooseExplanationAnchor(mf = d)
  out = buildAnchorAuditEntry(anchorInfo)

  expect_identical(out$anchorReference, "median")
  expect_equal(out$table$anchorValue, 5.625)
  expect_identical(out$table$anchorDisplayValue, "5.6")
  expect_match(out$note, "Raw anchor values", fixed = TRUE)
})

test_that("chooseExplanationAnchor validates supplied anchors", {
  d = data.frame(
    y = c(1, 2, 3, 4),
    x = c(5, 6, 7, 8)
  )

  expect_error(
    chooseExplanationAnchor(mf = d, userAnchors = c(7)),
    "must be named",
    fixed = TRUE
  )

  expect_error(
    chooseExplanationAnchor(mf = d, meaningfulAnchors = "x"),
    "must be a named numeric",
    fixed = TRUE
  )
})
