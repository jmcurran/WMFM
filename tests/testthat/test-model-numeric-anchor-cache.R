test_that("lmExplanation cache key depends on numeric anchor metadata", {
  local_mocked_bindings(
    buildModelNumericAnchorInfo = function(...) {
      list(
        numericReference = "mean",
        promptText = "anchor prompt",
        cacheKey = "anchor_a"
      )
    },
    .package = "WMFM"
  )

  rm(list = ls(envir = .env_cache), envir = .env_cache)

  on.exit({
    rm(list = ls(envir = .env_cache), envir = .env_cache)
  }, add = TRUE)

  dat = data.frame(
    y = c(1.0, 2.1, 2.9, 4.2, 5.1),
    x = c(5, 6, 7, 8, 9)
  )
  fit = stats::lm(y ~ x, data = dat)

  callCount = 0L
  chat = list(
    chat = function(prompt) {
      callCount <<- callCount + 1L
      paste("explanation", callCount)
    }
  )

  first = suppressWarnings(lmExplanation(fit, chat = chat, useCache = TRUE))
  second = suppressWarnings(lmExplanation(fit, chat = chat, useCache = TRUE))

  expect_identical(first, second)
  expect_identical(callCount, 1L)

  local_mocked_bindings(
    buildModelNumericAnchorInfo = function(...) {
      list(
        numericReference = "zero",
        promptText = "anchor prompt",
        cacheKey = "anchor_b"
      )
    },
    .package = "WMFM"
  )

  third = suppressWarnings(lmExplanation(fit, chat = chat, useCache = TRUE))

  expect_identical(callCount, 2L)
  expect_false(identical(third, first))
})

test_that("lmEquations cache key depends on numeric anchor metadata", {
  local_mocked_bindings(
    buildModelNumericAnchorInfo = function(...) {
      list(
        numericReference = "mean",
        promptText = "anchor prompt",
        cacheKey = "anchor_a"
      )
    },
    .package = "WMFM"
  )

  rm(list = ls(envir = .env_cache), envir = .env_cache)

  on.exit({
    rm(list = ls(envir = .env_cache), envir = .env_cache)
  }, add = TRUE)

  dat = data.frame(
    y = c(1.0, 2.1, 2.9, 4.2, 5.1),
    x = c(5, 6, 7, 8, 9)
  )
  fit = stats::lm(y ~ x, data = dat)

  callCount = 0L
  chat = list(
    chat = function(prompt) {
      callCount <<- callCount + 1L
      paste("equation", callCount)
    }
  )

  first = suppressWarnings(lmEquations(fit, chat = chat))
  second = suppressWarnings(lmEquations(fit, chat = chat))

  expect_identical(first, second)
  expect_identical(callCount, 1L)

  local_mocked_bindings(
    buildModelNumericAnchorInfo = function(...) {
      list(
        numericReference = "zero",
        promptText = "anchor prompt",
        cacheKey = "anchor_b"
      )
    },
    .package = "WMFM"
  )

  third = suppressWarnings(lmEquations(fit, chat = chat))

  expect_identical(callCount, 2L)
  expect_false(identical(third, first))
})

test_that("lmExplanation cache key changes when adjustment policy inputs change", {
  keyA = buildLmExplanationCacheKey(
    formulaStr = "y ~ gender + picture + gender:picture",
    coefStr = "1;2;3",
    numericAnchorCacheKey = "anchor",
    researchQuestion = "Does gender differ?",
    adjustmentVariables = "picture"
  )
  keyB = buildLmExplanationCacheKey(
    formulaStr = "y ~ gender + picture + gender:picture",
    coefStr = "1;2;3",
    numericAnchorCacheKey = "anchor",
    researchQuestion = "Does gender differ?",
    adjustmentVariables = character(0)
  )

  expect_false(identical(keyA, keyB))
  expect_match(keyA, "stage20.13-v1", fixed = TRUE)
})


test_that("lmExplanation cache key changes when research question changes", {
  keyA = buildLmExplanationCacheKey(
    formulaStr = "y ~ x",
    coefStr = "1;2",
    numericAnchorCacheKey = "anchor",
    researchQuestion = "Does x increase y?",
    adjustmentVariables = character(0)
  )
  keyB = buildLmExplanationCacheKey(
    formulaStr = "y ~ x",
    coefStr = "1;2",
    numericAnchorCacheKey = "anchor",
    researchQuestion = "How does x relate to y?",
    adjustmentVariables = character(0)
  )

  expect_false(identical(keyA, keyB))
})
