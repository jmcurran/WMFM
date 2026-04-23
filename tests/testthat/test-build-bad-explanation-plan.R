makeBadExplanationTestModel = function(includeFactor = FALSE, includeInteraction = FALSE) {

  if (isTRUE(includeFactor) && isTRUE(includeInteraction)) {
    dat = data.frame(
      y = c(10, 12, 9, 14, 11, 15),
      x1 = c(1, 2, 3, 4, 5, 6),
      grp = factor(c("A", "A", "B", "B", "A", "B"))
    )

    model = lm(y ~ x1 * grp, data = dat)
    interactionTerms = "x1:grpB"
    formulaObj = y ~ x1 * grp
  } else if (isTRUE(includeFactor)) {
    dat = data.frame(
      y = c(10, 12, 9, 14, 11, 15),
      x1 = c(1, 2, 3, 4, 5, 6),
      grp = factor(c("A", "A", "B", "B", "A", "B"))
    )

    model = lm(y ~ x1 + grp, data = dat)
    interactionTerms = character(0)
    formulaObj = y ~ x1 + grp
  } else {
    dat = data.frame(
      y = c(10, 12, 9, 14, 11, 15),
      x1 = c(1, 2, 3, 4, 5, 6),
      x2 = c(6, 5, 4, 3, 2, 1)
    )

    model = lm(y ~ x1 + x2, data = dat)
    interactionTerms = character(0)
    formulaObj = y ~ x1 + x2
  }

  newWmfmModel(
    model = model,
    formula = formulaObj,
    modelType = "lm",
    data = dat,
    explanation = "This is a good explanation.",
    interactionTerms = interactionTerms,
    interactionMinPValue = if (length(interactionTerms) > 0L) 0.04 else NA_real_
  )
}

test_that("getAvailableBadExplanationTypes excludes factor and interaction types when absent", {

  x = makeBadExplanationTestModel(includeFactor = FALSE, includeInteraction = FALSE)
  out = getAvailableBadExplanationTypes(x)

  expect_false("referenceLevelError" %in% out)
  expect_false("factorOffsetOmissionError" %in% out)
  expect_false("interactionIgnoredError" %in% out)
  expect_false("mainEffectOverinterpretationWithInteraction" %in% out)
})

test_that("getAvailableBadExplanationTypes includes factor types when factors are present", {

  x = makeBadExplanationTestModel(includeFactor = TRUE, includeInteraction = FALSE)
  out = getAvailableBadExplanationTypes(x)

  expect_true("referenceLevelError" %in% out)
  expect_true("factorOffsetOmissionError" %in% out)
  expect_false("interactionIgnoredError" %in% out)
})

test_that("getAvailableBadExplanationTypes includes interaction types when interactions are present", {

  x = makeBadExplanationTestModel(includeFactor = TRUE, includeInteraction = TRUE)
  out = getAvailableBadExplanationTypes(x)

  expect_true("interactionIgnoredError" %in% out)
  expect_true("mainEffectOverinterpretationWithInteraction" %in% out)
})

test_that("buildBadExplanationPlan returns one type per explanation when mixTypes is FALSE", {

  set.seed(1)
  x = makeBadExplanationTestModel(includeFactor = TRUE, includeInteraction = TRUE)

  plan = buildBadExplanationPlan(
    x = x,
    type = c("effectDirectionError", "wrongScaleError"),
    severity = "moderate",
    n = 4,
    mixTypes = FALSE
  )

  expect_length(plan$selectedTypes, 4)
  expect_true(all(vapply(plan$selectedTypes, length, integer(1)) == 1L))
})

test_that("buildBadExplanationPlan returns multiple types when mixTypes is TRUE and severity allows it", {

  set.seed(1)
  x = makeBadExplanationTestModel(includeFactor = TRUE, includeInteraction = TRUE)

  plan = buildBadExplanationPlan(
    x = x,
    type = c("effectDirectionError", "wrongScaleError", "rSquaredOverclaim"),
    severity = "severe",
    n = 3,
    mixTypes = TRUE
  )

  expect_length(plan$selectedTypes, 3)
  expect_true(all(vapply(plan$selectedTypes, length, integer(1)) >= 2L))
})
