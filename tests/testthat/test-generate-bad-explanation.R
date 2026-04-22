makeFakeBadExplanationProvider = function(rawResponse) {

  structure(
    list(
      chat = function(prompt) {
        rawResponse
      }
    ),
    class = "wmfm_fake_bad_explanation_provider"
  )
}

makeBadExplanationGradeModel = function() {

  dat = data.frame(
    y = c(10, 12, 9, 14, 11, 15),
    x1 = c(1, 2, 3, 4, 5, 6),
    grp = factor(c("A", "A", "B", "B", "A", "B"))
  )

  model = lm(y ~ x1 + grp, data = dat)

  newWmfmModel(
    model = model,
    formula = y ~ x1 + grp,
    modelType = "lm",
    data = dat,
    explanation = paste(
      "The model suggests that higher x1 values are associated with higher y values,",
      "and group B differs from the reference group after accounting for x1."
    ),
    interactionTerms = character(0),
    interactionMinPValue = NA_real_
  )
}

test_that("generateBadExplanation returns a character scalar for one explanation", {

  x = makeBadExplanationGradeModel()
  provider = makeFakeBadExplanationProvider(
    '[
      {
        "name": "explanation_1",
        "text": "Higher x1 causes higher y.",
        "errorTypes": ["causalInferenceError"],
        "severity": "subtle"
      }
    ]'
  )

  out = generateBadExplanation(
    x,
    type = "causalInferenceError",
    n = 1,
    provider = provider
  )

  expect_type(out, "character")
  expect_length(out, 1)
  expect_false(!is.null(names(out)))
})

test_that("generateBadExplanation returns a named character vector for multiple explanations", {

  x = makeBadExplanationGradeModel()
  provider = makeFakeBadExplanationProvider(
    '[
      {
        "name": "explanation_1",
        "text": "Higher x1 causes higher y.",
        "errorTypes": ["causalInferenceError"],
        "severity": "subtle"
      },
      {
        "name": "explanation_2",
        "text": "Group B is the reference level and x1 lowers y.",
        "errorTypes": ["referenceLevelError", "effectDirectionError"],
        "severity": "moderate"
      }
    ]'
  )

  out = generateBadExplanation(
    x,
    type = c("causalInferenceError", "referenceLevelError"),
    n = 2,
    mixTypes = TRUE,
    provider = provider
  )

  expect_type(out, "character")
  expect_identical(names(out), c("explanation_1", "explanation_2"))
  expect_length(out, 2)
})

test_that("generateBadExplanation can return labelled output", {

  x = makeBadExplanationGradeModel()
  provider = makeFakeBadExplanationProvider(
    '[
      {
        "name": "explanation_1",
        "text": "Higher x1 causes higher y.",
        "errorTypes": ["causalInferenceError"],
        "severity": "subtle"
      },
      {
        "name": "explanation_2",
        "text": "Group B is the reference level and x1 lowers y.",
        "errorTypes": ["referenceLevelError", "effectDirectionError"],
        "severity": "moderate"
      }
    ]'
  )

  out = generateBadExplanation(
    x,
    type = c("causalInferenceError", "referenceLevelError"),
    n = 2,
    mixTypes = TRUE,
    labelErrors = TRUE,
    provider = provider
  )

  expect_type(out, "list")
  expect_named(out, c("explanations", "errorTypes", "severity"))
  expect_identical(names(out$explanations), c("explanation_1", "explanation_2"))
})

test_that("generated bad explanations can be passed directly to grade", {

  x = makeBadExplanationGradeModel()
  provider = makeFakeBadExplanationProvider(
    '[
      {
        "name": "explanation_1",
        "text": "Higher x1 causes higher y.",
        "errorTypes": ["causalInferenceError"],
        "severity": "subtle"
      },
      {
        "name": "explanation_2",
        "text": "Group B is the reference level and x1 lowers y.",
        "errorTypes": ["referenceLevelError", "effectDirectionError"],
        "severity": "moderate"
      }
    ]'
  )

  out = generateBadExplanation(
    x,
    type = c("causalInferenceError", "referenceLevelError"),
    n = 2,
    mixTypes = TRUE,
    provider = provider
  )

  graded = grade(
    x,
    explanation = out,
    autoScore = FALSE
  )

  expect_s3_class(graded, "wmfmGradeListObj")
  expect_length(graded$grades, 2)
})
