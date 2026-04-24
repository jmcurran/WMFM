testthat::test_that("developer feedback report captures sentence feedback", {
  d = data.frame(
    Exam = c(60, 70, 80, 90),
    Test = c(55, 65, 75, 85),
    Gender = factor(c("F", "M", "F", "M"))
  )
  m = stats::lm(Exam ~ Test + Gender, data = d)

  claimMap = list(
    claims = data.frame(
      sentenceIndex = c(1L, 2L),
      claimText = c(
        "The typical exam score increases with Test.",
        "There is also a difference between Gender groups."
      ),
      claimTags = I(list(c("effect", "typical_case"), "comparison")),
      stringsAsFactors = FALSE
    )
  )

  input = list(
    developerFeedbackIncorrect_1 = TRUE,
    developerFeedbackComment_1 = "Wrong scale.",
    developerFeedbackIncorrect_2 = FALSE,
    developerFeedbackComment_2 = ""
  )

  report = buildDeveloperFeedbackReport(
    model = m,
    claimMap = claimMap,
    input = input,
    explanationText = paste(claimMap$claims$claimText, collapse = " "),
    researchQuestion = "Does Test predict Exam?",
    data = d,
    timestamp = as.POSIXct("2026-04-24 12:00:00", tz = "Pacific/Auckland")
  )

  testthat::expect_equal(report$metadata$modelType, "lm")
  testthat::expect_equal(report$metadata$modelFormula, "Exam ~ Test + Gender")
  testthat::expect_equal(report$metadata$responseVariable, "Exam")
  testthat::expect_equal(report$metadata$predictors, c("Test", "Gender"))
  testthat::expect_equal(report$context$researchQuestion, "Does Test predict Exam?")
  testthat::expect_null(report$context$otherIssues)
  testthat::expect_equal(report$context$datasetSummary$nRows, 4L)
  testthat::expect_length(report$sentenceRecords, 2L)
  testthat::expect_true(report$sentenceRecords[[1]]$isMarkedIncorrect)
  testthat::expect_equal(report$sentenceRecords[[1]]$userComment, "Wrong scale.")
  testthat::expect_false(report$sentenceRecords[[2]]$isMarkedIncorrect)
  testthat::expect_null(report$sentenceRecords[[2]]$userComment)
})


testthat::test_that("developer feedback report captures general debugging notes", {
  d = data.frame(
    Exam = c(60, 70, 80, 90),
    Test = c(55, 65, 75, 85)
  )
  m = stats::lm(Exam ~ Test, data = d)

  claimMap = list(
    claims = data.frame(
      sentenceIndex = 1L,
      claimText = "Exam tends to increase with Test.",
      claimTags = I(list("effect")),
      stringsAsFactors = FALSE
    )
  )

  report = buildDeveloperFeedbackReport(
    model = m,
    claimMap = claimMap,
    input = list(),
    data = d,
    otherIssues = "The answer sentence should be checked against the research question."
  )

  testthat::expect_equal(
    report$context$otherIssues,
    "The answer sentence should be checked against the research question."
  )
})

testthat::test_that("developer feedback report handles missing comments", {
  d = data.frame(
    Pass = factor(c(
      "Fail", "Fail", "Pass", "Fail",
      "Pass", "Pass", "Fail", "Pass"
    )),
    Assign = c(38, 45, 49, 55, 61, 67, 72, 81)
  )
  m = stats::glm(Pass ~ Assign, data = d, family = stats::binomial())

  claimMap = list(
    claims = data.frame(
      sentenceIndex = 1L,
      claimText = "The fitted model is on the log-odds scale.",
      claimTags = I(list("scale")),
      stringsAsFactors = FALSE
    )
  )

  report = buildDeveloperFeedbackReport(
    model = m,
    claimMap = claimMap,
    input = list(developerFeedbackIncorrect_1 = TRUE),
    data = d
  )

  testthat::expect_equal(report$metadata$modelType, "glm-binomial")
  testthat::expect_true(report$sentenceRecords[[1]]$isMarkedIncorrect)
  testthat::expect_null(report$sentenceRecords[[1]]$userComment)
})

testthat::test_that("developer feedback report handles no incorrect entries", {
  d = data.frame(
    Freq = c(10, 6, 2, 1),
    Magnitude = c(4, 5, 6, 7),
    Locn = factor(c("SC", "SC", "WA", "WA"))
  )
  m = stats::glm(Freq ~ Magnitude + Locn, data = d, family = stats::poisson())

  claimMap = list(
    claims = data.frame(
      sentenceIndex = c(1L, 2L),
      claimText = c(
        "The expected count changes with Magnitude.",
        "The model includes location."
      ),
      claimTags = I(list("effect", "context")),
      stringsAsFactors = FALSE
    )
  )

  report = buildDeveloperFeedbackReport(
    model = m,
    claimMap = claimMap,
    input = list(),
    data = d
  )

  testthat::expect_equal(report$metadata$modelType, "glm-poisson")
  testthat::expect_equal(report$metadata$responseVariable, "Freq")
  testthat::expect_equal(report$metadata$predictors, c("Magnitude", "Locn"))
  testthat::expect_true(all(vapply(
    report$sentenceRecords,
    function(record) {
      !isTRUE(record$isMarkedIncorrect) && is.null(record$userComment)
    },
    logical(1)
  )))
})

testthat::test_that("developer feedback report supports wmfmModel inputs", {
  d = data.frame(
    Exam = c(60, 70, 80, 90),
    Test = c(55, 65, 75, 85)
  )
  m = stats::lm(Exam ~ Test, data = d)

  wmfm = newWmfmModel(
    model = m,
    formula = stats::formula(m),
    modelType = "gaussian",
    data = d,
    researchQuestion = "Does Test predict Exam?",
    explanation = "Exam tends to increase with Test."
  )

  claimMap = list(
    claims = data.frame(
      sentenceIndex = 1L,
      claimText = "Exam tends to increase with Test.",
      claimTags = I(list("effect")),
      stringsAsFactors = FALSE
    )
  )

  report = buildDeveloperFeedbackReport(
    model = wmfm,
    claimMap = claimMap,
    input = list()
  )

  testthat::expect_equal(report$metadata$modelType, "gaussian")
  testthat::expect_equal(report$context$researchQuestion, "Does Test predict Exam?")
  testthat::expect_equal(report$explanation$fullText, "Exam tends to increase with Test.")
})

testthat::test_that("developer feedback report converts to JSON", {
  report = list(
    metadata = list(
      modelType = "lm",
      modelFormula = "Exam ~ Test"
    ),
    context = list(
      researchQuestion = NULL
    ),
    sentenceRecords = list(
      list(
        sentenceId = 1L,
        sentenceText = "Exam tends to increase with Test.",
        isMarkedIncorrect = FALSE,
        userComment = NULL
      )
    )
  )

  json = developerFeedbackReportToJson(report)
  parsed = jsonlite::fromJSON(json, simplifyVector = FALSE)

  testthat::expect_match(json, "\\n", fixed = FALSE)
  testthat::expect_equal(parsed$metadata$modelType, "lm")
  testthat::expect_null(parsed$context$researchQuestion)
  testthat::expect_false(parsed$sentenceRecords[[1]]$isMarkedIncorrect)
})

testthat::test_that("developer feedback report writes JSON to file", {
  report = list(
    metadata = list(modelType = "glm-binomial"),
    sentenceRecords = list()
  )
  file = tempfile(fileext = ".json")

  result = writeDeveloperFeedbackReportJson(report = report, file = file)
  parsed = jsonlite::fromJSON(file, simplifyVector = FALSE)

  testthat::expect_identical(result, file)
  testthat::expect_true(file.exists(file))
  testthat::expect_equal(parsed$metadata$modelType, "glm-binomial")
})
