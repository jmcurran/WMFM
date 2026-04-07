testthat::test_that("runExample can be unit-tested offline by mocking the expensive boundary", {
  fakeTracker = new.env(parent = emptyenv())
  fakeTracker$steps = integer(0)

  testthat::local_mocked_bindings(
    loadExampleSpec = function(name, package) {
      list(
        spec = list(
          formula = "Exam ~ Attend",
          modelType = "lm"
        ),
        data = data.frame(Exam = c(10, 12), Attend = c(0, 1)),
        dataContext = "Synthetic context"
      )
    },
    runWMFMModelDebug = function(...) {
      list(
        explanation = "Attendance is associated with higher exam scores.",
        equations = "Exam = 6.62 + 3.52 * Attend",
        interactionTerms = character(0),
        interactionMinPValue = NA_real_
      )
    },
    newWmfmProgressTracker = function(nSteps, showProgress, label) {
      list(nSteps = nSteps, showProgress = showProgress, label = label)
    },
    updateWmfmProgressTracker = function(tracker, step, stepSeconds) {
      fakeTracker$steps = c(fakeTracker$steps, step)
      invisible(NULL)
    },
    closeWmfmProgressTracker = function(tracker) {
      list(
        startedAt = as.character(Sys.time()),
        finishedAt = as.character(Sys.time()),
        elapsedSeconds = 0.2,
        averageIterationSeconds = 0.1,
        iterationSeconds = c(0.1, 0.1)
      )
    },
    .package = "WMFM"
  )

  out = runExample(
    name = "Course",
    package = "WMFM",
    nRuns = 2,
    showProgress = FALSE
  )

  testthat::expect_s3_class(out, "wmfmRuns")
  testthat::expect_length(out$runs, 2)
  testthat::expect_identical(out$meta$nRuns, 2L)
  testthat::expect_false(any(c("overallScore", "llmScored") %in% names(out$runs[[1]])))
})
