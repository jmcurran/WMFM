testthat::test_that("plot.wmfmScores returns ggplot objects for supported score views", {
  x = makeLongScoreDf()

  x$clarityScore = c(1.5, 1.6, 1.7, 1.4, 1.5, 1.6)
  x$factualScore = c(1.2, 1.3, 1.4, 1.3, 1.4, 1.5)
  x$inferenceScore = c(1.1, 1.2, 1.3, 1.2, 1.3, 1.4)
  x$completenessScore = c(1.4, 1.5, 1.6, 1.5, 1.6, 1.7)
  x$calibrationScore = c(1.0, 1.1, 1.2, 1.1, 1.2, 1.3)

  scores = makeFakeWmfmScores(x)

  p1 = plot(scores, method = "deterministic", type = "scores")
  p2 = plot(scores, method = "deterministic", type = "summary")
  p3 = plot(scores, method = "deterministic", type = "overall")

  testthat::expect_s3_class(p1, "ggplot")
  testthat::expect_s3_class(p2, "ggplot")
  testthat::expect_s3_class(p3, "ggplot")
})
