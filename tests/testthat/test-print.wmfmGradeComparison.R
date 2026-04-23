test_that("print.wmfmGradeComparison prints a report-style comparison", {
  wm = makeOfflineWmfmModel()

  parsed = makeValidParsedScores()
  parsed$overallScore = 85
  parsed$llmScoringSummary = "Mock score."

  fakeChat = makeFakeChat(
    responseText = jsonlite::toJSON(parsed, auto_unbox = TRUE, null = "null")
  )

  g = grade(
    wm,
    explanation = paste(
      "Each one-unit increase in x is associated with about",
      "a one-point increase in y."
    ),
    autoScore = FALSE
  )

  g = score(g, method = "deterministic")
  g = score(g, method = "llm", chat = fakeChat, nLlm = 3, showProgress = FALSE)

  comparison = compare(g)

  output = capture.output(print(comparison, digits = 1))
  outputText = paste(output, collapse = "\n")

  expect_true(any(grepl("WMFM grade comparison", output, fixed = TRUE)))
  expect_true(any(grepl("Methods compared", output, fixed = TRUE)))
  expect_true(any(grepl("Mark comparison", output, fixed = TRUE)))
  expect_true(any(grepl("Overall score comparison", output, fixed = TRUE)))
  expect_true(any(grepl("Per-metric comparison", output, fixed = TRUE)))
  expect_true(any(grepl("Deterministic", output, fixed = TRUE)))
  expect_true(any(grepl("LLM (mean of 3 runs)", output, fixed = TRUE)))
  expect_true(any(grepl("Difference (LLM - deterministic)", output, fixed = TRUE)))

  metricLabels = comparison$metricComparison$label
  metricLabels = metricLabels[!is.na(metricLabels) & nzchar(metricLabels)]

  labelPositions = vapply(metricLabels, function(label) {
    regexpr(label, outputText, fixed = TRUE)[1]
  }, numeric(1))

  expect_true(all(labelPositions > 0))
  expect_equal(order(labelPositions), seq_along(labelPositions))
})

test_that("print.wmfmGradeComparison keeps the LLM minus deterministic difference label when methods are reversed", {
  wm = makeOfflineWmfmModel()

  parsed = makeValidParsedScores()
  parsed$overallScore = 85
  parsed$llmScoringSummary = "Mock score."

  fakeChat = makeFakeChat(
    responseText = jsonlite::toJSON(parsed, auto_unbox = TRUE, null = "null")
  )

  g = grade(
    wm,
    explanation = paste(
      "Each one-unit increase in x is associated with about",
      "a one-point increase in y."
    ),
    autoScore = FALSE
  )

  g = score(g, method = "deterministic")
  g = score(g, method = "llm", chat = fakeChat, nLlm = 2, showProgress = FALSE)

  comparison = compare(g, methods = c("llm", "deterministic"))

  output = capture.output(print(comparison, digits = 1))

  expect_true(any(grepl("Difference (LLM - deterministic)", output, fixed = TRUE)))
  expect_true(any(grepl("LLM (mean of 2 runs)", output, fixed = TRUE)))
})
