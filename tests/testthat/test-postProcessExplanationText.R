testthat::test_that("postProcessExplanationText preserves cleanExplanationText behaviour", {
  testthat::expect_identical(
    postProcessExplanationText("Answer: On average, exam marks increase."),
    "On average, exam marks increase."
  )

  testthat::expect_identical(postProcessExplanationText(NA_character_), NA_character_)
  testthat::expect_null(postProcessExplanationText(NULL))
})


testthat::test_that("postProcessExplanationText standardises unit-change wording", {
  text = "A one-magnitude rise multiplies the expected count by 0.21."

  out = postProcessExplanationText(text)

  testthat::expect_identical(
    out,
    "If the magnitude increases by one, the expected count is multiplied by 0.21."
  )
  testthat::expect_false(grepl("one-magnitude rise", out, ignore.case = TRUE))
  testthat::expect_true(grepl("0.21", out, fixed = TRUE))
})


testthat::test_that("postProcessExplanationText handles additional unit-change variants", {
  text = paste(
    "For a one-unit increase in score, the expected mark increases by 2.3.",
    "A one-unit rise in age multiplies the odds by 1.4."
  )

  out = postProcessExplanationText(text)

  testthat::expect_identical(
    out,
    paste(
      "If score increases by one unit, the expected mark increases by 2.3.",
      "If age increases by one unit, the odds are multiplied by 1.4."
    )
  )
  testthat::expect_false(grepl("one-unit increase|one-unit rise", out, ignore.case = TRUE))
  testthat::expect_true(grepl("2.3", out, fixed = TRUE))
  testthat::expect_true(grepl("1.4", out, fixed = TRUE))
})


testthat::test_that("postProcessExplanationText removes verbal fractions", {
  text = paste(
    "The lower count is only one-third of the original value.",
    "The upper count is about three-quarters of the original value."
  )

  out = postProcessExplanationText(text)

  testthat::expect_false(grepl("one-third|three-quarters", out, ignore.case = TRUE))
  testthat::expect_true(grepl("about 33%", out, fixed = TRUE))
  testthat::expect_true(grepl("about 75%", out, fixed = TRUE))
})


testthat::test_that("postProcessExplanationText reduces recurring model-mechanism language", {
  text = paste(
    "The interaction term makes the decrease with magnitude steeper in Washington than in California.",
    "The coefficient is negative in the fitted model."
  )

  out = postProcessExplanationText(text)

  testthat::expect_false(grepl("interaction term", out, ignore.case = TRUE))
  testthat::expect_false(grepl("coefficient", out, ignore.case = TRUE))
  testthat::expect_false(grepl("fitted model", out, ignore.case = TRUE))
  testthat::expect_true(grepl("decrease with magnitude is steeper", out, fixed = TRUE))
})


testthat::test_that("postProcessExplanationText reduces additional technical leakage", {
  text = paste(
    "The slope for score shows that marks increase.",
    "The parameter is positive in the regression model."
  )

  out = postProcessExplanationText(text)

  testthat::expect_false(grepl("slope", out, ignore.case = TRUE))
  testthat::expect_false(grepl("parameter", out, ignore.case = TRUE))
  testthat::expect_false(grepl("regression model", out, ignore.case = TRUE))
  testthat::expect_true(grepl("score marks increase", out, fixed = TRUE))
})


testthat::test_that("postProcessExplanationText splits supported long sentence patterns", {
  text = "For a student with score 12, the expected mark is 65.2, with values between 60.1 and 70.3."

  out = postProcessExplanationText(text)

  testthat::expect_identical(
    out,
    "For a student with score 12, the expected mark is 65.2. This estimate could plausibly lie between 60.1 and 70.3."
  )
  testthat::expect_true(grepl("12", out, fixed = TRUE))
  testthat::expect_true(grepl("65.2", out, fixed = TRUE))
  testthat::expect_true(grepl("60.1", out, fixed = TRUE))
  testthat::expect_true(grepl("70.3", out, fixed = TRUE))
})


testthat::test_that("postProcessExplanationText splits additional confidence sentence patterns", {
  text = paste(
    "The expected count is 6.8, with limits from 5.1 to 8.3.",
    "The odds ratio is 1.42, with a 95% confidence interval between 1.10 and 1.82."
  )

  out = postProcessExplanationText(text)

  testthat::expect_identical(
    out,
    paste(
      "The expected count is 6.8. The confidence limits run from 5.1 to 8.3.",
      "The odds ratio is 1.42. The 95% confidence interval runs from 1.10 to 1.82."
    )
  )
})


testthat::test_that("postProcessExplanationText leaves already clean text unchanged", {
  text = paste(
    "The expected mark is 65.2.",
    "The 95% confidence interval runs from 60.1 to 70.3."
  )

  out = postProcessExplanationText(text)

  testthat::expect_identical(out, text)
})


testthat::test_that("postProcessExplanationText preserves numeric tokens during supported rewrites", {
  text = paste(
    "A one-magnitude rise multiplies the expected count by 0.21, with values between 0.13 and 0.31.",
    "The upper count is about three-quarters of the original value."
  )

  out = postProcessExplanationText(text)

  expectedNumbers = c("0.21", "0.13", "0.31", "75%")
  for (numberText in expectedNumbers) {
    testthat::expect_true(grepl(numberText, out, fixed = TRUE))
  }
})


testthat::test_that("postProcessExplanationText debug mode reports changed rules", {
  text = "Answer: A one-magnitude rise multiplies the expected count by 0.21."

  out = postProcessExplanationText(text, debug = TRUE)

  testthat::expect_named(out, c("original", "processed", "rulesApplied"))
  testthat::expect_identical(out$original, text)
  testthat::expect_identical(
    out$processed,
    "If the magnitude increases by one, the expected count is multiplied by 0.21."
  )
  testthat::expect_true("cleanExplanationText" %in% out$rulesApplied)
  testthat::expect_true("unitChangePhrasing" %in% out$rulesApplied)
})


testthat::test_that("postProcessExplanationText rejects invalid inputs", {
  testthat::expect_error(
    postProcessExplanationText(1),
    "`text` must be a character vector or NULL.",
    fixed = TRUE
  )

  testthat::expect_error(
    postProcessExplanationText("Text", debug = NA),
    "`debug` must be TRUE or FALSE.",
    fixed = TRUE
  )
})


testthat::test_that("postProcessExplanationText guards against numeric token loss in rules", {
  text = "The expected mark is 65.2, with values between 60.1 and 70.3."

  out = postProcessApplyRule(
    text = text,
    ruleName = "badRule",
    ruleFunction = function(x) {
      gsub("65.2", "", x, fixed = TRUE)
    },
    rulesApplied = character(0)
  )

  testthat::expect_identical(out$text, text)
  testthat::expect_identical(out$rulesApplied, character(0))
})


testthat::test_that("postProcessExplanationText handles additional confidence interval wording", {
  text = paste(
    "The expected mark is 65.2, with confidence limits from 60.1 to 70.3.",
    "The estimated change is 2.4, and the 95% confidence interval is 1.2 to 3.6."
  )

  out = postProcessExplanationText(text)

  testthat::expect_identical(
    out,
    paste(
      "The expected mark is 65.2. The confidence limits run from 60.1 to 70.3.",
      "The estimated change is 2.4. The 95% confidence interval runs from 1.2 to 3.6."
    )
  )
})


testthat::test_that("postProcessExplanationText cleans small grammar artefacts", {
  text = "A one-unit increase in age multiplies the odds by 1.4."

  out = postProcessExplanationText(text)

  testthat::expect_identical(
    out,
    "If age increases by one unit, the odds are multiplied by 1.4."
  )
  testthat::expect_false(grepl("odds is", out, fixed = TRUE))
})


testthat::test_that("postProcessExplanationText improves model-centred sentence openings", {
  text = paste(
    "In a model that predicts exam mark, the relationship between score and mark is positive.",
    "Based on the fitted model, the expected mark is 65.2."
  )

  out = postProcessExplanationText(text)

  testthat::expect_identical(
    out,
    paste(
      "The relationship between score and mark is positive.",
      "Based on the results, the expected mark is 65.2."
    )
  )
  testthat::expect_false(grepl("model that predicts|fitted model", out, ignore.case = TRUE))
  testthat::expect_true(grepl("65.2", out, fixed = TRUE))
})


testthat::test_that("postProcessExplanationText rewrites simple model prediction sentences", {
  text = "The model predicts 65.2 for a student with score 12."

  out = postProcessExplanationText(text)

  testthat::expect_identical(
    out,
    "For a student with score 12, the expected value is 65.2."
  )
  testthat::expect_false(grepl("The model predicts", out, fixed = TRUE))
  testthat::expect_true(grepl("65.2", out, fixed = TRUE))
  testthat::expect_true(grepl("12", out, fixed = TRUE))
})


testthat::test_that("postProcessExplanationText debug mode reports sentence opening rewrites", {
  text = "In this model, the relationship between score and mark is positive."

  out = postProcessExplanationText(text, debug = TRUE)

  testthat::expect_identical(
    out$processed,
    "The relationship between score and mark is positive."
  )
  testthat::expect_true("sentenceOpenings" %in% out$rulesApplied)
})


testthat::test_that("findExplanationSurfaceIssues reports known surface issues", {
  text = c(
    "The interaction term shows that a one-magnitude rise matters.",
    "The lower count is only one-third of the original value.",
    "The estimate is 2.4, with values between 1.2 and 3.6."
  )

  out = findExplanationSurfaceIssues(text)

  testthat::expect_s3_class(out, "data.frame")
  testthat::expect_named(out, c("element", "issueType", "pattern", "matchedText"))
  testthat::expect_true("modelMechanismLanguage" %in% out$issueType)
  testthat::expect_true("unitChangePhrasing" %in% out$issueType)
  testthat::expect_true("verbalFractions" %in% out$issueType)
  testthat::expect_true("longSentencePatterns" %in% out$issueType)
  testthat::expect_true(any(out$matchedText == "interaction term"))
  testthat::expect_true(any(out$matchedText == "one-magnitude rise"))
  testthat::expect_true(any(out$matchedText == "one-third"))
})


testthat::test_that("findExplanationSurfaceIssues returns empty schema for clean text", {
  text = paste(
    "The expected mark is 65.2.",
    "The 95% confidence interval runs from 60.1 to 70.3."
  )

  out = findExplanationSurfaceIssues(text)

  testthat::expect_s3_class(out, "data.frame")
  testthat::expect_equal(nrow(out), 0)
  testthat::expect_named(out, c("element", "issueType", "pattern", "matchedText"))
})


testthat::test_that("findExplanationSurfaceIssues can scan selected issue groups", {
  text = paste(
    "The coefficient is positive.",
    "The lower value is one-third of the original value."
  )

  out = findExplanationSurfaceIssues(text, issueTypes = "verbalFractions")

  testthat::expect_equal(unique(out$issueType), "verbalFractions")
  testthat::expect_true(any(out$matchedText == "one-third"))
  testthat::expect_false(any(grepl("coefficient", out$matchedText, fixed = TRUE)))
})


testthat::test_that("findExplanationSurfaceIssues rejects invalid inputs", {
  testthat::expect_error(
    findExplanationSurfaceIssues(1),
    "`text` must be a character vector or NULL.",
    fixed = TRUE
  )

  testthat::expect_error(
    findExplanationSurfaceIssues("Text", issueTypes = character(0)),
    "`issueTypes` must be a non-empty character vector.",
    fixed = TRUE
  )

  testthat::expect_error(
    findExplanationSurfaceIssues("Text", issueTypes = "notSupported"),
    "Unsupported issue type: notSupported.",
    fixed = TRUE
  )
})


testthat::test_that("diagnoseExplanationSurfaceProcessing compares issues before and after processing", {
  text = paste(
    "The interaction term shows that a one-magnitude rise matters.",
    "The lower count is only one-third of the original value.",
    "The estimate is 2.4, with values between 1.2 and 3.6."
  )

  out = diagnoseExplanationSurfaceProcessing(text)

  testthat::expect_s3_class(out, "wmfmExplanationSurfaceDiagnosis")
  testthat::expect_named(
    out,
    c(
      "original",
      "processed",
      "rulesApplied",
      "issuesBefore",
      "issuesAfter",
      "unresolvedIssueCount"
    )
  )
  testthat::expect_identical(out$original, text)
  testthat::expect_type(out$processed, "character")
  testthat::expect_true(nrow(out$issuesBefore) >= 4)
  testthat::expect_true(nrow(out$issuesAfter) < nrow(out$issuesBefore))
  testthat::expect_identical(out$unresolvedIssueCount, nrow(out$issuesAfter))
  testthat::expect_true("unitChangePhrasing" %in% out$rulesApplied)
  testthat::expect_true("verbalFractions" %in% out$rulesApplied)
})


testthat::test_that("diagnoseExplanationSurfaceProcessing reports no changes for clean text", {
  text = paste(
    "The expected mark is 65.2.",
    "The 95% confidence interval runs from 60.1 to 70.3."
  )

  out = diagnoseExplanationSurfaceProcessing(text)

  testthat::expect_s3_class(out, "wmfmExplanationSurfaceDiagnosis")
  testthat::expect_identical(out$original, text)
  testthat::expect_identical(out$processed, text)
  testthat::expect_identical(out$rulesApplied, character(0))
  testthat::expect_equal(nrow(out$issuesBefore), 0)
  testthat::expect_equal(nrow(out$issuesAfter), 0)
  testthat::expect_identical(out$unresolvedIssueCount, 0L)
})


testthat::test_that("diagnoseExplanationSurfaceProcessing handles null and invalid input", {
  out = diagnoseExplanationSurfaceProcessing(NULL)

  testthat::expect_s3_class(out, "wmfmExplanationSurfaceDiagnosis")
  testthat::expect_null(out$original)
  testthat::expect_null(out$processed)
  testthat::expect_identical(out$rulesApplied, character(0))
  testthat::expect_equal(nrow(out$issuesBefore), 0)
  testthat::expect_equal(nrow(out$issuesAfter), 0)
  testthat::expect_identical(out$unresolvedIssueCount, 0L)

  testthat::expect_error(
    diagnoseExplanationSurfaceProcessing(1),
    "`text` must be a character vector or NULL.",
    fixed = TRUE
  )
})


testthat::test_that("print.wmfmExplanationSurfaceDiagnosis returns input invisibly", {
  out = diagnoseExplanationSurfaceProcessing(
    "A one-magnitude rise multiplies the expected count by 0.21."
  )

  printed = NULL
  testthat::expect_output(
    {
      printed = print(out)
    },
    "Explanation surface diagnosis"
  )
  testthat::expect_identical(printed, out)
})


testthat::test_that("postProcessExplanationText replaces additional verbal fractions", {
  text = paste(
    "The smaller count is one-fifth of the original value.",
    "The larger count is about four-fifths of the original value."
  )

  out = postProcessExplanationText(text)

  testthat::expect_false(grepl("one-fifth|four-fifths", out, ignore.case = TRUE))
  testthat::expect_true(grepl("about 20%", out, fixed = TRUE))
  testthat::expect_true(grepl("about 80%", out, fixed = TRUE))
})


testthat::test_that("postProcessExplanationText rewrites multiplicative confidence interval language", {
  text = paste(
    "This is a multiplicative confidence interval for the odds ratio.",
    "The multiplicative confidence limits run from 0.13 to 0.31."
  )

  out = postProcessExplanationText(text)

  testthat::expect_false(grepl("multiplicative confidence", out, ignore.case = TRUE))
  testthat::expect_true(grepl("confidence interval for the multiplier", out, fixed = TRUE))
  testthat::expect_true(grepl("confidence limits for the multiplier", out, fixed = TRUE))
  testthat::expect_true(grepl("0.13", out, fixed = TRUE))
  testthat::expect_true(grepl("0.31", out, fixed = TRUE))
})


testthat::test_that("postProcessExplanationText splits confidence ranges with scientific notation", {
  text = "The estimated multiplier is 1.2e-03, with values between 1.0e-03 and 1.4E-03."

  out = postProcessExplanationText(text)

  testthat::expect_identical(
    out,
    "The estimated multiplier is 1.2e-03. This estimate could plausibly lie between 1.0e-03 and 1.4E-03."
  )
  testthat::expect_true(grepl("1.2e-03", out, fixed = TRUE))
  testthat::expect_true(grepl("1.0e-03", out, fixed = TRUE))
  testthat::expect_true(grepl("1.4E-03", out, fixed = TRUE))
})


testthat::test_that("findExplanationSurfaceIssues reports new surface issue groups", {
  text = paste(
    "The value is one-fifth of the original value.",
    "This is a multiplicative confidence interval."
  )

  out = findExplanationSurfaceIssues(text)

  testthat::expect_true("verbalFractions" %in% out$issueType)
  testthat::expect_true("confidenceIntervalTerminology" %in% out$issueType)
  testthat::expect_true(any(out$matchedText == "one-fifth"))
  testthat::expect_true(any(out$matchedText == "multiplicative confidence interval"))
})


testthat::test_that("diagnoseExplanationSurfaceProcessing resolves new diagnostic examples", {
  text = paste(
    "This is a multiplicative confidence interval.",
    "The count is one-fifth of the original value."
  )

  out = diagnoseExplanationSurfaceProcessing(text)

  testthat::expect_true("confidenceIntervalTerminology" %in% out$rulesApplied)
  testthat::expect_true("verbalFractions" %in% out$rulesApplied)
  testthat::expect_true(nrow(out$issuesAfter) < nrow(out$issuesBefore))
})
