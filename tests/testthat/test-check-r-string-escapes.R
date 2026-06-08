test_that("R string escape scanner flags common accidental regex escapes", {
  source(findWmfmProjectFile("scripts", "checkRStringEscapes.R"), local = TRUE)

  badText = paste(
    "x = paste0(\"cost\")",
    paste0("pattern = ", '"', "\\", "$", '"'),
    sep = "\n"
  )

  findings = scanRStringEscapesInText(badText, path = "bad.R")

  expect_equal(nrow(findings), 1)
  expect_equal(findings$file, "bad.R")
  expect_equal(findings$line, 2)
  expect_equal(findings$escape, "\\$")
})

test_that("R string escape scanner ignores escaped backslashes and raw strings", {
  source(findWmfmProjectFile("scripts", "checkRStringEscapes.R"), local = TRUE)

  safeText = paste(
    paste0("pattern = ", '"', "\\", "\\", "$", '"'),
    paste0("rawPattern = r\"(", "\\", "$", ")\""),
    "literalMatch = grepl(\"$\", x, fixed = TRUE)",
    sep = "\n"
  )

  findings = scanRStringEscapesInText(safeText, path = "safe.R")

  expect_equal(nrow(findings), 0)
})

test_that("R string escape scanner ignores comments", {
  source(findWmfmProjectFile("scripts", "checkRStringEscapes.R"), local = TRUE)

  commentText = paste0("# pattern = ", '"', "\\", "$", '"')
  findings = scanRStringEscapesInText(commentText, path = "comment.R")

  expect_equal(nrow(findings), 0)
})
