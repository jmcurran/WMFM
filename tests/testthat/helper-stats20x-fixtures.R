loadStats20xFixture = function() {
  path = system.file("extdata", "STATS20x.txt", package = "WMFM")

  if (!nzchar(path)) {
    path = testthat::test_path("..", "..", "inst", "extdata", "STATS20x.txt")
  }

  utils::read.table(
    path,
    header = TRUE,
    stringsAsFactors = FALSE
  )
}

getStats20xExamTestData = function() {
  df = loadStats20xFixture()

  data.frame(
    Exam = as.numeric(df$Exam),
    Test = as.numeric(df$Test),
    Gender = factor(trimws(df$Gender)),
    stringsAsFactors = FALSE
  )
}
