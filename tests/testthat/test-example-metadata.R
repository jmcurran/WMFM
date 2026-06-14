testthat::test_that("example metadata separates classroom and developer examples", {
  visibleExamples = listWMFMExamples(package = "WMFM")
  developerExamples = listWMFMExamples(
    package = "WMFM",
    includeTestExamples = TRUE
  )

  testthat::expect_true("Course" %in% visibleExamples)
  testthat::expect_true("Diamonds II" %in% visibleExamples)
  testthat::expect_true("test-SG-1" %in% developerExamples)
  testthat::expect_true("test-arousal-01" %in% developerExamples)
  testthat::expect_false("test-SG-1" %in% visibleExamples)
  testthat::expect_false("test-arousal-01" %in% visibleExamples)
  testthat::expect_true(all(visibleExamples %in% developerExamples))
})


testthat::test_that("example metadata is available as an installed manifest", {
  examplesPath = system.file("extdata", "examples", package = "WMFM")
  metadata = loadWMFMExampleMetadata(examplesPath)

  testthat::expect_true("Course" %in% names(metadata))
  testthat::expect_true("test-SG-1" %in% names(metadata))
  testthat::expect_identical(metadata$Course$exampleAudience, "classroom")
  testthat::expect_identical(metadata$`test-SG-1`$exampleAudience, "developer")
  testthat::expect_identical(metadata$`test-01-G00F`$exampleAudience, "test")
  testthat::expect_identical(metadata$Course$exampleFamily, "lm")
})


testthat::test_that("example audience metadata overrides name-prefix heuristics", {
  examplesPath = file.path(tempdir(), "wmfm-example-metadata-test")
  unlink(examplesPath, recursive = TRUE, force = TRUE)
  dir.create(file.path(examplesPath, "InternalFixture"), recursive = TRUE)
  dir.create(file.path(examplesPath, "ClassroomFixture"), recursive = TRUE)

  writeLines(
    c(
      "modelType: lm",
      "formula: y ~ x"
    ),
    file.path(examplesPath, "InternalFixture", "InternalFixture.spec.yml")
  )
  writeLines(
    c(
      "modelType: lm",
      "formula: y ~ x"
    ),
    file.path(examplesPath, "ClassroomFixture", "ClassroomFixture.spec.yml")
  )
  writeLines(
    c(
      "examples:",
      "  InternalFixture:",
      "    exampleAudience: developer",
      "    displayName: Internal Fixture",
      "  ClassroomFixture:",
      "    exampleAudience: classroom",
      "    displayName: Classroom Fixture"
    ),
    file.path(examplesPath, "example-metadata.yml")
  )

  metadata = loadWMFMExampleMetadata(examplesPath)
  records = buildWMFMExampleRecords(
    examplesPath = examplesPath,
    specFiles = list.files(
      path = examplesPath,
      pattern = "\\.spec\\.yml$",
      recursive = TRUE,
      full.names = FALSE
    ),
    exampleMetadata = metadata
  )
  visibleRecords = records[
    !records$exampleAudience %in% c("developer", "test"),
    ,
    drop = FALSE
  ]

  testthat::expect_true("Internal Fixture" %in% records$exampleName)
  testthat::expect_true("Classroom Fixture" %in% visibleRecords$exampleName)
  testthat::expect_false("Internal Fixture" %in% visibleRecords$exampleName)
})
