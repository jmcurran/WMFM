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


testthat::test_that("example records keep package-relative paths for future reorganisation", {
  examplesPath = file.path(tempdir(), "wmfm-example-path-test")
  unlink(examplesPath, recursive = TRUE, force = TRUE)
  dir.create(file.path(examplesPath, "developer", "NestedFixture"), recursive = TRUE)

  writeLines(
    c(
      "modelType: lm",
      "formula: y ~ x",
      "displayName: Nested Fixture"
    ),
    file.path(examplesPath, "developer", "NestedFixture", "NestedFixture.spec.yml")
  )

  specFiles = list.files(
    path = examplesPath,
    pattern = "\\.spec\\.yml$",
    recursive = TRUE,
    full.names = FALSE
  )
  records = buildWMFMExampleRecords(
    examplesPath = examplesPath,
    specFiles = specFiles,
    exampleMetadata = list()
  )
  matchedRecord = findWMFMExampleRecord(
    requestedName = "Nested Fixture",
    exampleRecords = records
  )

  testthat::expect_identical(records$exampleDir, "NestedFixture")
  testthat::expect_identical(records$examplePath, "developer/NestedFixture")
  testthat::expect_identical(records$specFile, "developer/NestedFixture/NestedFixture.spec.yml")
  testthat::expect_identical(matchedRecord$examplePath, "developer/NestedFixture")
})


testthat::test_that("example detail listing exposes metadata-backed fields", {
  visibleDetails = listWMFMExampleDetails(package = "WMFM")
  developerDetails = listWMFMExampleDetails(
    package = "WMFM",
    includeTestExamples = TRUE
  )

  expectedColumns = c(
    "exampleDir",
    "examplePath",
    "specFile",
    "exampleName",
    "exampleAudience",
    "exampleFamily",
    "exampleDifficulty",
    "teachingTopic",
    "developerPurpose"
  )

  testthat::expect_true(all(expectedColumns %in% names(visibleDetails)))
  testthat::expect_true("Diamonds II" %in% visibleDetails$exampleName)
  testthat::expect_true("test-SG-1" %in% developerDetails$exampleName)
  testthat::expect_false("test-SG-1" %in% visibleDetails$exampleName)

  courseRow = visibleDetails[visibleDetails$exampleName == "Course", , drop = FALSE]
  sgRow = developerDetails[developerDetails$exampleName == "test-SG-1", , drop = FALSE]

  testthat::expect_identical(courseRow$exampleAudience, "classroom")
  testthat::expect_identical(courseRow$exampleFamily, "lm")
  testthat::expect_identical(courseRow$exampleDifficulty, "introductory")
  testthat::expect_match(courseRow$teachingTopic, "Multiple regression")
  testthat::expect_identical(sgRow$developerPurpose, "Scoring and grading fixture")
})


testthat::test_that("empty example detail listing has stable columns", {
  examplesPath = file.path(tempdir(), "wmfm-empty-example-details-test")
  unlink(examplesPath, recursive = TRUE, force = TRUE)
  dir.create(examplesPath, recursive = TRUE)

  records = buildWMFMExampleRecords(
    examplesPath = examplesPath,
    specFiles = character(0),
    exampleMetadata = list()
  )

  testthat::expect_equal(nrow(records), 0)
  testthat::expect_true("exampleFamily" %in% names(records))
  testthat::expect_true("teachingTopic" %in% names(records))
})
