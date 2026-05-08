findDeveloperScoringFixtureDir = function(stage = "stage18-9") {
  packageRoot = findPackageRootForSourceTests()

  if (is.na(packageRoot)) {
    testthat::skip("Package source tree is not available in this test environment")
  }

  fixtureDir = file.path(
    packageRoot,
    "inst",
    "extdata",
    "developer-scoring",
    stage
  )

  if (!dir.exists(fixtureDir)) {
    testthat::skip(paste0("Developer scoring fixture directory is not available: ", fixtureDir))
  }

  fixtureDir
}

listDeveloperScoringFixtureFiles = function(stage = "stage18-9") {
  fixtureDir = findDeveloperScoringFixtureDir(stage = stage)
  files = list.files(
    fixtureDir,
    pattern = "[.]json$",
    full.names = TRUE
  )

  if (length(files) == 0) {
    testthat::skip("Developer scoring JSON fixtures are not available")
  }

  sort(files)
}

readDeveloperScoringFixture = function(file) {
  jsonlite::fromJSON(file, simplifyVector = FALSE)
}

readDeveloperScoringFixtures = function(stage = "stage18-9") {
  files = listDeveloperScoringFixtureFiles(stage = stage)
  fixtures = lapply(files, readDeveloperScoringFixture)
  names(fixtures) = vapply(
    seq_along(fixtures),
    function(i) {
      exampleName = fixtures[[i]]$appState$exampleName

      if (is.null(exampleName) || !nzchar(exampleName)) {
        return(basename(files[[i]]))
      }

      exampleName
    },
    character(1)
  )
  fixtures
}

getDeveloperScoringRunMarks = function(fixture) {
  vapply(
    fixture$repeated$runTable,
    function(x) {
      as.numeric(x$mark)
    },
    numeric(1)
  )
}

getDeveloperScoringRunOverallScores = function(fixture) {
  vapply(
    fixture$repeated$runTable,
    function(x) {
      as.numeric(x$overallScore)
    },
    numeric(1)
  )
}

getDeveloperScoringMetricValue = function(run, label) {
  metrics = run$grade$metrics
  matches = vapply(
    metrics,
    function(x) {
      identical(x$label, label)
    },
    logical(1)
  )

  if (!any(matches)) {
    return(NA_real_)
  }

  as.numeric(metrics[[which(matches)[1]]]$studentValue)
}
