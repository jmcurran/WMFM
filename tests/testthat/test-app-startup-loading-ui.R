test_that("startup data choice loading gives immediate visible feedback", {
  serverText = readPackageText("R", "app-server.R")
  startupHelperText = readPackageText("R", "app-server-startup-helpers.R")
  startupObserverText = readPackageText("R", "app-server-startup.R")
  startupText = paste(serverText, startupHelperText, startupObserverText, sep = "\n")
  uiText = readPackageText("R", "app-ui.R")

  expect_match(startupText, "Loading examples\\.\\.\\.", fixed = FALSE)
  expect_match(uiText, "Loading examples...", fixed = TRUE)
  expect_match(uiText, "Loading packages...", fixed = TRUE)
  expect_match(startupObserverText, "Checking installed packages for datasets", fixed = TRUE)
  expect_match(uiText, "Loading datasets...", fixed = TRUE)
  expect_match(startupText, "Dataset choices will appear once the package scan has finished", fixed = TRUE)
  expect_match(startupObserverText, "wmfm-startup-data-choices", fixed = TRUE)
  expect_match(startupObserverText, "removeNotification\\(startupNotificationId\\)", fixed = FALSE)
})

test_that("package dataset selector shows a loading placeholder before scanning", {
  startupObserverText = readPackageText("R", "app-server-startup.R")

  expect_match(startupObserverText, "Loading datasets\\.\\.\\.", fixed = FALSE)
  expect_match(startupObserverText, "wmfm-package-dataset-list", fixed = TRUE)
  expect_match(startupObserverText, "session\\$onFlushed\\(function\\(\\) \\{", fixed = FALSE)
})

test_that("model outputs use direct tabs without a duplicate summary-table header", {
  uiText = readPackageText("R", "app-ui.R")

  expect_match(uiText, "Model outputs", fixed = TRUE)
  expect_match(uiText, "id = \"model_output_tabs\"", fixed = TRUE)
  expect_false(grepl("Summary table", uiText, fixed = TRUE))
  expect_false(grepl("id = \"model_outputs\"", uiText, fixed = TRUE))
  expect_false(grepl("Regression output", uiText, fixed = TRUE))
})

test_that("model type uses a compact selector", {
  uiText = readPackageText("R", "app-ui.R")

  expect_match(uiText, "selectInput\\(\\s*\\\"model_type\\\"", perl = TRUE)
  expect_false(grepl("radioButtons\\(\\s*\\\"model_type\\\"", uiText, perl = TRUE))
})

test_that("example loading updates the compact model type selector", {
  serverText = readPackageText("R", "app-server.R")
  stateHelperText = readPackageText("R", "app-server-state-helpers.R")
  serverAndStateHelperText = paste(serverText, stateHelperText, sep = "\n")

  expect_match(
    serverAndStateHelperText,
    'updateSelectInput\\(\\s*session,\\s*"model_type"',
    perl = TRUE
  )
  expect_false(grepl(
    'updateRadioButtons\\(\\s*session,\\s*"model_type"',
    serverAndStateHelperText,
    perl = TRUE
  ))
})

test_that("model formula section keeps validation message and expert checkbox visible", {
  uiText = readPackageText("R", "app-ui.R")
  fitModelObserverText = readPackageText("R", "app-server-fit-model.R")

  expect_match(uiText, "h5\\(\"Model formula\"\\)", perl = TRUE)
  expect_match(uiText, "verbatimTextOutput\\(\"formula_status\"\\)", perl = TRUE)
  expect_match(uiText, "checkboxInput\\(\\s*\"expert_mode\"", perl = TRUE)
  expect_match(fitModelObserverText, "output\\$formula_status\\s*=\\s*renderText", perl = TRUE)
  expect_match(fitModelObserverText, "Formula OK\\.", perl = TRUE)
  expect_match(uiText, "\\.tab-content \\{\\\\n        overflow: visible;", perl = TRUE)
  expect_equal(sum(gregexpr("textInput\\(\"formula_text\"", uiText, perl = TRUE)[[1]] != -1), 1)
})

test_that("model tab scrolls normally and stacks fit controls", {
  uiText = readPackageText("R", "app-ui.R")

  expect_match(uiText, "html, body \\{\\\\n        min-height: 100%;\\\\n        overflow-y: auto;", perl = TRUE)
  expect_false(grepl("min-height: 100vh", uiText, fixed = TRUE))
  expect_match(uiText, "\\.wmfm-model-fit-buttons \\{\\\\n        display: flex;", perl = TRUE)
  expect_match(uiText, "flex-direction: column", fixed = TRUE)
  expect_match(uiText, "gap: 8px", fixed = TRUE)
  expect_false(grepl("height: 8px", uiText, fixed = TRUE))
})
