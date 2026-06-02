# WMFM Stage 33 example-coupling audit

Generated at: Tue Jun  2 11:05:27 NZST 2026
Branch: example-coupling-audit

## Git status

```text
?? stage33_example_coupling_audit.md
```

## Suspicious example terms in production-ish files

```text
R/methods-score-wmfmRuns.R:14:#' @param useCache Logical. Passed to LLM scoring helpers.
R/methods-score-wmfmRuns.R:17:#' @param verbose Logical. Passed to LLM scoring helpers.
R/methods-stability-wmfmScores.R:31:    "overallPass"
R/methods-stability-wmfmScores.R:44:    "clarityAdequate",
R/methods-stability-wmfmScores.R:53:    "clarityScore",
R/model-question-followup-answer.R:56:    return(buildDeterministicFollowupFailureAnswer(prediction = prediction))
R/model-question-followup-answer.R:247:  hasCourseCue = grepl("\\bscore", text, perl = TRUE) &&
R/model-question-followup-answer.R:253:  isTRUE((hasPredictionCue && (hasFollowupCue || hasCourseCue)) || hasIndividualIntervalCue)
R/model-question-followup-answer.R:283:#' Build deterministic follow-up failure answer text
R/model-question-followup-answer.R:287:#' @return Character scalar answer text, or empty string when no failure should
R/model-question-followup-answer.R:291:buildDeterministicFollowupFailureAnswer = function(prediction) {
R/model-factorOnly.R:22:#' @param hcType Sandwich type passed to \code{sandwich::vcovHC()} (e.g. \code{"HC0"}, \code{"HC3"}).
R/model-glm-notation.R:8:#' as \code{"Pr(Pass = Yes)"} and \code{"Odds(Pass = Yes)"}. The second level
R/model-glm-notation.R:13:#' name, such as \code{"E[Freq]"}.
R/model-glm-notation.R:46:      failureLevel = as.character(levels(response)[1])
R/model-glm-notation.R:53:        failureLevel = observed[1]
R/model-glm-notation.R:56:        failureLevel = "0"
R/model-glm-notation.R:64:      failureLevel = failureLevel,
R/model-glm-notation.R:66:      probabilityFailure = paste0("Pr(", responseName, " = \"", failureLevel, "\")"),
R/model-glm-notation.R:67:      oddsSuccess = paste0("Odds(", responseName, " = \"", successLevel, "\" vs ", responseName, " = \"", failureLevel, "\")"),
R/model-glm-notation.R:68:      oddsFailure = paste0("Odds(", responseName, " = \"", failureLevel, "\" vs ", responseName, " = \"", successLevel, "\")"),
R/model-question-prediction-lm.R:232:  # variable-name punctuation such as parentheses in log(carat), plus signs,
R/model-question-prediction-lm.R:554:  if (predictorNorm %in% c("locn", "location", "region", "state")) {
R/methods-summary-wmfmGrade.R:102:        "clarityScore",
R/prompt-core.R:75:    "- If dataset documentation is provided, use it only to define variables when needed; do not guess course level, study level, or other background details from abbreviated data set names.",
R/prompt-core.R:76:    "- Do not infer course level, study level, or other background details from abbreviated data set names such as s20x.",
R/prompt-core.R:129:    "- When describing expected values, always include a clear noun such as \"expected number\", \"expected count\", or \"expected value\" (e.g. \"expected number of oysters\").",
R/utils-developerModeAuth.R:1:#' Create a developer-mode password hash
R/utils-developerModeAuth.R:3:#' Creates a salted password hash suitable for storing in the
R/utils-developerModeAuth.R:4:#' `WMFM_DEVELOPER_MODE_PASSWORD_HASH` environment variable.
R/utils-developerModeAuth.R:6:#' @param password Character scalar giving the plain-text password.
R/utils-developerModeAuth.R:8:#' @return A salted password hash string.
R/utils-developerModeAuth.R:11:makeDeveloperModePasswordHash = function(password) {
R/utils-developerModeAuth.R:13:    stop("The sodium package is required to create the developer-mode password hash.")
R/utils-developerModeAuth.R:16:  if (!is.character(password) || length(password) != 1 || !nzchar(password)) {
R/utils-developerModeAuth.R:17:    stop("password must be a single non-empty string.")
R/utils-developerModeAuth.R:20:  sodium::password_store(password)
R/utils-developerModeAuth.R:23:#' Store a developer-mode password hash
R/utils-developerModeAuth.R:25:#' Backward-compatible alias for `makeDeveloperModePasswordHash()`.
R/utils-developerModeAuth.R:27:#' @param password Character scalar giving the plain-text password.
R/utils-developerModeAuth.R:29:#' @return A salted password hash string.
R/utils-developerModeAuth.R:32:storeDeveloperModePasswordHash = function(password) {
R/utils-developerModeAuth.R:33:  makeDeveloperModePasswordHash(password)
R/utils-developerModeAuth.R:36:#' Verify the developer-mode password
R/utils-developerModeAuth.R:38:#' Compares a candidate password against the salted hash stored in the
R/utils-developerModeAuth.R:39:#' `WMFM_DEVELOPER_MODE_PASSWORD_HASH` environment variable.
R/utils-developerModeAuth.R:41:#' @param password Character scalar giving the candidate password.
R/utils-developerModeAuth.R:43:#' @return Logical scalar. Returns `TRUE` when the password matches and
R/utils-developerModeAuth.R:47:verifyDeveloperModePassword = function(password) {
R/utils-developerModeAuth.R:49:    stop("The sodium package is required to verify the developer-mode password.")
R/utils-developerModeAuth.R:52:  storedHash = Sys.getenv("WMFM_DEVELOPER_MODE_PASSWORD_HASH", unset = "")
R/utils-developerModeAuth.R:55:    stop("WMFM_DEVELOPER_MODE_PASSWORD_HASH is not set.")
R/utils-developerModeAuth.R:58:  if (!is.character(password) || length(password) != 1 || !nzchar(password)) {
R/utils-developerModeAuth.R:62:  isTRUE(sodium::password_verify(storedHash, password))
R/api-generateBadExplanation.R:7:#' @param ... Additional arguments passed to methods.
R/api-generateBadExplanation.R:18:#' object. The generated explanations are returned in a form that can be passed
R/model-question-adjustment-comparison.R:92:      reason = "reduced_model_failed",
R/model-equation-render.R:7:#' Supported first-pass families are Gaussian identity, binomial logit, and
R/api-compare.R:7:#' @param ... Additional arguments passed to methods.
R/model-logLog.R:91:#' @param expr Character expression such as `log(price)`.
R/prompt-score.R:35:    "- factualScore, inferenceScore, completenessScore, clarityScore, and calibrationScore must each be numeric values between 0 and 2 inclusive.",
R/prompt-score.R:37:    "- overallPass should usually be FALSE if fatalFlawDetected is TRUE.",
R/prompt-score.R:120:    "- clarityAdequate:",
R/prompt-score.R:146:    '  "clarityAdequate": 0,',
R/prompt-score.R:153:    '  "clarityScore": 0.0,',
R/prompt-score.R:156:    '  "overallPass": false,',
R/prompt-score.R:168:    '    "clarityAdequate": "brief reason",',
R/prompt-score.R:207:    "clarityAdequate",
R/prompt-score.R:214:    "clarityScore",
R/prompt-score.R:217:    "overallPass",
R/prompt-score.R:222:    "- factualScore, inferenceScore, completenessScore, clarityScore, and calibrationScore must each be between 0 and 2.",
R/app-developer-scoring-grading.R:1243:                statusText <<- paste("scoring failed:", conditionMessage(e))
R/model-explanation-cleanText.R:294:    pattern = "\\b[Aa] one-magnitude rise multiplies the ([^.]+?) by\\b",
R/model-explanation-cleanText.R:295:    replacement = "If the magnitude increases by one, the \\1 is multiplied by",
R/model-explanation-cleanText.R:301:    pattern = "\\b[Aa] one-magnitude increase multiplies the ([^.]+?) by\\b",
R/model-explanation-cleanText.R:302:    replacement = "If the magnitude increases by one, the \\1 is multiplied by",
R/model-explanation-cleanText.R:308:    pattern = "\\b[Aa] one-magnitude rise\\b",
R/model-explanation-cleanText.R:309:    replacement = "If the magnitude increases by one,",
R/model-explanation-cleanText.R:315:    pattern = "\\b[Oo]ne-magnitude rise\\b",
R/model-explanation-cleanText.R:316:    replacement = "an increase of one in magnitude",
R/model-explanation-cleanText.R:322:    pattern = "\\b[Aa] one-magnitude increase\\b",
R/model-explanation-cleanText.R:323:    replacement = "If the magnitude increases by one,",
R/model-explanation-cleanText.R:329:    pattern = "\\b[Oo]ne-magnitude increase\\b",
R/model-explanation-cleanText.R:330:    replacement = "an increase of one in magnitude",
R/model-explanation-cleanText.R:1108:      name = "oneMagnitudeChange",
R/model-explanation-cleanText.R:1109:      pattern = "\\bone-magnitude (?:rise|increase)\\b"
R/model-explanation-cleanText.R:1151:#' @param audit Optional explanation audit object passed to
R/model-explanation-cleanText.R:1161:#'   "A one-magnitude rise multiplies the expected count by 0.21."
R/scoring-grade-summariseLosses.R:38:    "clarityScore",
R/scoring-grade-summariseLosses.R:49:    "clarityAdequate",
R/scoring-grade-summariseLosses.R:58:    "clarityScore",
R/scoring-grade-summariseLosses.R:72:    "clarityAdequate",
R/scoring-grade-summariseLosses.R:82:    clarityScore = 2,
R/scoring-grade-summariseLosses.R:93:    clarityAdequate = 2,
R/scoring-grade-summariseLosses.R:111:    clarityScore = "Clarity score",
R/scoring-grade-summariseLosses.R:122:    clarityAdequate = "Clarity adequate",
R/scoring-grade-summariseLosses.R:131:    clarityScore = "The explanation was generally clear and readable.",
R/scoring-grade-summariseLosses.R:142:    clarityAdequate = "The explanation wording was acceptably clear.",
R/scoring-grade-summariseLosses.R:259:        return("No clear numeric effect size or magnitude was stated.")
R/scoring-grade-summariseLosses.R:298:      clarityScore = "The explanation could be clearer or more clearly structured.",
R/scoring-grade-summariseLosses.R:308:      clarityAdequate = "The wording or structure could be clearer.",
R/scoring-grade-summariseLosses.R:317:      clarityScore = "The explanation was not clear enough to communicate the fitted model well.",
R/scoring-grade-summariseLosses.R:327:      clarityAdequate = "The explanation wording or structure was not adequately clear.",
R/scoring-grade-summariseLosses.R:557:        clarityAdequate = "clarityScore",
R/app-server-contrasts.R:482:      freq = data.frame(
R/app-server-contrasts.R:496:      freq = data.frame(
R/app-server-contrasts.R:502:      freq = freq[order(freq$Value), , drop = FALSE]
R/app-server-contrasts.R:505:    if (nrow(freq) < 10) {
R/app-server-contrasts.R:506:      return(freq)
R/app-server-contrasts.R:509:    top = freq[seq_len(10), , drop = FALSE]
R/app-server-contrasts.R:510:    otherCount = sum(freq$Count) - sum(top$Count)
R/model-ci-data.R:1283:      quantity = buildFittedQuantityLabel(labelContext = labelContext, notation = notation, quantityType = "probabilityFailure"),
R/model-ci-data.R:1299:      quantity = buildFittedQuantityLabel(labelContext = labelContext, notation = notation, quantityType = "oddsFailure"),
R/model-ci-data.R:1711:#' the one-row \code{newData} passed to \code{predict()}, filling any omitted
R/model-ci-data.R:1907:      probabilityFailure = notation$probabilityFailure,
R/model-ci-data.R:1909:      oddsFailure = notation$oddsFailure,
R/model-ci-data.R:1952:    probabilityFailure = notation$probabilityFailure,
R/model-ci-data.R:1954:    oddsFailure = notation$oddsFailure
R/model-ci-data.R:2271:      failureLevel = levels(response)[1]
R/model-ci-data.R:2272:      out$isComplement = nzchar(out$outcomeLevel) & out$outcomeLevel %in% failureLevel
R/plot-model.R:22:#'   \code{"sandwich"}. For factor-only models, this is passed through to
R/app-plot-uiHelpers.R:74:      "background-color: #fafafa;",
R/scoring-fields-extract.R:37:    "clarityAdequate",
R/scoring-fields-extract.R:44:    "clarityScore",
R/scoring-fields-extract.R:47:    "overallPass",
R/prompt-explain.R:200:For intercept-only models, Restate the research question inferentially; do not describe it as only the average in this data set, dataset, course data, or sample.
R/prompt-explain.R:540:    "\\b(in|from)\\s+(the|this|these|our)?\\s*course\\s+(data|dataset|data set|sample)\\b",
R/prompt-explain.R:541:    "for this course",
R/text-utils.R:125:#' failures where decimals or percentages are written in words rather than
R/app-server-startup.R:17:#' @importFrom shiny updateSelectInput updateTextInput updateCheckboxInput showNotification removeNotification showModal removeModal modalDialog modalButton tagList passwordInput actionButton
R/app-server-startup.R:93:        passwordInput(
R/app-server-startup.R:94:          inputId = "developerModePassword",
R/app-server-startup.R:95:          label = "Developer mode password",
R/app-server-startup.R:96:          placeholder = "Enter password to unlock developer mode"
R/app-server-startup.R:121:      password = input$developerModePassword %||% ""
R/app-server-startup.R:123:      passwordOk = tryCatch(
R/app-server-startup.R:124:        verifyDeveloperModePassword(password),
R/app-server-startup.R:131:      if (isTRUE(passwordOk)) {
R/app-server-startup.R:151:      showNotification(buildDeveloperModeIncorrectPasswordMessage(), type = "error")
R/model-question-classifier.R:42:      "bypass",
R/model-question-classifier.R:127:      "\\bfor\\s+(a\\s+)?(\\d+(?:\\.\\d+)?)\\s*[- ]?(point|mark|carat|magnitude|unit)?\\s*(increase|change)\\b",
R/model-question-classifier.R:175:      "\\bwhat\\b.*\\b(frequency|count|number|probability|odds|chance|value|response|mark|score)\\b.*\\b(expect|expected)\\b",
R/model-question-classifier.R:176:      "\\bwhat\\b.*\\b(expect|expected)\\b.*\\b(frequency|count|number|probability|odds|chance|value|response|mark|score)\\b",
R/model-question-classifier.R:189:    "\\b(if|for|when|with|who)\\b.*\\b(score|scored|scores|mark|marks|attendance|attend|attends|regularly|class|magnitude|washington|california)\\b",
R/model-question-classifier.R:208:  if (grepl("\\b(effect size|magnitude|how big|size of the effect|size of effect)\\b", normalizedText, perl = TRUE)) {
R/model-question-classifier.R:320:      "\\b(?:for\\s+)?(?:a\\s+)?(\\d+(?:\\.\\d+)?)\\s*[- ]?(?:point|mark|carat|magnitude)\\s+(?:increase|change)\\b",
R/model-question-classifier.R:321:      "\\b(\\d+(?:\\.\\d+)?)\\s*[- ]?(?:unit|point|mark|carat|magnitude)\\b",
R/plot-claims.R:11:#' getWmfmClaimColorMap()
R/plot-claims.R:13:getWmfmClaimColorMap = function() {
R/plot-claims.R:72:#' @param includeLegendBreaks Logical. Passed to `orderWmfmLegendValues()`.
R/plot-claims.R:77:#' @importFrom grDevices gray.colors
R/plot-claims.R:108:  colourMap = getWmfmClaimColorMap()
R/plot-claims.R:112:    fallbackColours = gray.colors(length(missingValues), start = 0.85, end = 0.35)
R/methods-print-wmfmGrade.R:100:      "clarityScore",
R/model-explanationAudit.R:229:    "Use deterministic validation-guard targets to flag likely prompt failures without automatically regenerating explanations."
R/app-server-fitted-equations.R:65:      background-color: #f9f9f9;
R/app-server-fitted-equations.R:77:              headingText = "Rounded to three significant figures for clarity"
R/app-server-fitted-equations.R:109:    background-color: #f9f9f9;
R/api-grade.R:7:#' @param ... Additional arguments passed to methods.
R/methods-print-summary-wmfmRuns.R:50:  cat("Claim frequencies\n")
R/model-weights-parseText.R:9:#' If parsing fails, the function returns \code{NA_real_}.
R/app-server-data-observers.R:39:      showNotification(buildDelimitedFileReadFailedMessage(), type = "error")
R/app-server-data-observers.R:192:        buildPackageDatasetLoadFailedMessage(dsName, pkg),
R/methods-compare-wmfmGrade.R:105:    "clarityScore",
R/scoring-llm.R:31:    clarityAdequate = parsedScores$clarityAdequate,
R/scoring-llm.R:38:    clarityScore = parsedScores$clarityScore,
R/scoring-llm.R:41:    overallPass = parsedScores$overallPass,
R/scoring-llm.R:189:      stop("Failed to parse LLM scoring JSON: ", conditionMessage(e), call. = FALSE)
R/scoring-llm.R:390:          "LLM scoring failed to return a valid score record for run %d.",
R/scoring-llm.R:471:    "- factualScore, inferenceScore, completenessScore, clarityScore, and calibrationScore must each be numeric values between 0 and 2 inclusive.",
R/scoring-llm.R:473:    "- overallPass should usually be FALSE if fatalFlawDetected is TRUE.",
R/scoring-llm.R:586:    "- clarityAdequate:",
R/scoring-llm.R:612:    '  "clarityAdequate": 0,',
R/scoring-llm.R:619:    '  "clarityScore": 0.0,',
R/scoring-llm.R:622:    '  "overallPass": false,',
R/scoring-llm.R:634:    '    "clarityAdequate": "brief reason",',
R/scoring-llm.R:676:    "clarityAdequate",
R/scoring-llm.R:683:    "clarityScore",
R/scoring-llm.R:686:    "overallPass",
R/scoring-llm.R:691:    "- factualScore, inferenceScore, completenessScore, clarityScore, and calibrationScore must each be between 0 and 2.",
R/scoring-llm.R:732:    "clarityAdequate",
R/scoring-llm.R:741:    "clarityScore",
R/scoring-llm.R:747:    "overallPass"
R/scoring-metricRegistry.R:17:      "clarityScore",
R/scoring-metricRegistry.R:28:      "clarityAdequate",
R/scoring-metricRegistry.R:32:      "overallPass"
R/scoring-metricRegistry.R:61:      "Clarity score",
R/scoring-metricRegistry.R:72:      "Clarity adequate",
R/scoring-metricRegistry.R:76:      "Overall pass"
R/scoring-metricRegistry.R:174:    NULL,            # clarityScore
R/scoring-metricRegistry.R:189:    NULL             # overallPass
R/app-server-developer-mode-helpers.R:16:#' Build the incorrect developer mode password message
R/app-server-developer-mode-helpers.R:20:buildDeveloperModeIncorrectPasswordMessage = function() {
R/app-server-developer-mode-helpers.R:21:  "Incorrect password. Developer mode remains locked."
R/app-server-developer-mode-helpers.R:48:    message = "The developer-mode password could not be verified."
R/explain-badExplanation-response.R:101:        "Failed to parse bad explanation JSON: ",
R/explain-badExplanation-response.R:250:#'     wrongScaleError = c("factualScore", "clarityScore"),
R/explain-badExplanation-response.R:252:#'     logicalContradiction = c("factualScore", "clarityScore")
R/scoring-semanticEvidence.R:305:    effectMagnitude = extractFirstNumber(
R/scoring-semanticEvidence.R:317:    alternativeModelInterpretationProvided = modelCannotAnswer && detectPattern("study|study effort|study hours|additional hour|odds|passing")
R/scoring-semanticEvidence.R:339:    "effectMagnitude",
R/scoring-semanticEvidence.R:352:    effectMagnitude = "Effect magnitude",
R/scoring-semanticEvidence.R:415:    effectMagnitude = "First clearly stated fitted magnitude found near effect wording.",
R/app-server-data-load-helpers.R:1:#' Build message for a failed delimited file read.
R/app-server-data-load-helpers.R:6:buildDelimitedFileReadFailedMessage = function() {
R/app-server-data-load-helpers.R:7:  "Failed to read file with the chosen separator."
R/app-server-data-load-helpers.R:37:#' Build message for a package dataset loading failure.
R/app-server-data-load-helpers.R:45:buildPackageDatasetLoadFailedMessage = function(datasetName, packageName) {
R/app-server-data-load-helpers.R:131:  "Example: Each row is a student. pass is 0/1. test is a score out of 20. attendance is days attended. We want to understand how test and attendance relate to passing."
R/methods-plot-metricComparisonData.R:57:    plotDf$Freq = as.numeric(plotDf$Freq)
R/methods-plot-metricComparisonData.R:62:        ggplot2::aes(x = .data$detValue, y = .data$otherValue, fill = .data$Freq)
R/methods-plot-metricComparisonData.R:65:        ggplot2::geom_text(ggplot2::aes(label = .data$Freq)) +
R/model-plots.R:524:          color = "red"
R/model-plots.R:542:        color = "blue"
R/model-plots.R:551:      color = "red"
R/model-plots.R:565:        color = "blue"
R/model-plots.R:569:    plot + geom_hline(yintercept = 0, linetype = "dashed", linewidth = 2, color = "red")
R/utils-progress.R:70:#' @param code Function with no arguments to execute.
R/app-ui.R:29:#' @importFrom shiny conditionalPanel selectInput div checkboxInput textOutput passwordInput
R/app-ui.R:38:    tags$style(HTML("\n      .bucket-list .rank-list {\n        max-height: 8em;\n        overflow-y: auto;\n      }\n      html, body {\n        min-height: 100%;\n        overflow-y: auto;\n      }\n      body {\n        font-size: 90%;\n      }\n      .container-fluid {\n        padding-bottom: 18px;\n      }\n      .shiny-input-container { font-size: 90%; }\n      .nav-tabs > li > a { font-size: 90%; }\n      pre, code { font-size: 90%; }\n\n      h4 {\n        margin-top: 12px;\n        margin-bottom: 8px;\n      }\n\n      h5 {\n        margin-top: 6px;\n        margin-bottom: 4px;\n      }\n\n      hr {\n        margin: 8px 0;\n      }\n\n      .hr-tight {\n        margin: 6px 0;\n      }\n\n      .form-group {\n        margin-bottom: 8px;\n      }\n\n      .radio {\n        margin-top: 3px;\n        margin-bottom: 3px;\n      }\n\n      .shiny-html-output,\n      .shiny-text-output {\n        margin-bottom: 6px;\n      }\n\n      .wmfm-ci-section-label {\n        font-weight: 600;\n        margin-top: 10px;\n        margin-bottom: 4px;\n      }\n\n      .wmfm-ci-drilldown-box {\n        border: 1px solid #d9d9d9;\n        border-radius: 6px;\n        padding: 12px;\n        background-color: #fcfcfc;\n        margin-top: 10px;\n        margin-bottom: 10px;\n      }\n\n      .wmfm-ci-secondary-note {\n        color: #666;\n        margin-bottom: 8px;\n      }\n\n      .wmfm-ci-collapsible-block {\n        margin-top: 10px;\n      }\n\n      .wmfm-explanation-box {\n        border: 1px solid #d9d9d9;\n        border-radius: 6px;\n        padding: 12px;\n        background-color: #fcfcfc;\n        margin-top: 8px;\n        white-space: normal;\n      }\n\n      .wmfm-explanation-box p {\n        margin: 0 0 0.8em 0;\n      }\n\n      .wmfm-explanation-box p:last-child {\n        margin-bottom: 0;\n      }\n\n      .wmfm-explanation-helper-box {\n        border: 1px solid #d9d9d9;\n        border-radius: 6px;\n        padding: 12px;\n        background-color: #f8f9fb;\n        margin-top: 10px;\n        margin-bottom: 10px;\n      }\n\n      .wmfm-explanation-helper-note {\n        color: #666;\n        margin-bottom: 8px;\n      }\n\n      .wmfm-model-tab h5 {\n        margin-top: 6px;\n        margin-bottom: 4px;\n      }\n\n      .wmfm-model-tab {\n        padding-bottom: 16px;\n      }\n\n      .wmfm-model-tab .help-block {\n        margin-bottom: 6px;\n      }\n\n      #modelFollowupQuestion::placeholder {\n        color: #9aa0a6;\n        opacity: 1;\n      }\n\n      #modelFollowupQuestion::-webkit-input-placeholder {\n        color: #9aa0a6;\n      }\n\n      #modelFollowupQuestion::-moz-placeholder {\n        color: #9aa0a6;\n        opacity: 1;\n      }\n\n      .wmfm-model-tab .form-group {\n        margin-bottom: 8px;\n      }\n\n      .wmfm-model-tab .hr-tight {\n        margin: 6px 0;\n      }\n\n      .wmfm-model-tab #formula_text {\n        margin-bottom: 4px;\n      }\n\n      .wmfm-model-tab #formula_status {\n        margin-top: 4px;\n        margin-bottom: 0;\n        min-height: 1.4em;\n      }\n\n      .wmfm-formula-status {\n        display: inline-block;\n        padding: 2px 8px;\n        border-radius: 12px;\n        font-size: 0.9em;\n        font-weight: 600;\n      }\n\n      .wmfm-formula-status-ok {\n        background-color: #e8f5e9;\n        color: #1b5e20;\n        border: 1px solid #c8e6c9;\n      }\n\n      .wmfm-formula-status-error {\n        background-color: #ffebee;\n        color: #b71c1c;\n        border: 1px solid #ffcdd2;\n      }\n\n      .wmfm-model-tab .checkbox {\n        margin-top: 6px;\n        margin-bottom: 6px;\n      }\n\n      .wmfm-optional-controls-row .wmfm-optional-control-btn {\n        display: flex;\n        align-items: center;\n        min-height: 34px;\n      }\n\n      .wmfm-optional-controls-row .wmfm-optional-control-btn .btn {\n        margin-bottom: 0;\n        display: inline-flex;\n        align-items: center;\n      }\n\n      .wmfm-model-compact-action-btn {\n        padding: 4px 10px;\n        font-size: 12px;\n        line-height: 1.5;\n        border-radius: 3px;\n        min-height: 30px;\n      }\n\n      .wmfm-model-fit-buttons {\n        display: grid;\n        grid-template-columns: repeat(2, minmax(0, auto));\n        align-items: center;\n        justify-content: start;\n        gap: 8px;\n      }\n\n      .wmfm-model-fit-buttons .btn {\n        margin-bottom: 0;\n        width: auto;\n      }\n\n      .wmfm-model-fit-actions {\n        margin-top: 25px;\n      }\n\n      @media (max-width: 767px) {\n        .wmfm-model-fit-buttons {\n          grid-template-columns: 1fr;\n        }\n      }\n\n      .wmfm-data-context-control {\n        display: flex;\n        align-items: flex-start;\n        min-height: 34px;\n      }\n\n      .wmfm-data-context-control .btn {\n        margin-bottom: 0;\n        display: inline-flex;\n        align-items: center;\n      }\n\n      .wmfm-developer-mode-toggle-row {
R/app-ui.R:72:        background-color: #d9534f;
R/app-ui.R:74:        transition: background-color 0.18s ease-in-out;
R/app-ui.R:85:        background-color: #ffffff;
R/app-ui.R:92:        background-color: #2e7d32;
R/app-ui.R:137:        background-color: #ffffff;
R/app-ui.R:139:        color: #333333;
R/app-ui.R:187:        background-color: #ffffff;
R/app-ui.R:189:        color: #333333;
R/app-ui.R:207:        background-color: #fcfcfc;
R/app-ui.R:278:            style = "font-size: 0.85em; color: #666;",
R/app-ui.R:282:            style = "font-size: 0.85em; color: #666;",
R/app-ui.R:305:          style = "font-size: 0.85em; color: #666;",
R/app-ui.R:319:          style = "font-size: 0.8em; color: #666; margin-top: 20px;",
R/scoring-runRecords-core.R:18:#' @param passThreshold Numeric in `0` to `100`. Threshold used to create
R/scoring-runRecords-core.R:19:#'   `overallPass`.
R/scoring-runRecords-core.R:23:#' @param clarityWeight Numeric weight for the clarity dimension.
R/scoring-runRecords-core.R:37:    passThreshold = 65,
R/scoring-runRecords-core.R:41:    clarityWeight = 0.15,
R/scoring-runRecords-core.R:260:  clarityAdequateExisting = getIntegerColumn(runsDf, "clarityAdequate")
R/scoring-runRecords-core.R:400:  hasNumericMagnitude = hasNumericDigits | hasNumberWords
R/scoring-runRecords-core.R:420:  numericExpressionAdequateComputed[hasNumericMagnitude] = 1L
R/scoring-runRecords-core.R:423:      hasNumericMagnitude
R/scoring-runRecords-core.R:427:      hasNumericMagnitude
R/scoring-runRecords-core.R:431:      hasNumericMagnitude &
R/scoring-runRecords-core.R:437:      hasNumericMagnitude &
R/scoring-runRecords-core.R:442:      hasNumericMagnitude
R/scoring-runRecords-core.R:455:  clarityAdequateComputed = rep(1L, nrow(runsDf))
R/scoring-runRecords-core.R:457:  clarityAdequateComputed[preferredLength] = 2L
R/scoring-runRecords-core.R:458:  clarityAdequateComputed[wordCount < max(20L, as.integer(preferredMinWords %/% 2))] = 0L
R/scoring-runRecords-core.R:459:  clarityAdequateComputed[wordCount > as.integer(preferredMaxWords * 1.5)] = 0L
R/scoring-runRecords-core.R:460:  clarityAdequateComputed[comparisonStructureClear == 2L & numericExpressionAdequate == 2L & preferredLength] = 2L
R/scoring-runRecords-core.R:461:  clarityAdequateComputed[!explanationPresent] = 0L
R/scoring-runRecords-core.R:462:  clarityAdequate = overwriteIfMissing(clarityAdequateExisting, clarityAdequateComputed)
R/scoring-runRecords-core.R:463:  clarityAdequate[
R/scoring-runRecords-core.R:508:  clarityScore = meanIgnoringNa(
R/scoring-runRecords-core.R:509:    clarityAdequate,
R/scoring-runRecords-core.R:519:  totalWeight = factualWeight + inferenceWeight + completenessWeight + clarityWeight + calibrationWeight
R/scoring-runRecords-core.R:529:      clarityWeight * (clarityScore / 2) +
R/scoring-runRecords-core.R:542:  overallPass = overallScore >= as.numeric(passThreshold) & !fatalFlawDetected
R/scoring-runRecords-core.R:588:  scoredDf$clarityAdequate = clarityAdequate
R/scoring-runRecords-core.R:596:  scoredDf$clarityScore = round(clarityScore, 3)
R/scoring-runRecords-core.R:599:  scoredDf$overallPass = overallPass
R/scoring-runs.R:232:#' Build extracted-claim frequency data for a WMFM runs object
R/scoring-runs.R:236:#' @return A data frame summarising claim frequencies.
R/scoring-repeatedRuns.R:32:#'   \item{Clarity score}{Whether the explanation is reasonably clear, sensibly
R/scoring-repeatedRuns.R:53:#'   \item{clarityAdequate}{Integer in `0`, `1`, `2`.}
R/scoring-repeatedRuns.R:65:#'   \item{clarityScore}{Numeric score on a `0` to `2` scale.}
R/scoring-repeatedRuns.R:68:#'   \item{overallPass}{Logical.}
R/scoring-repeatedRuns.R:89:#' @param passThreshold Numeric in `0` to `100`. Threshold used to create
R/scoring-repeatedRuns.R:90:#'   `overallPass`.
R/scoring-repeatedRuns.R:94:#' @param clarityWeight Numeric weight for the clarity dimension.
R/scoring-repeatedRuns.R:106:    passThreshold = 65,
R/scoring-repeatedRuns.R:110:    clarityWeight = 0.15,
R/scoring-repeatedRuns.R:138:    passThreshold = passThreshold,
R/scoring-repeatedRuns.R:142:    clarityWeight = clarityWeight,
R/utils-runRecords.R:149:      "\\bcut(s|ting)?\\b",
R/utils-runRecords.R:163:      "\\bmore frequent\\b",
R/methods-grade-wmfmModel.R:26:#' @param ... Additional arguments passed to `score()` when `autoScore = TRUE`.
R/methods-print-wmfmGradeComparison.R:203:      "clarityScore",
R/prompt-equation.R:68:    logit(Pr({response} = Pass)) = a + c * X    (when F = reference level)
R/prompt-equation.R:71:    logit(Pr({response} = Pass)) = -4.71 + 0.42 * Test    (when F = \"No\")
R/prompt-equation.R:84:    logit(Pr({response} = Pass)) = (-4.71 - 5.13) + (0.42 + 0.76) * Test = -9.84 + 1.18 * Test    (when F = \"Yes\")
R/prompt-equation.R:119:    logit(Pr({response} = Pass)) = (-7.70 + 2.14) + 0.70 * Test = -5.56 + 0.70 * Test    (when Attend = \"Yes\")
R/prompt-equation.R:120:    Odds({response} = Pass) = exp(-5.56 + 0.70 * Test)    (when Attend = \"Yes\")
R/prompt-equation.R:121:    Pr({response} = Pass) = exp(-5.56 + 0.70 * Test) / (1 + exp(-5.56 + 0.70 * Test))    (when Attend = \"Yes\")
R/app-output-messages.R:52:      "The language model request for equations failed.",
R/scoring-explainFieldScore.R:59:    "Clarity" = "clarityAdequate",
R/scoring-explainFieldScore.R:63:    "Overall pass" = "overallPass",
R/scoring-explainFieldScore.R:67:    "Clarity score" = "clarityScore",
R/scoring-explainFieldScore.R:124:                   too_weak = "The explanation fails to acknowledge a real interaction.",
R/scoring-explainFieldScore.R:144:            paste0("Clarity: ", row$clarityScore),
R/methods-score-wmfmGradeListObj.R:12:#' @param ... Additional arguments passed to `score.wmfmGrade()`.
R/app-explanationAudit-uiHelpers.R:48:          "background-color: #f1f3f5;",
R/model-explanationClaimTagDetectors.R:378:    "\\bwhen\\b|at about|at around|at the average|at an average|at a value|at a magnitude of|at the typical|typical magnitude|holding|for a student|for someone|starting value|baseline|fitted value",
R/model-explanationClaimTagDetectors.R:555:    "multiplies|multiplied|1-unit|one-unit|one magnitude|one-magnitude|per unit|per one-unit",
R/methods-plot-wmfmRuns.R:10:#'   \item{`"claims"`}{Bar plot of extracted binary claim frequencies across runs.}
R/methods-plot-wmfmRuns.R:18:#' @param ... Passed through to lower-level plotting helpers.
R/methods-plot-wmfmRuns.R:54:                title = "Claim frequency across repeated runs",
R/app-server-fit-model-helpers.R:1:#' Build message for a failed chat provider connection.
R/app-server-fit-model-helpers.R:8:buildChatProviderConnectionFailedMessage = function(details) {
R/model-explanationAudit-supportNotes.R:32:      "Records deterministic inputs passed into explanation construction.",
R/model-equation-mean.R:80:    pFailure = 1 - pSuccess
R/model-equation-mean.R:82:    oddsFailure = exp(-eta)
R/model-equation-mean.R:94:      notation$oddsFailure, " = exp(-eta) = ",
R/model-equation-mean.R:95:      fmt2dp(oddsFailure), " (\u2248 ", fmt3sf(oddsFailure), ")"
R/model-equation-mean.R:98:      notation$probabilityFailure, " = 1 / (1 + exp(eta)) = ",
R/model-equation-mean.R:99:      fmt2dp(pFailure), " (\u2248 ", fmt3sf(pFailure), ")"
R/model-response.R:142:#' @param modelType Model type passed to \code{validateResponseVar()}.
R/text-describeField.R:216:        "real difference, no clear difference, or fails to mention the issue."
R/text-describeField.R:412:      scoring = "Used as a light clarity/completeness signal.",
R/text-describeField.R:413:      examplesHigh = "TRUE when the outcome is clearly named, such as 'expected earthquake count' or 'exam mark'.",
R/text-describeField.R:425:      scoring = "Used as a light clarity/completeness signal.",
R/text-describeField.R:445:        "strongly for the evidence. `too_weak` means it fails to claim a",
R/text-describeField.R:578:    clarityAdequate = makeEntry(
R/text-describeField.R:579:      canonicalName = "clarityAdequate",
R/text-describeField.R:582:      title = "Clarity adequate",
R/text-describeField.R:589:      prettyLabels = c("Clarity")
R/text-describeField.R:669:    clarityScore = makeEntry(
R/text-describeField.R:670:      canonicalName = "clarityScore",
R/text-describeField.R:673:      title = "Clarity score",
R/text-describeField.R:680:      prettyLabels = c("Clarity score")
R/text-describeField.R:700:      description = "Weighted overall performance score combining factual, inference, completeness, clarity, and calibration dimensions.",
R/text-describeField.R:711:    overallPass = makeEntry(
R/text-describeField.R:712:      canonicalName = "overallPass",
R/text-describeField.R:715:      title = "Overall pass",
R/text-describeField.R:716:      description = "Whether the run's final score exceeds the chosen pass threshold and does not have a fatal flaw.",
R/text-describeField.R:718:      scoring = "TRUE means the run passed by the current rubric settings. FALSE means it did not pass, either because the score was too low or because a fatal flaw was present.",
R/text-describeField.R:720:      examplesLow = "FALSE when the score is below threshold or a fatal flaw forces failure.",
R/text-describeField.R:722:      prettyLabels = c("Overall pass")
R/methods-score-wmfmGrade.R:10:#' @param preferredMinWords Integer. Passed to deterministic scoring.
R/methods-score-wmfmGrade.R:11:#' @param preferredMaxWords Integer. Passed to deterministic scoring.
R/methods-score-wmfmGrade.R:12:#' @param fatalFlawCap Numeric. Passed to deterministic scoring.
R/methods-score-wmfmGrade.R:13:#' @param passThreshold Numeric. Passed to deterministic scoring.
R/methods-score-wmfmGrade.R:15:#' @param useCache Logical. Passed to LLM scoring.
R/methods-score-wmfmGrade.R:18:#' @param verbose Logical. Passed to LLM scoring.
R/methods-score-wmfmGrade.R:21:#' @param ... Additional arguments passed to the relevant scoring helper.
R/methods-score-wmfmGrade.R:31:    passThreshold = 65,
R/methods-score-wmfmGrade.R:63:      passThreshold = passThreshold,
R/methods-score-wmfmGrade.R:287:  x$meta$passThreshold = passThreshold
R/model-binomial-outcomes.R:1:#' Get success and failure labels for a binomial response
R/model-binomial-outcomes.R:5:#' labels for the success and failure outcomes so the app can write
R/model-binomial-outcomes.R:7:#' `Pr(Pass = Pass)` and `Odds(Pass = Pass)` rather than a generic `p`.
R/model-binomial-outcomes.R:11:#' @return A list with `successLabel`, `failureLabel`, and `responseName`.
R/model-binomial-outcomes.R:22:      failureLabel = as.character(levels(y)[1]),
R/model-binomial-outcomes.R:30:      failureLabel = "FALSE",
R/model-binomial-outcomes.R:41:        failureLabel = as.character(values[1]),
R/model-binomial-outcomes.R:49:    failureLabel = "Failure",
R/model-binomial-outcomes.R:57:#' @param outcome Either `"success"` or `"failure"`.
R/model-binomial-outcomes.R:60:#' @return A character scalar such as `"Pr(Pass = Pass)"` or `"Pr(Pass_i = Pass)"`.
R/model-binomial-outcomes.R:62:formatBinomialProbabilityLabel = function(model, outcome = c("success", "failure"), indexed = FALSE) {
R/model-binomial-outcomes.R:66:  outcomeLabel = if (identical(outcome, "success")) labels$successLabel else labels$failureLabel
R/model-binomial-outcomes.R:79:#' @param outcome Either `"success"` or `"failure"`.
R/model-binomial-outcomes.R:82:#' @return A character scalar such as `"Odds(Pass = Pass)"` or `"Odds(Pass_i = Pass)"`.
R/model-binomial-outcomes.R:84:formatBinomialOddsLabel = function(model, outcome = c("success", "failure"), indexed = FALSE) {
R/model-binomial-outcomes.R:88:  outcomeLabel = if (identical(outcome, "success")) labels$successLabel else labels$failureLabel
R/methods-plot-wmfmScores.R:97:    "clarityScore",
R/methods-plot-wmfmScores.R:123:    clarityScore = "Clarity score",
R/methods-plot-wmfmScoreComparison.R:16:#' @param ... Additional arguments passed to the underlying helper.
R/app-server-model-setup.R:529:      style = "margin-top: 6px; color: #b00020;",
R/api-describeField.R:9:#' @param ... Additional arguments passed to methods.
R/api-diagnose.R:7:#' @param ... Additional arguments passed to methods.
R/plot-score.R:15:#' makeWmfmDeterministicCategoryColors(c("yes", "no", "mixed", "(missing)"))
R/plot-score.R:17:makeWmfmDeterministicCategoryColors = function(values,
R/scoring-comparison.R:225:#' @param continuousBreaks Numeric vector of length 3 giving cut points for the
R/api-runModel.R:308:          "Deterministic equation generation failed: ",
R/api-runModel.R:339:            "Deterministic equation generation failed after LLM equation fallback: ",
R/api-runModel.R:357:            "LLM equation generation failed. Falling back to deterministic equations. Details: ",
R/api-runModel.R:374:              "Deterministic equation generation failed after LLM equation fallback: ",
R/api-runModel.R:395:        warning("Explanation generation failed: ", conditionMessage(e), call. = FALSE)
R/examples-run.R:404:  if (identical(transformName, "courseDfScoringGradingAliases")) {
R/examples-run.R:405:    return(addCourseDfScoringGradingAliases(data))
R/examples-run.R:408:  if (identical(transformName, "diamondsPlainFactors")) {
R/examples-run.R:409:    return(convertDiamondsOrderedFactors(data))
R/examples-run.R:419:#' Add stable scoring-and-grading aliases to s20x course.df
R/examples-run.R:421:#' Adds the historical Stage 18 scoring variable names to `s20x::course.df`
R/examples-run.R:425:#' @param data A data frame loaded from `s20x::course.df`.
R/examples-run.R:431:addCourseDfScoringGradingAliases = function(data) {
R/examples-run.R:448:#' Convert ggplot2 diamonds ordered factors to ordinary factors
R/examples-run.R:450:#' The `ggplot2::diamonds` variables `cut`, `color`, and `clarity` are
R/examples-run.R:455:#' @param data A data frame loaded from `ggplot2::diamonds`.
R/examples-run.R:462:convertDiamondsOrderedFactors = function(data) {
R/examples-run.R:467:  factorNames = intersect(c("cut", "color", "clarity"), names(data))
R/examples-run.R:562:#' @param printOutput Logical. Passed to `runModel()`.
R/examples-run.R:566:#' @param useExplanationCache Logical. Passed to `runModel()`.
R/examples-run.R:570:#' @param ... Additional arguments passed to `runModel()`.
R/examples-run.R:586:#' x = runExample("Course")
R/examples-run.R:587:#' y = runExample("Course", nRuns = 20)
R/api-score.R:6:#' @param ... Additional arguments passed to methods.
R/model-scale-phrasingRules.R:26:#'   student-facing language (e.g. \code{"oyster count"}). If \code{NULL},
R/app-server-fit-model.R:93:          buildChatProviderConnectionFailedMessage(conditionMessage(e)),
R/app-server-fit-model.R:397:              "Deterministic equation generation failed.",
R/methods-summary-wmfmRuns.R:5:#' frequencies. Judged fields and score summaries are intentionally excluded and
R/api-stability.R:6:#' @param ... Additional arguments passed to methods.
R/prompt-validationGuard.R:5:#' model to avoid known failure modes, while leaving automatic regeneration for a
R/prompt-validationGuard.R:51:    "- Keep the student-facing explanation clean and surface any failures through developer diagnostics.",
R/app-equation-runtime.R:65:#' @param useExplanationCache Logical. Passed through to `lmExplanation()` when
R/app-equation-runtime.R:105:#'   fails.
R/app-equation-runtime.R:126:#' deterministic fallback if the LLM equation request fails.
R/app-equation-runtime.R:136:#' @param useExplanationCache Logical. Passed through to `lmExplanation()` when
R/model-data.R:25:#' df = data.frame(passengers = 1:144)
inst/extdata/STATS20x.txt:1:Grade	Pass	Exam	Degree	Gender	Attend	Assign	Test	B	C	MC	Colour	Stage1	Years.Since	Repeat
inst/extdata/developer-scoring/stage18-9/wmfm-scoring-export-20260508-145223.json:75:          "label": "Clarity score",
inst/extdata/developer-scoring/stage18-9/wmfm-scoring-export-20260508-145223.json:185:          "label": "Clarity adequate",
inst/extdata/developer-scoring/stage18-9/wmfm-scoring-export-20260508-145223.json:202:          "reason": "No clear numeric effect size or magnitude was stated."
inst/extdata/developer-scoring/stage18-9/wmfm-scoring-export-20260508-145223.json:262:          "reason": "No clear numeric effect size or magnitude was stated."
inst/extdata/developer-scoring/stage18-9/wmfm-scoring-export-20260508-145223.json:297:          "metric": "clarityScore",
inst/extdata/developer-scoring/stage18-9/wmfm-scoring-export-20260508-145223.json:298:          "label": "Clarity score",
inst/extdata/developer-scoring/stage18-9/wmfm-scoring-export-20260508-145223.json:330:          "metric": "clarityAdequate",
inst/extdata/developer-scoring/stage18-9/wmfm-scoring-export-20260508-145223.json:331:          "label": "Clarity adequate",
inst/extdata/developer-scoring/stage18-9/wmfm-scoring-export-20260508-145223.json:464:              "label": "Clarity score",
inst/extdata/developer-scoring/stage18-9/wmfm-scoring-export-20260508-145223.json:574:              "label": "Clarity adequate",
inst/extdata/developer-scoring/stage18-9/wmfm-scoring-export-20260508-145223.json:614:              "metric": "clarityAdequate",
inst/extdata/developer-scoring/stage18-9/wmfm-scoring-export-20260508-145223.json:615:              "label": "Clarity adequate",
inst/extdata/developer-scoring/stage18-9/wmfm-scoring-export-20260508-145223.json:621:              "metric": "clarityScore",
inst/extdata/developer-scoring/stage18-9/wmfm-scoring-export-20260508-145223.json:622:              "label": "Clarity score",
inst/extdata/developer-scoring/stage18-9/wmfm-scoring-export-20260508-145223.json:790:              "label": "Clarity score",
inst/extdata/developer-scoring/stage18-9/wmfm-scoring-export-20260508-145223.json:900:              "label": "Clarity adequate",
inst/extdata/developer-scoring/stage18-9/wmfm-scoring-export-20260508-145223.json:940:              "metric": "clarityAdequate",
inst/extdata/developer-scoring/stage18-9/wmfm-scoring-export-20260508-145223.json:941:              "label": "Clarity adequate",
inst/extdata/developer-scoring/stage18-9/wmfm-scoring-export-20260508-145223.json:947:              "metric": "clarityScore",
inst/extdata/developer-scoring/stage18-9/wmfm-scoring-export-20260508-145223.json:948:              "label": "Clarity score",
inst/extdata/developer-scoring/stage18-9/wmfm-scoring-export-20260508-145223.json:1116:              "label": "Clarity score",
inst/extdata/developer-scoring/stage18-9/wmfm-scoring-export-20260508-145223.json:1226:              "label": "Clarity adequate",
inst/extdata/developer-scoring/stage18-9/wmfm-scoring-export-20260508-145223.json:1291:              "metric": "clarityAdequate",
inst/extdata/developer-scoring/stage18-9/wmfm-scoring-export-20260508-145223.json:1292:              "label": "Clarity adequate",
inst/extdata/developer-scoring/stage18-9/wmfm-scoring-export-20260508-145223.json:1298:              "metric": "clarityScore",
inst/extdata/developer-scoring/stage18-9/wmfm-scoring-export-20260508-145223.json:1299:              "label": "Clarity score",
inst/extdata/developer-scoring/stage18-9/wmfm-scoring-export-20260508-144814.json:15:    "researchQuestion": "Do average final exam marks differ by gender in this course example?"
inst/extdata/developer-scoring/stage18-9/wmfm-scoring-export-20260508-144814.json:18:    "explanation": "Do average final exam marks differ by gender in this course example? The model shows that the mean final exam mark is about 55 for female students (95% confidence interval 50 to 59) and about 51 for male students (95% confidence interval 46 to 55). Because these intervals overlap and the evidence for a difference is weak, we cannot say there is a clear difference in average marks between genders. On average, female students score a few points higher, but the data do not provide strong support for a definite gender gap.",
inst/extdata/developer-scoring/stage18-9/wmfm-scoring-export-20260508-144814.json:75:          "label": "Clarity score",
inst/extdata/developer-scoring/stage18-9/wmfm-scoring-export-20260508-144814.json:185:          "label": "Clarity adequate",
inst/extdata/developer-scoring/stage18-9/wmfm-scoring-export-20260508-144814.json:225:          "metric": "clarityAdequate",
inst/extdata/developer-scoring/stage18-9/wmfm-scoring-export-20260508-144814.json:226:          "label": "Clarity adequate",
inst/extdata/developer-scoring/stage18-9/wmfm-scoring-export-20260508-144814.json:232:          "metric": "clarityScore",
inst/extdata/developer-scoring/stage18-9/wmfm-scoring-export-20260508-144814.json:233:          "label": "Clarity score",
inst/extdata/developer-scoring/stage18-9/wmfm-scoring-export-20260508-144814.json:372:        "explanation": "Do the average final exam marks differ by gender in this course example? The analysis compares expected final exam scores for female and male students. For a female student the expected score is 55, with a 95 % confidence interval of 50 to 59. for a male student the expected score is 51, with a 95 % confidence interval of 46 to 55. Because the intervals overlap and the difference is modest, the evidence for a consistent gender difference is weak. On average, female students score roughly four points higher than male students, but the uncertainty shows no clear evidence that the gender effect is different from zero.",
inst/extdata/developer-scoring/stage18-9/wmfm-scoring-export-20260508-144814.json:429:              "label": "Clarity score",
inst/extdata/developer-scoring/stage18-9/wmfm-scoring-export-20260508-144814.json:539:              "label": "Clarity adequate",
inst/extdata/developer-scoring/stage18-9/wmfm-scoring-export-20260508-144814.json:579:              "metric": "clarityAdequate",
inst/extdata/developer-scoring/stage18-9/wmfm-scoring-export-20260508-144814.json:580:              "label": "Clarity adequate",
inst/extdata/developer-scoring/stage18-9/wmfm-scoring-export-20260508-144814.json:586:              "metric": "clarityScore",
inst/extdata/developer-scoring/stage18-9/wmfm-scoring-export-20260508-144814.json:587:              "label": "Clarity score",
inst/extdata/developer-scoring/stage18-9/wmfm-scoring-export-20260508-144814.json:684:        "explanation": "Do average final exam marks differ by gender in this course example? The results give an expected final mark of 55 for female students (95 % CI 50 to 59) and 51 for male students (95 % CI 46 to 55). because the intervals overlap, the evidence for a consistent gender difference is weak, suggesting that on average the two groups perform similarly.",
inst/extdata/developer-scoring/stage18-9/wmfm-scoring-export-20260508-144814.json:741:              "label": "Clarity score",
inst/extdata/developer-scoring/stage18-9/wmfm-scoring-export-20260508-144814.json:851:              "label": "Clarity adequate",
inst/extdata/developer-scoring/stage18-9/wmfm-scoring-export-20260508-144814.json:923:              "metric": "clarityAdequate",
inst/extdata/developer-scoring/stage18-9/wmfm-scoring-export-20260508-144814.json:924:              "label": "Clarity adequate",
inst/extdata/developer-scoring/stage18-9/wmfm-scoring-export-20260508-144814.json:963:              "metric": "clarityScore",
inst/extdata/developer-scoring/stage18-9/wmfm-scoring-export-20260508-144814.json:964:              "label": "Clarity score",
inst/extdata/developer-scoring/stage18-9/wmfm-scoring-export-20260508-144814.json:1016:        "explanation": "Do average final exam marks differ by gender in this course example? The expected exam score for a female student is 55 (95 % CI 50 to 59), while for a male student it is 51 (95 % CI 46 to 55). because the confidence intervals overlap, the data provide weak evidence that gender explains a difference, suggesting that on average the two groups perform similarly.",
inst/extdata/developer-scoring/stage18-9/wmfm-scoring-export-20260508-144814.json:1073:              "label": "Clarity score",
inst/extdata/developer-scoring/stage18-9/wmfm-scoring-export-20260508-144814.json:1183:              "label": "Clarity adequate",
inst/extdata/developer-scoring/stage18-9/wmfm-scoring-export-20260508-144814.json:1255:              "metric": "clarityAdequate",
inst/extdata/developer-scoring/stage18-9/wmfm-scoring-export-20260508-144814.json:1256:              "label": "Clarity adequate",
inst/extdata/developer-scoring/stage18-9/wmfm-scoring-export-20260508-144814.json:1295:              "metric": "clarityScore",
inst/extdata/developer-scoring/stage18-9/wmfm-scoring-export-20260508-144814.json:1296:              "label": "Clarity score",
inst/extdata/developer-scoring/stage18-9/wmfm-scoring-export-20260508-145041.json:12:    "formula": "Pass ~ StudyHours",
inst/extdata/developer-scoring/stage18-9/wmfm-scoring-export-20260508-145041.json:15:    "researchQuestion": "How does the test mark relate to the probability of passing?"
inst/extdata/developer-scoring/stage18-9/wmfm-scoring-export-20260508-145041.json:18:    "explanation": "The question is: how does the test mark relate to the probability of passing? The model gives an estimated probability of passing of 84 % (95 % confidence interval 73 % to 91 %) for a student who studies an average of 11.567 hours. For each additional hour of study, the odds of passing double (odds multiplier = 2. 95 % confidence interval 1.6 to 2.5). Because the model does not include the test mark as a predictor, it does not provide any estimate of how test marks affect the probability of passing. Consequently, the relationship between test mark and passing cannot be inferred from this analysis.",
inst/extdata/developer-scoring/stage18-9/wmfm-scoring-export-20260508-145041.json:75:          "label": "Clarity score",
inst/extdata/developer-scoring/stage18-9/wmfm-scoring-export-20260508-145041.json:185:          "label": "Clarity adequate",
inst/extdata/developer-scoring/stage18-9/wmfm-scoring-export-20260508-145041.json:273:          "metric": "clarityScore",
inst/extdata/developer-scoring/stage18-9/wmfm-scoring-export-20260508-145041.json:274:          "label": "Clarity score",
inst/extdata/developer-scoring/stage18-9/wmfm-scoring-export-20260508-145041.json:290:          "metric": "clarityAdequate",
inst/extdata/developer-scoring/stage18-9/wmfm-scoring-export-20260508-145041.json:291:          "label": "Clarity adequate",
inst/extdata/developer-scoring/stage18-9/wmfm-scoring-export-20260508-145041.json:367:        "explanation": "How does the test mark relate to the probability of passing? The model does not include a test mark variable, so it cannot tell us how a mark changes the chance of passing. It does, however, describe how study effort affects the outcome. For a student who studies the typical 11.567 hours, the estimated probability of passing is 84% (95% confidence interval 73% to 91%). Each additional hour of study roughly doubles the odds that the student passes (odds multiplier estimate 2, 95% confidence interval 1.6 to 2.5). Overall, the model gives evidence that increased study time is associated with a higher probability of passing, but it does not provide information about the relationship between the test mark and passing.",
inst/extdata/developer-scoring/stage18-9/wmfm-scoring-export-20260508-145041.json:424:              "label": "Clarity score",
inst/extdata/developer-scoring/stage18-9/wmfm-scoring-export-20260508-145041.json:534:              "label": "Clarity adequate",
inst/extdata/developer-scoring/stage18-9/wmfm-scoring-export-20260508-145041.json:599:              "metric": "clarityAdequate",
inst/extdata/developer-scoring/stage18-9/wmfm-scoring-export-20260508-145041.json:600:              "label": "Clarity adequate",
inst/extdata/developer-scoring/stage18-9/wmfm-scoring-export-20260508-145041.json:606:              "metric": "clarityScore",
inst/extdata/developer-scoring/stage18-9/wmfm-scoring-export-20260508-145041.json:607:              "label": "Clarity score",
inst/extdata/developer-scoring/stage18-9/wmfm-scoring-export-20260508-145041.json:669:        "explanation": "The researcher asks how a test mark influences the chance of passing an exam. At the average level of study effort-about 11.6 hours-the estimated probability of passing is 84% (95% confidence interval 73% to 91%). For each additional hour of study, the odds that a student passes are multiplied by about 2 (95% confidence interval 1.6 to 2.5). Because the analysis does not include a test-mark variable, it cannot explain how test marks change the probability of passing. Therefore it cannot directly answer the research question.",
inst/extdata/developer-scoring/stage18-9/wmfm-scoring-export-20260508-145041.json:726:              "label": "Clarity score",
inst/extdata/developer-scoring/stage18-9/wmfm-scoring-export-20260508-145041.json:836:              "label": "Clarity adequate",
inst/extdata/developer-scoring/stage18-9/wmfm-scoring-export-20260508-145041.json:924:              "metric": "clarityScore",
inst/extdata/developer-scoring/stage18-9/wmfm-scoring-export-20260508-145041.json:925:              "label": "Clarity score",
inst/extdata/developer-scoring/stage18-9/wmfm-scoring-export-20260508-145041.json:941:              "metric": "clarityAdequate",
inst/extdata/developer-scoring/stage18-9/wmfm-scoring-export-20260508-145041.json:942:              "label": "Clarity adequate",
inst/extdata/developer-scoring/stage18-9/wmfm-scoring-export-20260508-145041.json:976:        "explanation": "The research question asks how a student’s test mark influences the chance of passing. The available analysis includes only the effect of study effort, not the test mark, so it cannot directly answer that question. For a student who studies about 11.6 hours, the predicted chance of passing is 84% (95% confidence interval 73% to 91%). Each additional hour of study roughly doubles the odds of passing (odds multiplier 2. 95% confidence interval 1.6 to 2.5). Because the model does not contain a test-mark variable, it does not provide an estimate of how test scores affect the probability of passing.",
inst/extdata/developer-scoring/stage18-9/wmfm-scoring-export-20260508-145041.json:1033:              "label": "Clarity score",
inst/extdata/developer-scoring/stage18-9/wmfm-scoring-export-20260508-145041.json:1143:              "label": "Clarity adequate",
inst/extdata/developer-scoring/stage18-9/wmfm-scoring-export-20260508-145041.json:1231:              "metric": "clarityScore",
inst/extdata/developer-scoring/stage18-9/wmfm-scoring-export-20260508-145041.json:1232:              "label": "Clarity score",
inst/extdata/developer-scoring/stage18-9/wmfm-scoring-export-20260508-145041.json:1248:              "metric": "clarityAdequate",
inst/extdata/developer-scoring/stage18-9/wmfm-scoring-export-20260508-145041.json:1249:              "label": "Clarity adequate",
inst/extdata/developer-scoring/stage18-9/wmfm-scoring-export-20260508-144104.json:75:          "label": "Clarity score",
inst/extdata/developer-scoring/stage18-9/wmfm-scoring-export-20260508-144104.json:185:          "label": "Clarity adequate",
inst/extdata/developer-scoring/stage18-9/wmfm-scoring-export-20260508-144104.json:225:          "metric": "clarityScore",
inst/extdata/developer-scoring/stage18-9/wmfm-scoring-export-20260508-144104.json:226:          "label": "Clarity score",
inst/extdata/developer-scoring/stage18-9/wmfm-scoring-export-20260508-144104.json:258:          "metric": "clarityAdequate",
inst/extdata/developer-scoring/stage18-9/wmfm-scoring-export-20260508-144104.json:259:          "label": "Clarity adequate",
inst/extdata/developer-scoring/stage18-9/wmfm-scoring-export-20260508-144104.json:420:              "label": "Clarity score",
inst/extdata/developer-scoring/stage18-9/wmfm-scoring-export-20260508-144104.json:530:              "label": "Clarity adequate",
inst/extdata/developer-scoring/stage18-9/wmfm-scoring-export-20260508-144104.json:570:              "metric": "clarityAdequate",
inst/extdata/developer-scoring/stage18-9/wmfm-scoring-export-20260508-144104.json:571:              "label": "Clarity adequate",
inst/extdata/developer-scoring/stage18-9/wmfm-scoring-export-20260508-144104.json:577:              "metric": "clarityScore",
inst/extdata/developer-scoring/stage18-9/wmfm-scoring-export-20260508-144104.json:578:              "label": "Clarity score",
inst/extdata/developer-scoring/stage18-9/wmfm-scoring-export-20260508-144104.json:711:              "label": "Clarity score",
inst/extdata/developer-scoring/stage18-9/wmfm-scoring-export-20260508-144104.json:821:              "label": "Clarity adequate",
inst/extdata/developer-scoring/stage18-9/wmfm-scoring-export-20260508-144104.json:861:              "metric": "clarityAdequate",
inst/extdata/developer-scoring/stage18-9/wmfm-scoring-export-20260508-144104.json:862:              "label": "Clarity adequate",
inst/extdata/developer-scoring/stage18-9/wmfm-scoring-export-20260508-144104.json:868:              "metric": "clarityScore",
inst/extdata/developer-scoring/stage18-9/wmfm-scoring-export-20260508-144104.json:869:              "label": "Clarity score",
inst/extdata/developer-scoring/stage18-9/wmfm-scoring-export-20260508-144104.json:1002:              "label": "Clarity score",
inst/extdata/developer-scoring/stage18-9/wmfm-scoring-export-20260508-144104.json:1112:              "label": "Clarity adequate",
inst/extdata/developer-scoring/stage18-9/wmfm-scoring-export-20260508-144104.json:1152:              "metric": "clarityAdequate",
inst/extdata/developer-scoring/stage18-9/wmfm-scoring-export-20260508-144104.json:1153:              "label": "Clarity adequate",
inst/extdata/developer-scoring/stage18-9/wmfm-scoring-export-20260508-144104.json:1159:              "metric": "clarityScore",
inst/extdata/developer-scoring/stage18-9/wmfm-scoring-export-20260508-144104.json:1160:              "label": "Clarity score",
inst/extdata/developer-scoring/stage18-9/wmfm-scoring-export-20260508-143946.json:75:          "label": "Clarity score",
inst/extdata/developer-scoring/stage18-9/wmfm-scoring-export-20260508-143946.json:185:          "label": "Clarity adequate",
inst/extdata/developer-scoring/stage18-9/wmfm-scoring-export-20260508-143946.json:225:          "metric": "clarityAdequate",
inst/extdata/developer-scoring/stage18-9/wmfm-scoring-export-20260508-143946.json:226:          "label": "Clarity adequate",
inst/extdata/developer-scoring/stage18-9/wmfm-scoring-export-20260508-143946.json:232:          "metric": "clarityScore",
inst/extdata/developer-scoring/stage18-9/wmfm-scoring-export-20260508-143946.json:233:          "label": "Clarity score",
inst/extdata/developer-scoring/stage18-9/wmfm-scoring-export-20260508-143946.json:408:              "label": "Clarity score",
inst/extdata/developer-scoring/stage18-9/wmfm-scoring-export-20260508-143946.json:518:              "label": "Clarity adequate",
inst/extdata/developer-scoring/stage18-9/wmfm-scoring-export-20260508-143946.json:558:              "metric": "clarityScore",
inst/extdata/developer-scoring/stage18-9/wmfm-scoring-export-20260508-143946.json:559:              "label": "Clarity score",
inst/extdata/developer-scoring/stage18-9/wmfm-scoring-export-20260508-143946.json:591:              "metric": "clarityAdequate",
inst/extdata/developer-scoring/stage18-9/wmfm-scoring-export-20260508-143946.json:592:              "label": "Clarity adequate",
inst/extdata/developer-scoring/stage18-9/wmfm-scoring-export-20260508-143946.json:711:              "label": "Clarity score",
inst/extdata/developer-scoring/stage18-9/wmfm-scoring-export-20260508-143946.json:821:              "label": "Clarity adequate",
inst/extdata/developer-scoring/stage18-9/wmfm-scoring-export-20260508-143946.json:861:              "metric": "clarityScore",
inst/extdata/developer-scoring/stage18-9/wmfm-scoring-export-20260508-143946.json:862:              "label": "Clarity score",
inst/extdata/developer-scoring/stage18-9/wmfm-scoring-export-20260508-143946.json:894:              "metric": "clarityAdequate",
inst/extdata/developer-scoring/stage18-9/wmfm-scoring-export-20260508-143946.json:895:              "label": "Clarity adequate",
inst/extdata/developer-scoring/stage18-9/wmfm-scoring-export-20260508-143946.json:1014:              "label": "Clarity score",
inst/extdata/developer-scoring/stage18-9/wmfm-scoring-export-20260508-143946.json:1124:              "label": "Clarity adequate",
inst/extdata/developer-scoring/stage18-9/wmfm-scoring-export-20260508-143946.json:1164:              "metric": "clarityAdequate",
inst/extdata/developer-scoring/stage18-9/wmfm-scoring-export-20260508-143946.json:1165:              "label": "Clarity adequate",
inst/extdata/developer-scoring/stage18-9/wmfm-scoring-export-20260508-143946.json:1171:              "metric": "clarityScore",
inst/extdata/developer-scoring/stage18-9/wmfm-scoring-export-20260508-143946.json:1172:              "label": "Clarity score",
inst/extdata/developer-scoring/stage19-3-1/wmfm-scoring-export-20260510-112534.json:75:          "label": "Clarity score",
inst/extdata/developer-scoring/stage19-3-1/wmfm-scoring-export-20260510-112534.json:185:          "label": "Clarity adequate",
inst/extdata/developer-scoring/stage19-3-1/wmfm-scoring-export-20260510-112534.json:225:          "metric": "clarityAdequate",
inst/extdata/developer-scoring/stage19-3-1/wmfm-scoring-export-20260510-112534.json:226:          "label": "Clarity adequate",
inst/extdata/developer-scoring/stage19-3-1/wmfm-scoring-export-20260510-112534.json:233:          "metric": "clarityScore",
inst/extdata/developer-scoring/stage19-3-1/wmfm-scoring-export-20260510-112534.json:234:          "label": "Clarity score",
inst/extdata/developer-scoring/stage19-3-1/wmfm-scoring-export-20260510-112534.json:357:          "field": "effectMagnitude",
inst/extdata/developer-scoring/stage19-3-1/wmfm-scoring-export-20260510-112534.json:358:          "label": "Effect magnitude",
inst/extdata/developer-scoring/stage19-3-1/wmfm-scoring-export-20260510-112534.json:361:          "detail": "First clearly stated fitted magnitude found near effect wording."
inst/extdata/developer-scoring/stage19-3-1/wmfm-scoring-export-20260510-112534.json:469:        "explanation": "Does the relationship between attendance and final exam mark differ by gender after accounting for mid-term test performance? The model explains about 63% of the variation in final exam marks. Among female students with average mid-term test marks (around 11.6 out of 100), those who attended regularly scored about 59.7 on the final exam, while those who did not attend regularly scored about 48.6, a difference of about 11.1 marks (95% confidence interval 5.3 to 16.9). Among male students with similar mid-term marks, regular attenders scored about 60.8 compared to 51.5 for non-attenders, a difference of about 9.3 marks (95% confidence interval 3.6 to 15). The difference between these attendance effects across genders is modest: the data are consistent with female students benefiting anywhere from 6.8 marks less to 3.9 marks more from regular attendance compared to male students (95% confidence interval -6.8 to 3.9), indicating no clear evidence that attendance relates to exam performance differently for the two genders. Overall, after accounting for mid-term test performance, there is no clear evidence that the relationship between attendance and final exam mark differs by gender. Both female and male students who attended regularly tended to score higher on the final exam, with expected gains of similar magnitude, though the data do not rule out modest differences in either direction.",
inst/extdata/developer-scoring/stage19-3-1/wmfm-scoring-export-20260510-112534.json:526:              "label": "Clarity score",
inst/extdata/developer-scoring/stage19-3-1/wmfm-scoring-export-20260510-112534.json:636:              "label": "Clarity adequate",
inst/extdata/developer-scoring/stage19-3-1/wmfm-scoring-export-20260510-112534.json:676:              "metric": "clarityAdequate",
inst/extdata/developer-scoring/stage19-3-1/wmfm-scoring-export-20260510-112534.json:677:              "label": "Clarity adequate",
inst/extdata/developer-scoring/stage19-3-1/wmfm-scoring-export-20260510-112534.json:683:              "metric": "clarityScore",
inst/extdata/developer-scoring/stage19-3-1/wmfm-scoring-export-20260510-112534.json:684:              "label": "Clarity score",
inst/extdata/developer-scoring/stage19-3-1/wmfm-scoring-export-20260510-112534.json:797:              "field": "effectMagnitude",
inst/extdata/developer-scoring/stage19-3-1/wmfm-scoring-export-20260510-112534.json:798:              "label": "Effect magnitude",
inst/extdata/developer-scoring/stage19-3-1/wmfm-scoring-export-20260510-112534.json:801:              "detail": "First clearly stated fitted magnitude found near effect wording."
inst/extdata/developer-scoring/stage19-3-1/wmfm-scoring-export-20260510-112534.json:924:              "label": "Clarity score",
inst/extdata/developer-scoring/stage19-3-1/wmfm-scoring-export-20260510-112534.json:1034:              "label": "Clarity adequate",
inst/extdata/developer-scoring/stage19-3-1/wmfm-scoring-export-20260510-112534.json:1074:              "metric": "clarityAdequate",
inst/extdata/developer-scoring/stage19-3-1/wmfm-scoring-export-20260510-112534.json:1075:              "label": "Clarity adequate",
inst/extdata/developer-scoring/stage19-3-1/wmfm-scoring-export-20260510-112534.json:1081:              "metric": "clarityScore",
inst/extdata/developer-scoring/stage19-3-1/wmfm-scoring-export-20260510-112534.json:1082:              "label": "Clarity score",
inst/extdata/developer-scoring/stage19-3-1/wmfm-scoring-export-20260510-112534.json:1195:              "field": "effectMagnitude",
inst/extdata/developer-scoring/stage19-3-1/wmfm-scoring-export-20260510-112534.json:1196:              "label": "Effect magnitude",
inst/extdata/developer-scoring/stage19-3-1/wmfm-scoring-export-20260510-112534.json:1199:              "detail": "First clearly stated fitted magnitude found near effect wording."
inst/extdata/developer-scoring/stage19-3-1/wmfm-scoring-export-20260510-112534.json:1322:              "label": "Clarity score",
inst/extdata/developer-scoring/stage19-3-1/wmfm-scoring-export-20260510-112534.json:1432:              "label": "Clarity adequate",
inst/extdata/developer-scoring/stage19-3-1/wmfm-scoring-export-20260510-112534.json:1497:              "metric": "clarityAdequate",
inst/extdata/developer-scoring/stage19-3-1/wmfm-scoring-export-20260510-112534.json:1498:              "label": "Clarity adequate",
inst/extdata/developer-scoring/stage19-3-1/wmfm-scoring-export-20260510-112534.json:1504:              "metric": "clarityScore",
inst/extdata/developer-scoring/stage19-3-1/wmfm-scoring-export-20260510-112534.json:1505:              "label": "Clarity score",
inst/extdata/developer-scoring/stage19-3-1/wmfm-scoring-export-20260510-112534.json:1604:              "field": "effectMagnitude",
inst/extdata/developer-scoring/stage19-3-1/wmfm-scoring-export-20260510-112534.json:1605:              "label": "Effect magnitude",
inst/extdata/developer-scoring/stage19-3-1/wmfm-scoring-export-20260510-112534.json:1608:              "detail": "First clearly stated fitted magnitude found near effect wording."
inst/extdata/developer-scoring/stage19-3-1/wmfm-scoring-export-20260510-112328.json:15:    "researchQuestion": "Do average final exam marks differ by gender in this course example?"
inst/extdata/developer-scoring/stage19-3-1/wmfm-scoring-export-20260510-112328.json:18:    "explanation": "Do average final exam marks differ by gender in this course example? The model compares average final exam marks (out of 100) for female and male students. The expected mark for female students is 55 (95% confidence interval 50 to 59), while for male students it is 51 (95% confidence interval 46 to 55). The fitted values differ by about 4 marks, but the model provides weak evidence for a clear difference-the data are consistent with a range from a small advantage for male students to a moderate advantage for female students. The model explains very little of the variation in exam marks (about 1%), so other factors not included here are more important for understanding why students receive different marks. Overall, the model does not show a clear difference in average exam marks by gender. The estimated difference is modest, and the uncertainty around this comparison means we cannot rule out patterns ranging from nearly equal marks to female students scoring moderately higher on average.",
inst/extdata/developer-scoring/stage19-3-1/wmfm-scoring-export-20260510-112328.json:75:          "label": "Clarity score",
inst/extdata/developer-scoring/stage19-3-1/wmfm-scoring-export-20260510-112328.json:185:          "label": "Clarity adequate",
inst/extdata/developer-scoring/stage19-3-1/wmfm-scoring-export-20260510-112328.json:225:          "metric": "clarityAdequate",
inst/extdata/developer-scoring/stage19-3-1/wmfm-scoring-export-20260510-112328.json:226:          "label": "Clarity adequate",
inst/extdata/developer-scoring/stage19-3-1/wmfm-scoring-export-20260510-112328.json:232:          "metric": "clarityScore",
inst/extdata/developer-scoring/stage19-3-1/wmfm-scoring-export-20260510-112328.json:233:          "label": "Clarity score",
inst/extdata/developer-scoring/stage19-3-1/wmfm-scoring-export-20260510-112328.json:332:          "field": "effectMagnitude",
inst/extdata/developer-scoring/stage19-3-1/wmfm-scoring-export-20260510-112328.json:333:          "label": "Effect magnitude",
inst/extdata/developer-scoring/stage19-3-1/wmfm-scoring-export-20260510-112328.json:336:          "detail": "First clearly stated fitted magnitude found near effect wording."
inst/extdata/developer-scoring/stage19-3-1/wmfm-scoring-export-20260510-112328.json:444:        "explanation": "Do average final exam marks differ by gender in this course example? The analysis compares average final exam marks (out of 100) for female and male students. The expected mark for female students is 55 (95% confidence interval 50 to 59), while for male students it is 51 (95% confidence interval 46 to 55). Although the estimated marks differ by about 4 marks, the model does not show a clear difference-values consistent with these data range from male students scoring slightly higher to female students scoring moderately higher. The model explains very little of the variation in exam marks (about 1%), indicating that other factors not included here play a much larger role in determining individual marks. Overall, there is no clear evidence of a difference in average exam marks by gender based on these data.",
inst/extdata/developer-scoring/stage19-3-1/wmfm-scoring-export-20260510-112328.json:501:              "label": "Clarity score",
inst/extdata/developer-scoring/stage19-3-1/wmfm-scoring-export-20260510-112328.json:611:              "label": "Clarity adequate",
inst/extdata/developer-scoring/stage19-3-1/wmfm-scoring-export-20260510-112328.json:651:              "metric": "clarityAdequate",
inst/extdata/developer-scoring/stage19-3-1/wmfm-scoring-export-20260510-112328.json:652:              "label": "Clarity adequate",
inst/extdata/developer-scoring/stage19-3-1/wmfm-scoring-export-20260510-112328.json:658:              "metric": "clarityScore",
inst/extdata/developer-scoring/stage19-3-1/wmfm-scoring-export-20260510-112328.json:659:              "label": "Clarity score",
inst/extdata/developer-scoring/stage19-3-1/wmfm-scoring-export-20260510-112328.json:758:              "field": "effectMagnitude",
inst/extdata/developer-scoring/stage19-3-1/wmfm-scoring-export-20260510-112328.json:759:              "label": "Effect magnitude",
inst/extdata/developer-scoring/stage19-3-1/wmfm-scoring-export-20260510-112328.json:762:              "detail": "First clearly stated fitted magnitude found near effect wording."
inst/extdata/developer-scoring/stage19-3-1/wmfm-scoring-export-20260510-112328.json:828:        "explanation": "Do average final exam marks differ by gender in this course example? The analysis compares average final exam marks (out of 100) for female and male students. The expected mark for female students is 55 (95% confidence interval 50 to 59), while for male students it is 51 (95% confidence interval 46 to 55). Although the point estimates differ by about 4 marks, the model does not show a clear difference-the data are consistent with patterns ranging from male students scoring slightly higher to female students scoring moderately higher. The model explains very little of the variation in exam marks (about 1%), indicating that gender accounts for only a small part of the differences in individual performance. Overall, there is no clear evidence of a difference in average exam marks by gender based on these data.",
inst/extdata/developer-scoring/stage19-3-1/wmfm-scoring-export-20260510-112328.json:885:              "label": "Clarity score",
inst/extdata/developer-scoring/stage19-3-1/wmfm-scoring-export-20260510-112328.json:995:              "label": "Clarity adequate",
inst/extdata/developer-scoring/stage19-3-1/wmfm-scoring-export-20260510-112328.json:1035:              "metric": "clarityAdequate",
inst/extdata/developer-scoring/stage19-3-1/wmfm-scoring-export-20260510-112328.json:1036:              "label": "Clarity adequate",
inst/extdata/developer-scoring/stage19-3-1/wmfm-scoring-export-20260510-112328.json:1042:              "metric": "clarityScore",
inst/extdata/developer-scoring/stage19-3-1/wmfm-scoring-export-20260510-112328.json:1043:              "label": "Clarity score",
inst/extdata/developer-scoring/stage19-3-1/wmfm-scoring-export-20260510-112328.json:1142:              "field": "effectMagnitude",
inst/extdata/developer-scoring/stage19-3-1/wmfm-scoring-export-20260510-112328.json:1143:              "label": "Effect magnitude",
inst/extdata/developer-scoring/stage19-3-1/wmfm-scoring-export-20260510-112328.json:1146:              "detail": "First clearly stated fitted magnitude found near effect wording."
inst/extdata/developer-scoring/stage19-3-1/wmfm-scoring-export-20260510-112328.json:1212:        "explanation": "Do average final exam marks differ by gender in this course example? The analysis compares average final exam marks (out of 100) for female and male students. The expected mark for female students is 55 (95% confidence interval 50 to 59), while for male students it is 51 (95% confidence interval 46 to 55). Although the point estimates differ by about 4 marks, the model does not show a clear difference-the data are consistent with patterns ranging from male students scoring slightly higher to female students scoring moderately higher. The model explains very little of the variation in exam marks (about 1%), indicating that gender accounts for only a small part of the differences observed in individual performance. There is no clear evidence of a difference in average exam marks by gender based on these data.",
inst/extdata/developer-scoring/stage19-3-1/wmfm-scoring-export-20260510-112328.json:1269:              "label": "Clarity score",
inst/extdata/developer-scoring/stage19-3-1/wmfm-scoring-export-20260510-112328.json:1379:              "label": "Clarity adequate",
inst/extdata/developer-scoring/stage19-3-1/wmfm-scoring-export-20260510-112328.json:1419:              "metric": "clarityAdequate",
inst/extdata/developer-scoring/stage19-3-1/wmfm-scoring-export-20260510-112328.json:1420:              "label": "Clarity adequate",
inst/extdata/developer-scoring/stage19-3-1/wmfm-scoring-export-20260510-112328.json:1426:              "metric": "clarityScore",
inst/extdata/developer-scoring/stage19-3-1/wmfm-scoring-export-20260510-112328.json:1427:              "label": "Clarity score",
inst/extdata/developer-scoring/stage19-3-1/wmfm-scoring-export-20260510-112328.json:1526:              "field": "effectMagnitude",
inst/extdata/developer-scoring/stage19-3-1/wmfm-scoring-export-20260510-112328.json:1527:              "label": "Effect magnitude",
inst/extdata/developer-scoring/stage19-3-1/wmfm-scoring-export-20260510-112328.json:1530:              "detail": "First clearly stated fitted magnitude found near effect wording."
inst/extdata/developer-scoring/stage19-3-1/wmfm-scoring-export-20260510-112154.json:75:          "label": "Clarity score",
inst/extdata/developer-scoring/stage19-3-1/wmfm-scoring-export-20260510-112154.json:185:          "label": "Clarity adequate",
inst/extdata/developer-scoring/stage19-3-1/wmfm-scoring-export-20260510-112154.json:225:          "metric": "clarityAdequate",
inst/extdata/developer-scoring/stage19-3-1/wmfm-scoring-export-20260510-112154.json:226:          "label": "Clarity adequate",
inst/extdata/developer-scoring/stage19-3-1/wmfm-scoring-export-20260510-112154.json:232:          "metric": "clarityScore",
inst/extdata/developer-scoring/stage19-3-1/wmfm-scoring-export-20260510-112154.json:233:          "label": "Clarity score",
inst/extdata/developer-scoring/stage19-3-1/wmfm-scoring-export-20260510-112154.json:311:          "field": "effectMagnitude",
inst/extdata/developer-scoring/stage19-3-1/wmfm-scoring-export-20260510-112154.json:312:          "label": "Effect magnitude",
inst/extdata/developer-scoring/stage19-3-1/wmfm-scoring-export-20260510-112154.json:315:          "detail": "First clearly stated fitted magnitude found near effect wording."
inst/extdata/developer-scoring/stage19-3-1/wmfm-scoring-export-20260510-112154.json:480:              "label": "Clarity score",
inst/extdata/developer-scoring/stage19-3-1/wmfm-scoring-export-20260510-112154.json:590:              "label": "Clarity adequate",
inst/extdata/developer-scoring/stage19-3-1/wmfm-scoring-export-20260510-112154.json:630:              "metric": "clarityAdequate",
inst/extdata/developer-scoring/stage19-3-1/wmfm-scoring-export-20260510-112154.json:631:              "label": "Clarity adequate",
inst/extdata/developer-scoring/stage19-3-1/wmfm-scoring-export-20260510-112154.json:637:              "metric": "clarityScore",
inst/extdata/developer-scoring/stage19-3-1/wmfm-scoring-export-20260510-112154.json:638:              "label": "Clarity score",
inst/extdata/developer-scoring/stage19-3-1/wmfm-scoring-export-20260510-112154.json:716:              "field": "effectMagnitude",
inst/extdata/developer-scoring/stage19-3-1/wmfm-scoring-export-20260510-112154.json:717:              "label": "Effect magnitude",
inst/extdata/developer-scoring/stage19-3-1/wmfm-scoring-export-20260510-112154.json:720:              "detail": "First clearly stated fitted magnitude found near effect wording."
inst/extdata/developer-scoring/stage19-3-1/wmfm-scoring-export-20260510-112154.json:843:              "label": "Clarity score",
inst/extdata/developer-scoring/stage19-3-1/wmfm-scoring-export-20260510-112154.json:953:              "label": "Clarity adequate",
inst/extdata/developer-scoring/stage19-3-1/wmfm-scoring-export-20260510-112154.json:993:              "metric": "clarityAdequate",
inst/extdata/developer-scoring/stage19-3-1/wmfm-scoring-export-20260510-112154.json:994:              "label": "Clarity adequate",
inst/extdata/developer-scoring/stage19-3-1/wmfm-scoring-export-20260510-112154.json:1000:              "metric": "clarityScore",
inst/extdata/developer-scoring/stage19-3-1/wmfm-scoring-export-20260510-112154.json:1001:              "label": "Clarity score",
inst/extdata/developer-scoring/stage19-3-1/wmfm-scoring-export-20260510-112154.json:1079:              "field": "effectMagnitude",
inst/extdata/developer-scoring/stage19-3-1/wmfm-scoring-export-20260510-112154.json:1080:              "label": "Effect magnitude",
inst/extdata/developer-scoring/stage19-3-1/wmfm-scoring-export-20260510-112154.json:1083:              "detail": "First clearly stated fitted magnitude found near effect wording."
inst/extdata/developer-scoring/stage19-3-1/wmfm-scoring-export-20260510-112154.json:1206:              "label": "Clarity score",
inst/extdata/developer-scoring/stage19-3-1/wmfm-scoring-export-20260510-112154.json:1316:              "label": "Clarity adequate",
inst/extdata/developer-scoring/stage19-3-1/wmfm-scoring-export-20260510-112154.json:1356:              "metric": "clarityAdequate",
inst/extdata/developer-scoring/stage19-3-1/wmfm-scoring-export-20260510-112154.json:1357:              "label": "Clarity adequate",
inst/extdata/developer-scoring/stage19-3-1/wmfm-scoring-export-20260510-112154.json:1363:              "metric": "clarityScore",
inst/extdata/developer-scoring/stage19-3-1/wmfm-scoring-export-20260510-112154.json:1364:              "label": "Clarity score",
inst/extdata/developer-scoring/stage19-3-1/wmfm-scoring-export-20260510-112154.json:1442:              "field": "effectMagnitude",
inst/extdata/developer-scoring/stage19-3-1/wmfm-scoring-export-20260510-112154.json:1443:              "label": "Effect magnitude",
inst/extdata/developer-scoring/stage19-3-1/wmfm-scoring-export-20260510-112154.json:1446:              "detail": "First clearly stated fitted magnitude found near effect wording."
inst/extdata/developer-scoring/stage19-3-1/wmfm-scoring-export-20260510-112245.json:75:          "label": "Clarity score",
inst/extdata/developer-scoring/stage19-3-1/wmfm-scoring-export-20260510-112245.json:185:          "label": "Clarity adequate",
inst/extdata/developer-scoring/stage19-3-1/wmfm-scoring-export-20260510-112245.json:225:          "metric": "clarityAdequate",
inst/extdata/developer-scoring/stage19-3-1/wmfm-scoring-export-20260510-112245.json:226:          "label": "Clarity adequate",
inst/extdata/developer-scoring/stage19-3-1/wmfm-scoring-export-20260510-112245.json:232:          "metric": "clarityScore",
inst/extdata/developer-scoring/stage19-3-1/wmfm-scoring-export-20260510-112245.json:233:          "label": "Clarity score",
inst/extdata/developer-scoring/stage19-3-1/wmfm-scoring-export-20260510-112245.json:311:          "field": "effectMagnitude",
inst/extdata/developer-scoring/stage19-3-1/wmfm-scoring-export-20260510-112245.json:312:          "label": "Effect magnitude",
inst/extdata/developer-scoring/stage19-3-1/wmfm-scoring-export-20260510-112245.json:315:          "detail": "First clearly stated fitted magnitude found near effect wording."
inst/extdata/developer-scoring/stage19-3-1/wmfm-scoring-export-20260510-112245.json:480:              "label": "Clarity score",
inst/extdata/developer-scoring/stage19-3-1/wmfm-scoring-export-20260510-112245.json:590:              "label": "Clarity adequate",
inst/extdata/developer-scoring/stage19-3-1/wmfm-scoring-export-20260510-112245.json:630:              "metric": "clarityAdequate",
inst/extdata/developer-scoring/stage19-3-1/wmfm-scoring-export-20260510-112245.json:631:              "label": "Clarity adequate",
inst/extdata/developer-scoring/stage19-3-1/wmfm-scoring-export-20260510-112245.json:637:              "metric": "clarityScore",
inst/extdata/developer-scoring/stage19-3-1/wmfm-scoring-export-20260510-112245.json:638:              "label": "Clarity score",
inst/extdata/developer-scoring/stage19-3-1/wmfm-scoring-export-20260510-112245.json:716:              "field": "effectMagnitude",
inst/extdata/developer-scoring/stage19-3-1/wmfm-scoring-export-20260510-112245.json:717:              "label": "Effect magnitude",
inst/extdata/developer-scoring/stage19-3-1/wmfm-scoring-export-20260510-112245.json:720:              "detail": "First clearly stated fitted magnitude found near effect wording."
inst/extdata/developer-scoring/stage19-3-1/wmfm-scoring-export-20260510-112245.json:843:              "label": "Clarity score",
inst/extdata/developer-scoring/stage19-3-1/wmfm-scoring-export-20260510-112245.json:953:              "label": "Clarity adequate",
inst/extdata/developer-scoring/stage19-3-1/wmfm-scoring-export-20260510-112245.json:993:              "metric": "clarityAdequate",
inst/extdata/developer-scoring/stage19-3-1/wmfm-scoring-export-20260510-112245.json:994:              "label": "Clarity adequate",
inst/extdata/developer-scoring/stage19-3-1/wmfm-scoring-export-20260510-112245.json:1000:              "metric": "clarityScore",
inst/extdata/developer-scoring/stage19-3-1/wmfm-scoring-export-20260510-112245.json:1001:              "label": "Clarity score",
inst/extdata/developer-scoring/stage19-3-1/wmfm-scoring-export-20260510-112245.json:1079:              "field": "effectMagnitude",
inst/extdata/developer-scoring/stage19-3-1/wmfm-scoring-export-20260510-112245.json:1080:              "label": "Effect magnitude",
inst/extdata/developer-scoring/stage19-3-1/wmfm-scoring-export-20260510-112245.json:1083:              "detail": "First clearly stated fitted magnitude found near effect wording."
inst/extdata/developer-scoring/stage19-3-1/wmfm-scoring-export-20260510-112245.json:1206:              "label": "Clarity score",
inst/extdata/developer-scoring/stage19-3-1/wmfm-scoring-export-20260510-112245.json:1316:              "label": "Clarity adequate",
inst/extdata/developer-scoring/stage19-3-1/wmfm-scoring-export-20260510-112245.json:1356:              "metric": "clarityAdequate",
inst/extdata/developer-scoring/stage19-3-1/wmfm-scoring-export-20260510-112245.json:1357:              "label": "Clarity adequate",
inst/extdata/developer-scoring/stage19-3-1/wmfm-scoring-export-20260510-112245.json:1363:              "metric": "clarityScore",
inst/extdata/developer-scoring/stage19-3-1/wmfm-scoring-export-20260510-112245.json:1364:              "label": "Clarity score",
inst/extdata/developer-scoring/stage19-3-1/wmfm-scoring-export-20260510-112245.json:1442:              "field": "effectMagnitude",
inst/extdata/developer-scoring/stage19-3-1/wmfm-scoring-export-20260510-112245.json:1443:              "label": "Effect magnitude",
inst/extdata/developer-scoring/stage19-3-1/wmfm-scoring-export-20260510-112245.json:1446:              "detail": "First clearly stated fitted magnitude found near effect wording."
inst/extdata/developer-scoring/stage19-3-1/wmfm-scoring-export-20260510-112425.json:12:    "formula": "Pass ~ StudyHours",
inst/extdata/developer-scoring/stage19-3-1/wmfm-scoring-export-20260510-112425.json:15:    "researchQuestion": "How does the test mark relate to the probability of passing?"
inst/extdata/developer-scoring/stage19-3-1/wmfm-scoring-export-20260510-112425.json:18:    "explanation": "We want to understand how a student's test mark is related to the probability that they will pass. The outcome is whether a student passes (coded as yes or no). The predictor is the test mark, recorded out of 100. For a student with an average test mark of around 11.6, the estimated probability of passing is 84%. The 95% confidence interval runs from 73% to 91%. As the test mark increases, the odds of passing rise substantially. For each additional an increase of one unit in the test mark, the odds of passing are multiplied by about 2. The 95% confidence interval runs from 1.6 to 2.5. This means that even modest increases in the test mark are associated with considerably higher chances of passing. Overall, higher test marks are strongly associated with a greater probability of passing, with the data indicating that each an increase of one unit in the test mark roughly doubles the odds of passing.",
inst/extdata/developer-scoring/stage19-3-1/wmfm-scoring-export-20260510-112425.json:75:          "label": "Clarity score",
inst/extdata/developer-scoring/stage19-3-1/wmfm-scoring-export-20260510-112425.json:185:          "label": "Clarity adequate",
inst/extdata/developer-scoring/stage19-3-1/wmfm-scoring-export-20260510-112425.json:225:          "metric": "clarityAdequate",
inst/extdata/developer-scoring/stage19-3-1/wmfm-scoring-export-20260510-112425.json:226:          "label": "Clarity adequate",
inst/extdata/developer-scoring/stage19-3-1/wmfm-scoring-export-20260510-112425.json:232:          "metric": "clarityScore",
inst/extdata/developer-scoring/stage19-3-1/wmfm-scoring-export-20260510-112425.json:233:          "label": "Clarity score",
inst/extdata/developer-scoring/stage19-3-1/wmfm-scoring-export-20260510-112425.json:311:          "field": "effectMagnitude",
inst/extdata/developer-scoring/stage19-3-1/wmfm-scoring-export-20260510-112425.json:312:          "label": "Effect magnitude",
inst/extdata/developer-scoring/stage19-3-1/wmfm-scoring-export-20260510-112425.json:315:          "detail": "First clearly stated fitted magnitude found near effect wording."
inst/extdata/developer-scoring/stage19-3-1/wmfm-scoring-export-20260510-112425.json:423:        "explanation": "We want to understand how a student's test mark is related to the probability that they will pass. The outcome is whether a student passes (coded as yes or no), and the predictor is the test mark (out of 100). For a student with an average study time of around 11.6 hours, the estimated probability of passing is 84%. The 95% confidence interval runs from 73% to 91%. For each an increase of one unit in the test mark, the odds of passing are multiplied by about 2. The 95% confidence interval runs from 1.6 to 2.5, indicating that even modest increases in the test mark are associated with substantially higher odds of passing. Overall, higher test marks are strongly associated with greater probability of passing, with each an increase of one unit in the test mark roughly doubling the odds of passing on average.",
inst/extdata/developer-scoring/stage19-3-1/wmfm-scoring-export-20260510-112425.json:480:              "label": "Clarity score",
inst/extdata/developer-scoring/stage19-3-1/wmfm-scoring-export-20260510-112425.json:590:              "label": "Clarity adequate",
inst/extdata/developer-scoring/stage19-3-1/wmfm-scoring-export-20260510-112425.json:630:              "metric": "clarityAdequate",
inst/extdata/developer-scoring/stage19-3-1/wmfm-scoring-export-20260510-112425.json:631:              "label": "Clarity adequate",
inst/extdata/developer-scoring/stage19-3-1/wmfm-scoring-export-20260510-112425.json:637:              "metric": "clarityScore",
inst/extdata/developer-scoring/stage19-3-1/wmfm-scoring-export-20260510-112425.json:638:              "label": "Clarity score",
inst/extdata/developer-scoring/stage19-3-1/wmfm-scoring-export-20260510-112425.json:716:              "field": "effectMagnitude",
inst/extdata/developer-scoring/stage19-3-1/wmfm-scoring-export-20260510-112425.json:717:              "label": "Effect magnitude",
inst/extdata/developer-scoring/stage19-3-1/wmfm-scoring-export-20260510-112425.json:720:              "detail": "First clearly stated fitted magnitude found near effect wording."
inst/extdata/developer-scoring/stage19-3-1/wmfm-scoring-export-20260510-112425.json:786:        "explanation": "We want to understand how a student's test mark is related to the probability that they will pass. The outcome is whether a student passes (coded as yes or no), and the predictor is the test mark (out of 100). For a student with average study time of around 11.6 hours, the estimated probability of passing is 84%. The 95% confidence interval runs from 73% to 91%. For each an increase of one unit in the test mark, the odds of passing are multiplied by about 2. The 95% confidence interval runs from 1.6 to 2.5, indicating that higher test marks are associated with substantially higher odds of passing. Overall, the data indicate that higher test marks are strongly associated with a greater probability of passing, with each additional mark point roughly doubling the odds of passing on average.",
inst/extdata/developer-scoring/stage19-3-1/wmfm-scoring-export-20260510-112425.json:843:              "label": "Clarity score",
inst/extdata/developer-scoring/stage19-3-1/wmfm-scoring-export-20260510-112425.json:953:              "label": "Clarity adequate",
inst/extdata/developer-scoring/stage19-3-1/wmfm-scoring-export-20260510-112425.json:993:              "metric": "clarityAdequate",
inst/extdata/developer-scoring/stage19-3-1/wmfm-scoring-export-20260510-112425.json:994:              "label": "Clarity adequate",
inst/extdata/developer-scoring/stage19-3-1/wmfm-scoring-export-20260510-112425.json:1000:              "metric": "clarityScore",
inst/extdata/developer-scoring/stage19-3-1/wmfm-scoring-export-20260510-112425.json:1001:              "label": "Clarity score",
inst/extdata/developer-scoring/stage19-3-1/wmfm-scoring-export-20260510-112425.json:1079:              "field": "effectMagnitude",
inst/extdata/developer-scoring/stage19-3-1/wmfm-scoring-export-20260510-112425.json:1080:              "label": "Effect magnitude",
inst/extdata/developer-scoring/stage19-3-1/wmfm-scoring-export-20260510-112425.json:1083:              "detail": "First clearly stated fitted magnitude found near effect wording."
inst/extdata/developer-scoring/stage19-3-1/wmfm-scoring-export-20260510-112425.json:1149:        "explanation": "We want to understand how a student's test mark is related to the probability that they will pass. The outcome is whether a student passes (coded as yes or no), and the predictor is the test mark (out of 100). For a student with average study time of around 11.6 hours, the estimated probability of passing is 84%. The 95% confidence interval runs from 73% to 91%. For each an increase of one unit in the test mark, the odds of passing are multiplied by about 2. The 95% confidence interval runs from 1.6 to 2.5, indicating that higher test marks are associated with substantially higher odds of passing. Overall, the data indicate that higher test marks are strongly associated with a greater probability of passing, with each additional mark point roughly doubling the odds of passing on average.",
inst/extdata/developer-scoring/stage19-3-1/wmfm-scoring-export-20260510-112425.json:1206:              "label": "Clarity score",
inst/extdata/developer-scoring/stage19-3-1/wmfm-scoring-export-20260510-112425.json:1316:              "label": "Clarity adequate",
inst/extdata/developer-scoring/stage19-3-1/wmfm-scoring-export-20260510-112425.json:1356:              "metric": "clarityAdequate",
inst/extdata/developer-scoring/stage19-3-1/wmfm-scoring-export-20260510-112425.json:1357:              "label": "Clarity adequate",
inst/extdata/developer-scoring/stage19-3-1/wmfm-scoring-export-20260510-112425.json:1363:              "metric": "clarityScore",
inst/extdata/developer-scoring/stage19-3-1/wmfm-scoring-export-20260510-112425.json:1364:              "label": "Clarity score",
inst/extdata/developer-scoring/stage19-3-1/wmfm-scoring-export-20260510-112425.json:1442:              "field": "effectMagnitude",
inst/extdata/developer-scoring/stage19-3-1/wmfm-scoring-export-20260510-112425.json:1443:              "label": "Effect magnitude",
inst/extdata/developer-scoring/stage19-3-1/wmfm-scoring-export-20260510-112425.json:1446:              "detail": "First clearly stated fitted magnitude found near effect wording."
inst/extdata/examples/test-SG-1/context.md:4:Pass is coded 0/1. Gender and Attend are categorical predictors.
inst/extdata/examples/test-SG-1/sg-course.csv:1:Exam,Test,Assignment,Gender,Attend,StudyHours,Pass
inst/extdata/examples/test-SG-1/test-SG-1.spec.yml:5:dataObject: course.df
inst/extdata/examples/test-SG-1/test-SG-1.spec.yml:6:dataTransform: courseDfScoringGradingAliases
inst/extdata/examples/DiamondsII/DiamondsII.spec.yml:1:displayName: Diamonds II
inst/extdata/examples/DiamondsII/DiamondsII.spec.yml:3:formula: log(price) ~ log(carat)
inst/extdata/examples/DiamondsII/DiamondsII.spec.yml:6:dataObject: diamonds
inst/extdata/examples/DiamondsII/DiamondsII.spec.yml:7:dataTransform: diamondsPlainFactors
inst/extdata/examples/DiamondsII/DiamondsII.spec.yml:8:researchQuestion: Can we predict the price of diamonds on the basis of weight?
inst/extdata/examples/test-course-follow-up/test-course-follow-up.spec.yml:1:displayName: test-Course Follow-Up
inst/extdata/examples/test-course-follow-up/test-course-follow-up.spec.yml:6:dataObject: course.df
inst/extdata/examples/test-06-G11F/test-06-G11F.spec.yml:5:dataObject: course.df
inst/extdata/examples/Quakes/Quakes.Rmd:2:The Gutenberg-Richter law says that the expected frequency of earthquakes decreases multiplicatively with their magnitude. The formula is $$\log_{10} N = a - b M,$$
inst/extdata/examples/Quakes/Quakes.Rmd:3:where $N$ is the expected number of earthquakes of magnitude $M$ or more on the Richter scale. Here, $a$ and $b$ are unknown parameters. 
inst/extdata/examples/Quakes/Quakes.Rmd:6:where $Y$ is the number of number of earthquakes of magnitude between $x-\delta$ and $x+\delta$.\footnote{In the above formula, $\beta_0$ and $\beta_1$ depend on $a$, $b$ and $\delta$ in a complicated way that we are not going to concern ourselves with.}
inst/extdata/examples/Quakes/Quakes.Rmd:10:* `Magnitude`: The strength of a recorded earthquake.
inst/extdata/examples/Quakes/Quakes.Rmd:11:* `Locn`: A two-level factor which describe the location of the earthquakes. 
inst/extdata/examples/Quakes/Quakes.Rmd:13:* `Freq`: The number of earthquakes recorded at `Locn` with magnitude `Magnitude`.
inst/extdata/examples/Quakes/Quakes.Rmd:16:The research question is to quantify the rate of decrease in earthquake frequency (with increasing magnitude) in both California and Washington states, and to assess whether these rates are the same.
inst/extdata/examples/Quakes/Quakes.spec.yml:1:name: earthquake
inst/extdata/examples/Quakes/Quakes.spec.yml:3:formula: Freq ~ Magnitude * Locn
inst/extdata/examples/Quakes/Quakes.spec.yml:4:data: Quakes.df.rda
inst/extdata/examples/Quakes/Quakes.spec.yml:5:context: Quakes.Rmd
inst/extdata/examples/Quakes/Quakes.spec.yml:6:researchQuestion: How quickly does expected earthquake frequency decrease as magnitude increases in California and Washington, and do those rates of decrease appear to differ between the two locations?
inst/extdata/examples/test-09-B01F/test-09-B01F.spec.yml:2:formula: Pass ~ Assign
inst/extdata/examples/test-09-B01F/test-09-B01F.spec.yml:5:dataObject: course.df
inst/extdata/examples/test-09-B01F/test-09-B01F.spec.yml:6:researchQuestion: How does the probability of passing tend to change as the assignment mark changes?
inst/extdata/examples/test-07-G11T/test-07-G11T.spec.yml:5:dataObject: course.df
inst/extdata/examples/Course/Course.spec.yml:5:dataObject: course.df
inst/extdata/examples/test-23-P11F-followup/test-23-P11F-followup.spec.yml:2:formula: Freq ~ Magnitude + Locn
inst/extdata/examples/test-23-P11F-followup/test-23-P11F-followup.spec.yml:3:data: ../Quakes/Quakes.df.rda
inst/extdata/examples/test-23-P11F-followup/test-23-P11F-followup.spec.yml:4:context: ../Quakes/Quakes.Rmd
inst/extdata/examples/test-23-P11F-followup/test-23-P11F-followup.spec.yml:5:researchQuestion: How do magnitude and location relate to expected earthquake frequency?
inst/extdata/examples/test-23-P11F-followup/test-23-P11F-followup.spec.yml:6:followupQuestion: What earthquake frequency would you expect for Magnitude = 5.4 and Locn = WA?
inst/extdata/examples/test-20-B01F-followup/test-20-B01F-followup.spec.yml:2:formula: Pass ~ Assign
inst/extdata/examples/test-20-B01F-followup/test-20-B01F-followup.spec.yml:5:dataObject: course.df
inst/extdata/examples/test-20-B01F-followup/test-20-B01F-followup.spec.yml:6:researchQuestion: How does the probability of passing tend to change as the assignment mark changes?
inst/extdata/examples/test-20-B01F-followup/test-20-B01F-followup.spec.yml:7:followupQuestion: A student scored 15 on Assign. What probability of passing would you predict?
inst/extdata/examples/test-03-G10F/test-03-G10F.spec.yml:5:dataObject: course.df
inst/extdata/examples/test-02-G01F/test-02-G01F.spec.yml:5:dataObject: course.df
inst/extdata/examples/test-01-G00F/test-01-G00F.spec.yml:5:dataObject: course.df
inst/extdata/examples/test-01-G00F/test-01-G00F.spec.yml:6:researchQuestion: What is the average final exam mark in the course data?
inst/extdata/examples/test-12-B20T/test-12-B20T.spec.yml:2:formula: Pass ~ Attend * Gender
inst/extdata/examples/test-12-B20T/test-12-B20T.spec.yml:5:dataObject: course.df
inst/extdata/examples/test-12-B20T/test-12-B20T.spec.yml:6:researchQuestion: Does the attendance difference in odds of passing appear to differ by gender?
inst/extdata/examples/QuakesUnitChange0_1/QuakesUnitChange0_1.Rmd:2:The Gutenberg-Richter law says that the expected frequency of earthquakes decreases multiplicatively as earthquake magnitude increases.
inst/extdata/examples/QuakesUnitChange0_1/QuakesUnitChange0_1.Rmd:6:* `Magnitude`: The strength of a recorded earthquake.
inst/extdata/examples/QuakesUnitChange0_1/QuakesUnitChange0_1.Rmd:7:* `Freq`: The number of earthquakes recorded for an interval of magnitudes.
inst/extdata/examples/QuakesUnitChange0_1/QuakesUnitChange0_1.Rmd:10:The research question is to describe how the expected earthquake frequency changes as magnitude increases, with the effect expressed for a 0.1 magnitude increase.
inst/extdata/examples/QuakesUnitChange0_1/QuakesUnitChange0_1.spec.yml:1:displayName: Quakes-unit-change
inst/extdata/examples/QuakesUnitChange0_1/QuakesUnitChange0_1.spec.yml:3:formula: Freq ~ Magnitude
inst/extdata/examples/QuakesUnitChange0_1/QuakesUnitChange0_1.spec.yml:4:data: Quakes.df.rda
inst/extdata/examples/QuakesUnitChange0_1/QuakesUnitChange0_1.spec.yml:5:context: QuakesUnitChange0_1.Rmd
inst/extdata/examples/QuakesUnitChange0_1/QuakesUnitChange0_1.spec.yml:6:researchQuestion: How does expected earthquake frequency change as magnitude increases?
inst/extdata/examples/QuakesUnitChange0_1/QuakesUnitChange0_1.spec.yml:7:followupQuestion: Can you explain the magnitude effect for a 0.1 magnitude increase?
inst/extdata/examples/test-05-G20T/test-05-G20T.spec.yml:5:dataObject: course.df
inst/extdata/examples/test-04-G20F/test-04-G20F.spec.yml:5:dataObject: course.df
inst/extdata/examples/DiamondsIII/DiamondsIII.spec.yml:1:displayName: Diamonds III
inst/extdata/examples/DiamondsIII/DiamondsIII.spec.yml:3:formula: log(price) ~ log(carat)
inst/extdata/examples/DiamondsIII/DiamondsIII.spec.yml:6:dataObject: diamonds
inst/extdata/examples/DiamondsIII/DiamondsIII.spec.yml:7:dataTransform: diamondsPlainFactors
inst/extdata/examples/DiamondsIII/DiamondsIII.spec.yml:8:researchQuestion: Can we predict the price of diamonds on the basis of weight?
inst/extdata/examples/DiamondsIII/DiamondsIII.spec.yml:9:followupQuestion: Can you express the weight effect for a 0.1 carat increase?
inst/extdata/examples/test-24-B01F-followup-odds/test-24-B01F-followup-odds.spec.yml:2:formula: Pass ~ Assign
inst/extdata/examples/test-24-B01F-followup-odds/test-24-B01F-followup-odds.spec.yml:5:dataObject: course.df
inst/extdata/examples/test-24-B01F-followup-odds/test-24-B01F-followup-odds.spec.yml:6:researchQuestion: How does the odds of passing tend to change as the assignment mark changes?
inst/extdata/examples/test-24-B01F-followup-odds/test-24-B01F-followup-odds.spec.yml:7:followupQuestion: A student scored 15 on Assign. What odds of passing would you predict?
inst/extdata/examples/test-22-P01F-followup/test-22-P01F-followup.spec.yml:2:formula: Freq ~ Magnitude
inst/extdata/examples/test-22-P01F-followup/test-22-P01F-followup.spec.yml:3:data: ../Quakes/Quakes.df.rda
inst/extdata/examples/test-22-P01F-followup/test-22-P01F-followup.spec.yml:4:context: ../Quakes/Quakes.Rmd
inst/extdata/examples/test-22-P01F-followup/test-22-P01F-followup.spec.yml:5:researchQuestion: How does expected earthquake frequency change as magnitude increases?
inst/extdata/examples/test-22-P01F-followup/test-22-P01F-followup.spec.yml:6:followupQuestion: What earthquake frequency would you expect for Magnitude = 5.4?
inst/extdata/examples/test-21-B11F-followup/test-21-B11F-followup.spec.yml:2:formula: Pass ~ Attend + Test
inst/extdata/examples/test-21-B11F-followup/test-21-B11F-followup.spec.yml:5:dataObject: course.df
inst/extdata/examples/test-21-B11F-followup/test-21-B11F-followup.spec.yml:6:researchQuestion: How do attendance and test mark relate to the probability of passing?
inst/extdata/examples/test-21-B11F-followup/test-21-B11F-followup.spec.yml:7:followupQuestion: Suppose a student has Test = 10 and Attend = Yes. What probability of passing would you predict?
inst/extdata/examples/test-SG-5/test-SG-5.spec.yml:5:dataObject: course.df
inst/extdata/examples/test-SG-5/test-SG-5.spec.yml:6:dataTransform: courseDfScoringGradingAliases
inst/extdata/examples/test-SG-5/context.md:4:Pass is coded 0/1. Gender and Attend are categorical predictors.
inst/extdata/examples/test-SG-5/sg-course.csv:1:Exam,Test,Assignment,Gender,Attend,StudyHours,Pass
inst/extdata/examples/test-SG-2/context.md:4:Pass is coded 0/1. Gender and Attend are categorical predictors.
inst/extdata/examples/test-SG-2/sg-course.csv:1:Exam,Test,Assignment,Gender,Attend,StudyHours,Pass
inst/extdata/examples/test-SG-2/test-SG-2.spec.yml:5:dataObject: course.df
inst/extdata/examples/test-SG-2/test-SG-2.spec.yml:6:dataTransform: courseDfScoringGradingAliases
inst/extdata/examples/DiamondsIV/DiamondsIV.spec.yml:1:displayName: Diamonds IV
inst/extdata/examples/DiamondsIV/DiamondsIV.spec.yml:3:formula: log(price) ~ log(carat) + cut + color + clarity
inst/extdata/examples/DiamondsIV/DiamondsIV.spec.yml:6:dataObject: diamonds
inst/extdata/examples/DiamondsIV/DiamondsIV.spec.yml:7:dataTransform: diamondsPlainFactors
inst/extdata/examples/DiamondsIV/DiamondsIV.spec.yml:8:researchQuestion: Can we predict the price of diamonds on the basis of weight?
inst/extdata/examples/DiamondsIV/DiamondsIV.spec.yml:9:followupQuestion: Does adjusting for cut, color, and clarity improve our predictions substantially?
inst/extdata/examples/DiamondsIV/DiamondsIV.spec.yml:12:  - cut
inst/extdata/examples/DiamondsIV/DiamondsIV.spec.yml:13:  - color
inst/extdata/examples/DiamondsIV/DiamondsIV.spec.yml:14:  - clarity
inst/extdata/examples/test-SG-3/context.md:4:Pass is coded 0/1. Gender and Attend are categorical predictors.
inst/extdata/examples/test-SG-3/sg-course.csv:1:Exam,Test,Assignment,Gender,Attend,StudyHours,Pass
inst/extdata/examples/test-SG-3/test-SG-3.spec.yml:5:dataObject: course.df
inst/extdata/examples/test-SG-3/test-SG-3.spec.yml:6:dataTransform: courseDfScoringGradingAliases
inst/extdata/examples/test-SG-3/test-SG-3.spec.yml:8:researchQuestion: Do average final exam marks differ by gender in this course example?
inst/extdata/examples/test-SG-4/test-SG-4.spec.yml:2:formula: Pass ~ StudyHours
inst/extdata/examples/test-SG-4/test-SG-4.spec.yml:5:dataObject: course.df
inst/extdata/examples/test-SG-4/test-SG-4.spec.yml:6:dataTransform: courseDfScoringGradingAliases
inst/extdata/examples/test-SG-4/test-SG-4.spec.yml:8:researchQuestion: How does the test mark relate to the probability of passing?
inst/extdata/examples/test-SG-4/context.md:4:Pass is coded 0/1. Gender and Attend are categorical predictors.
inst/extdata/examples/test-SG-4/sg-course.csv:1:Exam,Test,Assignment,Gender,Attend,StudyHours,Pass
inst/extdata/examples/test-14-B11T/test-14-B11T.spec.yml:2:formula: Pass ~ Attend * Assign
inst/extdata/examples/test-14-B11T/test-14-B11T.spec.yml:5:dataObject: course.df
inst/extdata/examples/test-14-B11T/test-14-B11T.spec.yml:6:researchQuestion: Does the relationship between assignment mark and the odds of passing appear to differ by attendance group?
inst/extdata/examples/test-11-B20F/test-11-B20F.spec.yml:2:formula: Pass ~ Attend + Gender
inst/extdata/examples/test-11-B20F/test-11-B20F.spec.yml:5:dataObject: course.df
inst/extdata/examples/test-11-B20F/test-11-B20F.spec.yml:6:researchQuestion: Do the odds of passing differ by attendance and gender, without allowing the attendance difference to depend on gender?
inst/extdata/examples/test-19-P11T/test-19-P11T.spec.yml:2:formula: Freq ~ Locn * Magnitude
inst/extdata/examples/test-19-P11T/test-19-P11T.spec.yml:3:data: ../Quakes/Quakes.df.rda
inst/extdata/examples/test-19-P11T/test-19-P11T.spec.yml:4:context: ../Quakes/Quakes.Rmd
inst/extdata/examples/test-19-P11T/test-19-P11T.spec.yml:5:researchQuestion: Does the relationship between magnitude and expected earthquake frequency appear to differ by location?
inst/extdata/examples/test-13-B11F/test-13-B11F.spec.yml:2:formula: Pass ~ Attend + Assign
inst/extdata/examples/test-13-B11F/test-13-B11F.spec.yml:5:dataObject: course.df
inst/extdata/examples/test-13-B11F/test-13-B11F.spec.yml:6:researchQuestion: How do attendance and assignment mark relate to the odds of passing, without allowing the assignment-mark slope to differ by attendance?
inst/extdata/examples/test-10-B10F/test-10-B10F.spec.yml:2:formula: Pass ~ Attend
inst/extdata/examples/test-10-B10F/test-10-B10F.spec.yml:5:dataObject: course.df
inst/extdata/examples/test-10-B10F/test-10-B10F.spec.yml:6:researchQuestion: Do students who regularly attended class appear to have different odds of passing?
inst/extdata/examples/DiamondsUnitChange/DiamondsUnitChange.spec.yml:1:displayName: Diamonds
inst/extdata/examples/DiamondsUnitChange/DiamondsUnitChange.spec.yml:3:formula: price ~ weight
inst/extdata/examples/DiamondsUnitChange/DiamondsUnitChange.spec.yml:6:dataObject: diamonds.df
inst/extdata/examples/DiamondsUnitChange/DiamondsUnitChange.spec.yml:7:researchQuestion: How does the price of a diamond change as its weight in carats increases?
inst/extdata/examples/DiamondsUnitChange/DiamondsUnitChange.spec.yml:8:followupQuestion: Can you explain the weight effect for a 0.1 carat increase?
inst/extdata/examples/test-08-B00F/test-08-B00F.spec.yml:2:formula: Pass ~ 1
inst/extdata/examples/test-08-B00F/test-08-B00F.spec.yml:5:dataObject: course.df
inst/extdata/examples/test-08-B00F/test-08-B00F.spec.yml:6:researchQuestion: What is the overall probability of passing in the course data?
inst/extdata/examples/test-17-P10F/test-17-P10F.spec.yml:2:formula: Freq ~ Locn
inst/extdata/examples/test-17-P10F/test-17-P10F.spec.yml:3:data: ../Quakes/Quakes.df.rda
inst/extdata/examples/test-17-P10F/test-17-P10F.spec.yml:4:context: ../Quakes/Quakes.Rmd
inst/extdata/examples/test-17-P10F/test-17-P10F.spec.yml:5:researchQuestion: Does expected earthquake frequency differ between the two locations?
inst/extdata/examples/test-16-P01F/test-16-P01F.spec.yml:2:formula: Freq ~ Magnitude
inst/extdata/examples/test-16-P01F/test-16-P01F.spec.yml:3:data: ../Quakes/Quakes.df.rda
inst/extdata/examples/test-16-P01F/test-16-P01F.spec.yml:4:context: ../Quakes/Quakes.Rmd
inst/extdata/examples/test-16-P01F/test-16-P01F.spec.yml:5:researchQuestion: How does expected earthquake frequency change as magnitude increases?
inst/extdata/examples/test-15-P00F/test-15-P00F.spec.yml:2:formula: Freq ~ 1
inst/extdata/examples/test-15-P00F/test-15-P00F.spec.yml:3:data: ../Quakes/Quakes.df.rda
inst/extdata/examples/test-15-P00F/test-15-P00F.spec.yml:4:context: ../Quakes/Quakes.Rmd
inst/extdata/examples/test-15-P00F/test-15-P00F.spec.yml:5:researchQuestion: What is the average expected earthquake count per observation in the quake data?
inst/extdata/examples/test-18-P11F/test-18-P11F.spec.yml:2:formula: Freq ~ Locn + Magnitude
inst/extdata/examples/test-18-P11F/test-18-P11F.spec.yml:3:data: ../Quakes/Quakes.df.rda
inst/extdata/examples/test-18-P11F/test-18-P11F.spec.yml:4:context: ../Quakes/Quakes.Rmd
inst/extdata/examples/test-18-P11F/test-18-P11F.spec.yml:5:researchQuestion: How do location and magnitude relate to expected earthquake frequency, without allowing the magnitude slope to differ by location?
inst/extdata/examples/test-25-B11F-followup-odds/test-25-B11F-followup-odds.spec.yml:2:formula: Pass ~ Attend + Test
inst/extdata/examples/test-25-B11F-followup-odds/test-25-B11F-followup-odds.spec.yml:5:dataObject: course.df
inst/extdata/examples/test-25-B11F-followup-odds/test-25-B11F-followup-odds.spec.yml:6:researchQuestion: How do attendance and test mark relate to the odds of passing?
inst/extdata/examples/test-25-B11F-followup-odds/test-25-B11F-followup-odds.spec.yml:7:followupQuestion: Suppose a student has Test = 10 and Attend = Yes. What odds of passing would you predict?
inst/extdata/examples/Oysters/Oysters.spec.yml:2:formula: Oysters ~ Site
inst/extdata/examples/Oysters/Oysters.spec.yml:5:dataObject: oysters.df
inst/extdata/examples/Oysters/Oysters.spec.yml:6:researchQuestion: Does the expected number of oysters appear to differ between the study sites?
```

## Same terms in tests, examples, docs for comparison

```text
tests/testthat/test-app-server-data-load-helpers.R:3:    buildDelimitedFileReadFailedMessage(),
tests/testthat/test-app-server-data-load-helpers.R:4:    "Failed to read file with the chosen separator."
tests/testthat/test-app-server-data-load-helpers.R:25:    buildPackageDatasetLoadFailedMessage("myData", "myPackage"),
tests/testthat/test-equation-render.R:77:    pass = factor(c("Fail", "Pass", "Fail", "Pass", "Fail", "Pass", "Fail", "Pass")),
tests/testthat/test-equation-render.R:81:  model = glm(pass ~ x, data = df, family = binomial(link = "logit"))
tests/testthat/test-equation-render.R:86:  expect_match(out[[1]]$linearPredictor, '^logit\\(Pr\\(pass = "Pass"\\)\\) = ')
tests/testthat/test-equation-render.R:87:  expect_match(out[[1]]$oddsScale, '^Odds\\(pass = "Pass" vs pass = "Fail"\\) = exp\\(')
tests/testthat/test-equation-render.R:88:  expect_match(out[[1]]$responseScale, '^Pr\\(pass = "Pass"\\) = exp\\(')
tests/testthat/test-scoreWmfmRunWithLlm.R:13:    '"clarityAdequate":2,',
tests/testthat/test-scoreWmfmRunWithLlm.R:20:    '"clarityScore":1.9,',
tests/testthat/test-scoreWmfmRunWithLlm.R:23:    '"overallPass":true,',
tests/testthat/test-scoreWmfmRunWithLlm.R:35:    '"clarityAdequate":"ok",',
tests/testthat/test-scoreWmfmRunWithLlm.R:76:    '"clarityAdequate":2,',
tests/testthat/test-scoreWmfmRunWithLlm.R:83:    '"clarityScore":2,',
tests/testthat/test-scoreWmfmRunWithLlm.R:86:    '"overallPass":true,',
tests/testthat/test-scoreWmfmRunWithLlm.R:98:    '"clarityAdequate":"ok",',
tests/testthat/test-model-question-unit-change.R:3:    Price = c(102, 127, 164, 187, 225, 247),
tests/testthat/test-model-question-unit-change.R:4:    Carat = c(0.20, 0.25, 0.30, 0.35, 0.40, 0.45)
tests/testthat/test-model-question-unit-change.R:6:  model = stats::lm(Price ~ Carat, data = df)
tests/testthat/test-model-question-unit-change.R:7:  payload = classifyModelFollowupQuestion("Explain the Carat effect for a 0.1-unit increase")
tests/testthat/test-model-question-unit-change.R:10:  refCoef = unname(stats::coef(model)[["Carat"]])
tests/testthat/test-model-question-unit-change.R:11:  refCi = stats::confint(model, parm = "Carat")
tests/testthat/test-model-question-unit-change.R:40:    Price = c(102, 127, 164, 187, 225, 247),
tests/testthat/test-model-question-unit-change.R:41:    Carat = c(0.20, 0.25, 0.30, 0.35, 0.40, 0.45)
tests/testthat/test-model-question-unit-change.R:43:  model = stats::lm(Price ~ Carat, data = df)
tests/testthat/test-model-question-unit-change.R:44:  payload = classifyModelFollowupQuestion("Explain the Carat effect for a 0.1-unit increase")
tests/testthat/test-model-question-unit-change.R:53:  testthat::expect_match(prompt, "Requested predictor: Carat", fixed = TRUE)
tests/testthat/test-model-question-unit-change.R:76:    Passed = c(0, 0, 1, 0, 1, 1, 0, 1, 1, 1),
tests/testthat/test-model-question-unit-change.R:79:  model = stats::glm(Passed ~ Hours, data = df, family = stats::binomial())
tests/testthat/test-buildMetricEvidenceSummary.R:38:    metric = "clarityAdequate"
tests/testthat/test-buildMetricEvidenceSummary.R:42:  expect_equal(out$metric, "clarityAdequate")
tests/testthat/test-buildModelExplanationAudit.R:169:    pass = factor(c("Fail", "Fail", "Pass", "Pass", "Pass", "Fail")),
tests/testthat/test-buildModelExplanationAudit.R:174:    pass ~ x,
tests/testthat/test-unit-change-examples.R:1:testthat::test_that("Diamonds example loads with deterministic unit-change follow-up", {
tests/testthat/test-unit-change-examples.R:3:  testthat::expect_true("Diamonds" %in% examples)
tests/testthat/test-unit-change-examples.R:5:  info = loadExampleSpec("Diamonds", package = "WMFM")
tests/testthat/test-unit-change-examples.R:7:  testthat::expect_identical(info$spec$formula, "price ~ weight")
tests/testthat/test-unit-change-examples.R:8:  testthat::expect_match(info$followupQuestion, "0.1 carat", fixed = TRUE)
tests/testthat/test-unit-change-examples.R:17:testthat::test_that("Quakes unit-change example loads with deterministic follow-up", {
tests/testthat/test-unit-change-examples.R:19:  testthat::expect_true("Quakes-unit-change" %in% examples)
tests/testthat/test-unit-change-examples.R:21:  info = loadExampleSpec("Quakes-unit-change", package = "WMFM")
tests/testthat/test-unit-change-examples.R:23:  testthat::expect_identical(info$spec$formula, "Freq ~ Magnitude")
tests/testthat/test-unit-change-examples.R:24:  testthat::expect_match(info$followupQuestion, "0.1 magnitude", fixed = TRUE)
tests/testthat/test-unit-change-examples.R:45:  testthat::expect_identical(payload$unitChangeResult$predictorName, "Magnitude")
tests/testthat/test-glm-followup-predictions.R:2:  data(course.df, package = "s20x")
tests/testthat/test-glm-followup-predictions.R:3:  fit = stats::glm(Pass ~ Assign, data = course.df, family = stats::binomial())
tests/testthat/test-glm-followup-predictions.R:7:    followupQuestion = "A student scored 15 on Assign. What probability of passing would you predict?"
tests/testthat/test-glm-followup-predictions.R:18:  data(course.df, package = "s20x")
tests/testthat/test-glm-followup-predictions.R:19:  fit = stats::glm(Pass ~ Attend + Test, data = course.df, family = stats::binomial())
tests/testthat/test-glm-followup-predictions.R:23:    followupQuestion = "Suppose a student has Test = 10 and Attend = Yes. What probability of passing would you predict?"
tests/testthat/test-glm-followup-predictions.R:34:  data(course.df, package = "s20x")
tests/testthat/test-glm-followup-predictions.R:35:  fit = stats::glm(Pass ~ Assign, data = course.df, family = stats::binomial())
tests/testthat/test-glm-followup-predictions.R:51:testthat::test_that("Poisson earthquake follow-ups predict at in-range Magnitude values", {
tests/testthat/test-glm-followup-predictions.R:52:  quakePath = system.file(
tests/testthat/test-glm-followup-predictions.R:55:    "Quakes",
tests/testthat/test-glm-followup-predictions.R:56:    "Quakes.df.rda",
tests/testthat/test-glm-followup-predictions.R:59:  testthat::skip_if_not(file.exists(quakePath), "quake example data is unavailable")
tests/testthat/test-glm-followup-predictions.R:62:  objectNames = load(quakePath, envir = loadEnv)
tests/testthat/test-glm-followup-predictions.R:63:  quakeDf = loadEnv[[objectNames[[1]]]]
tests/testthat/test-glm-followup-predictions.R:65:  fitMagnitude = stats::glm(Freq ~ Magnitude, data = quakeDf, family = stats::poisson())
tests/testthat/test-glm-followup-predictions.R:66:  outMagnitude = computeGlmModelQuestionPrediction(
tests/testthat/test-glm-followup-predictions.R:67:    model = fitMagnitude,
tests/testthat/test-glm-followup-predictions.R:68:    followupQuestion = "What earthquake frequency would you expect for Magnitude = 5.4?"
tests/testthat/test-glm-followup-predictions.R:71:  testthat::expect_identical(outMagnitude$status, "ok")
tests/testthat/test-glm-followup-predictions.R:72:  testthat::expect_equal(outMagnitude$resolvedPredictorValues$Magnitude, 5.4)
tests/testthat/test-glm-followup-predictions.R:73:  testthat::expect_true(is.list(outMagnitude$confidenceInterval))
tests/testthat/test-glm-followup-predictions.R:74:  testthat::expect_true(is.list(outMagnitude$predictionIntervalPolicy))
tests/testthat/test-glm-followup-predictions.R:75:  testthat::expect_true(outMagnitude$predictionIntervalPolicy$supported)
tests/testthat/test-glm-followup-predictions.R:76:  testthat::expect_null(outMagnitude$predictionInterval)
tests/testthat/test-glm-followup-predictions.R:78:  fitMagnitudeLocn = stats::glm(Freq ~ Magnitude + Locn, data = quakeDf, family = stats::poisson())
tests/testthat/test-glm-followup-predictions.R:79:  outMagnitudeLocn = computeGlmModelQuestionPrediction(
tests/testthat/test-glm-followup-predictions.R:80:    model = fitMagnitudeLocn,
tests/testthat/test-glm-followup-predictions.R:81:    followupQuestion = "What earthquake frequency would you expect for Magnitude = 5.4 and Locn = WA?"
tests/testthat/test-glm-followup-predictions.R:84:  testthat::expect_identical(outMagnitudeLocn$status, "ok")
tests/testthat/test-glm-followup-predictions.R:85:  testthat::expect_equal(outMagnitudeLocn$resolvedPredictorValues$Magnitude, 5.4)
tests/testthat/test-glm-followup-predictions.R:86:  testthat::expect_identical(outMagnitudeLocn$resolvedPredictorValues$Locn, "WA")
tests/testthat/test-glm-followup-predictions.R:87:  testthat::expect_true(is.list(outMagnitudeLocn$confidenceInterval))
tests/testthat/test-glm-followup-predictions.R:88:  testthat::expect_true(is.list(outMagnitudeLocn$predictionIntervalPolicy))
tests/testthat/test-glm-followup-predictions.R:89:  testthat::expect_true(outMagnitudeLocn$predictionIntervalPolicy$supported)
tests/testthat/test-glm-followup-predictions.R:90:  testthat::expect_null(outMagnitudeLocn$predictionInterval)
tests/testthat/test-glm-followup-predictions.R:96:  quakePath = system.file(
tests/testthat/test-glm-followup-predictions.R:99:    "Quakes",
tests/testthat/test-glm-followup-predictions.R:100:    "Quakes.df.rda",
tests/testthat/test-glm-followup-predictions.R:103:  testthat::skip_if_not(file.exists(quakePath), "quake example data is unavailable")
tests/testthat/test-glm-followup-predictions.R:106:  objectNames = load(quakePath, envir = loadEnv)
tests/testthat/test-glm-followup-predictions.R:107:  quakeDf = loadEnv[[objectNames[[1]]]]
tests/testthat/test-glm-followup-predictions.R:109:  fitMagnitude = stats::glm(Freq ~ Magnitude, data = quakeDf, family = stats::poisson())
tests/testthat/test-glm-followup-predictions.R:111:    model = fitMagnitude,
tests/testthat/test-glm-followup-predictions.R:112:    followupQuestion = "What prediction interval would you give for Magnitude = 5.4?"
tests/testthat/test-glm-followup-predictions.R:128:  data(course.df, package = "s20x")
tests/testthat/test-glm-followup-predictions.R:129:  fit = stats::glm(Pass ~ Assign, data = course.df, family = stats::binomial())
tests/testthat/test-glm-followup-predictions.R:168:  data(course.df, package = "s20x")
tests/testthat/test-glm-followup-predictions.R:169:  fit = stats::glm(Pass ~ Assign, data = course.df, family = stats::binomial())
tests/testthat/test-glm-followup-predictions.R:182:  data(course.df, package = "s20x")
tests/testthat/test-glm-followup-predictions.R:183:  fit = stats::glm(Pass ~ Attend, data = course.df, family = stats::binomial())
tests/testthat/test-glm-followup-predictions.R:197:  quakePath = system.file(
tests/testthat/test-glm-followup-predictions.R:200:    "Quakes",
tests/testthat/test-glm-followup-predictions.R:201:    "Quakes.df.rda",
tests/testthat/test-glm-followup-predictions.R:204:  testthat::skip_if_not(file.exists(quakePath), "quake example data is unavailable")
tests/testthat/test-glm-followup-predictions.R:207:  objectNames = load(quakePath, envir = loadEnv)
tests/testthat/test-glm-followup-predictions.R:208:  quakeDf = loadEnv[[objectNames[[1]]]]
tests/testthat/test-glm-followup-predictions.R:209:  fit = stats::glm(Freq ~ Magnitude + Locn, data = quakeDf, family = stats::poisson())
tests/testthat/test-glm-followup-predictions.R:213:    followupQuestion = "How many earthquakes would you expect in Washington at magnitude 5.6?"
tests/testthat/test-glm-followup-predictions.R:217:  testthat::expect_equal(out$resolvedPredictorValues$Magnitude, 5.6)
tests/testthat/test-glm-followup-predictions.R:218:  testthat::expect_identical(out$resolvedPredictorValues$Locn, "WA")
tests/testthat/test-auditBadExplanationGrading.R:26:                metric = c("factualScore", "clarityScore"),
tests/testthat/test-auditBadExplanationGrading.R:27:                label = c("Factual score", "Clarity score"),
tests/testthat/test-auditBadExplanationGrading.R:57:  goodGrade = makeGrade(8.8, c("factualScore", "clarityScore"))
tests/testthat/test-auditBadExplanationGrading.R:101:                metric = c("factualScore", "clarityScore"),
tests/testthat/test-auditBadExplanationGrading.R:102:                label = c("Factual score", "Clarity score"),
tests/testthat/test-auditBadExplanationGrading.R:125:        logicalContradiction = makeGrade(8.7, "clarityScore")
tests/testthat/test-auditBadExplanationGrading.R:131:  goodGrade = makeGrade(8.8, c("factualScore", "clarityScore"))
tests/testthat/test-auditBadExplanationGrading.R:137:      logicalContradiction = c("factualScore", "clarityScore")
tests/testthat/test-app-developer-feedback-report.R:85:    Pass = factor(c(
tests/testthat/test-app-developer-feedback-report.R:86:      "Fail", "Fail", "Pass", "Fail",
tests/testthat/test-app-developer-feedback-report.R:87:      "Pass", "Pass", "Fail", "Pass"
tests/testthat/test-app-developer-feedback-report.R:91:  m = stats::glm(Pass ~ Assign, data = d, family = stats::binomial())
tests/testthat/test-app-developer-feedback-report.R:116:    Freq = c(10, 6, 2, 1),
tests/testthat/test-app-developer-feedback-report.R:117:    Magnitude = c(4, 5, 6, 7),
tests/testthat/test-app-developer-feedback-report.R:118:    Locn = factor(c("SC", "SC", "WA", "WA"))
tests/testthat/test-app-developer-feedback-report.R:120:  m = stats::glm(Freq ~ Magnitude + Locn, data = d, family = stats::poisson())
tests/testthat/test-app-developer-feedback-report.R:126:        "The expected count changes with Magnitude.",
tests/testthat/test-app-developer-feedback-report.R:142:  testthat::expect_equal(report$metadata$responseVariable, "Freq")
tests/testthat/test-app-developer-feedback-report.R:143:  testthat::expect_equal(report$metadata$predictors, c("Magnitude", "Locn"))
tests/testthat/test-validateWmfmParsedScores.R:21:  x$fieldReasons$clarityAdequate = NULL
tests/testthat/test-app-server-developer-mode-helpers.R:20:    buildDeveloperModeIncorrectPasswordMessage(),
tests/testthat/test-app-server-developer-mode-helpers.R:21:    "Incorrect password. Developer mode remains locked."
tests/testthat/test-course-followup-example.R:1:testthat::test_that("test Course Follow-Up example exists and loads follow-up question", {
tests/testthat/test-course-followup-example.R:7:  testthat::expect_false("test-Course Follow-Up" %in% visibleExamples)
tests/testthat/test-course-followup-example.R:8:  testthat::expect_true("test-Course Follow-Up" %in% developerExamples)
tests/testthat/test-course-followup-example.R:10:  info = loadExampleSpec("test-Course Follow-Up")
tests/testthat/test-course-followup-example.R:53:testthat::test_that("test Course Follow-Up never defaults regular attendance to not", {
tests/testthat/test-course-followup-example.R:97:testthat::test_that("deterministic follow-up failure text only asks for fitted-model predictors", {
tests/testthat/test-course-followup-example.R:104:  out = buildDeterministicFollowupFailureAnswer(prediction = prediction)
tests/testthat/test-equation-api.R:63:test_that("runModel falls back to deterministic equations when llm equations fail", {
tests/testthat/test-equation-api.R:72:        stop("llm equations failed", call. = FALSE)
tests/testthat/test-equation-api.R:107:    pass = factor(c("Fail", "Fail", "Pass", "Pass", "Pass", "Fail")),
tests/testthat/test-equation-api.R:111:    pass ~ x,
tests/testthat/test-explanation-prompt-diagnostics.R:45:testthat::test_that("list and load test Course Follow-Up example through display name", {
tests/testthat/test-explanation-prompt-diagnostics.R:47:  testthat::expect_true("test-Course Follow-Up" %in% examples)
tests/testthat/test-explanation-prompt-diagnostics.R:49:  info = loadExampleSpec("test-Course Follow-Up")
tests/testthat/test-explanation-prompt-diagnostics.R:51:  testthat::expect_identical(info$spec$displayName, "test-Course Follow-Up")
tests/testthat/test-buildModelConfidenceIntervalData.R:49:    Pass = factor(
tests/testthat/test-buildModelConfidenceIntervalData.R:66:  fit = glm(Pass ~ Attend * Score, data = d, family = binomial())
tests/testthat/test-buildModelConfidenceIntervalData.R:71:  testthat::expect_true(any(grepl("Pr(Pass = \"Yes\") when Attend = No", out$table$quantity, fixed = TRUE)))
tests/testthat/test-buildModelConfidenceIntervalData.R:72:  testthat::expect_true(any(grepl("Pr(Pass = \"No\") when Attend = No", out$table$quantity, fixed = TRUE)))
tests/testthat/test-buildModelConfidenceIntervalData.R:73:  testthat::expect_true(any(grepl("Odds(Pass = \"Yes\" vs Pass = \"No\") when Attend = No", out$table$quantity, fixed = TRUE)))
tests/testthat/test-buildModelConfidenceIntervalData.R:75:    "Odds(Pass = \"Yes\" vs Pass = \"No\") multiplier for a 1-unit increase in Score when Attend = Yes",
tests/testthat/test-buildModelConfidenceIntervalData.R:83:      identical(x$quantity, "Odds(Pass = \"Yes\" vs Pass = \"No\") multiplier for a 1-unit increase in Score when Attend = Yes")
tests/testthat/test-buildModelConfidenceIntervalData.R:96:    Freq = c(32, 27, 10, 9, 6, 6, 3, 2, 2, 13, 6, 2, 1, 0, 0, 1, 0, 0),
tests/testthat/test-buildModelConfidenceIntervalData.R:97:    Locn = factor(
tests/testthat/test-buildModelConfidenceIntervalData.R:101:    Magnitude = c(5.25, 5.50, 5.75, 6.00, 6.25, 6.50, 6.75, 7.00, 7.25,
tests/testthat/test-buildModelConfidenceIntervalData.R:105:  fit = glm(Freq ~ Locn * Magnitude, data = d, family = poisson())
tests/testthat/test-buildModelConfidenceIntervalData.R:111:  testthat::expect_true(any(grepl("Magnitude = 6.25", vapply(out$details, `[[`, character(1), "settings"), fixed = TRUE)))
tests/testthat/test-glm-teaching-notation.R:3:    pass = factor(c("Fail", "Fail", "Pass", "Pass", "Pass", "Fail")),
tests/testthat/test-glm-teaching-notation.R:7:  model = glm(pass ~ x, data = df, family = binomial(link = "logit"))
tests/testthat/test-glm-teaching-notation.R:11:  expect_identical(out$probabilitySuccess, "Pr(pass = \"Pass\")")
tests/testthat/test-glm-teaching-notation.R:12:  expect_identical(out$probabilityFailure, "Pr(pass = \"Fail\")")
tests/testthat/test-glm-teaching-notation.R:13:  expect_identical(out$oddsSuccess, "Odds(pass = \"Pass\" vs pass = \"Fail\")")
tests/testthat/test-glm-teaching-notation.R:14:  expect_identical(out$oddsFailure, "Odds(pass = \"Fail\" vs pass = \"Pass\")")
tests/testthat/test-glm-teaching-notation.R:15:  expect_identical(out$logitSuccess, "logit(Pr(pass = \"Pass\"))")
tests/testthat/test-glm-teaching-notation.R:21:    pass = factor(c("Fail", "Fail", "Pass", "Pass", "Pass", "Fail")),
tests/testthat/test-glm-teaching-notation.R:25:  model = glm(pass ~ x, data = df, family = binomial(link = "logit"))
tests/testthat/test-glm-teaching-notation.R:30:  expect_match(out, 'logit\\(Pr\\(pass = "Pass"\\)\\)')
tests/testthat/test-glm-teaching-notation.R:31:  expect_match(out, 'Odds\\(pass = "Pass" vs pass = "Fail"\\)')
tests/testthat/test-glm-teaching-notation.R:32:  expect_match(out, 'Pr\\(pass = "Pass"\\)')
tests/testthat/test-glm-teaching-notation.R:33:  expect_match(out, 'Odds\\(pass = "Fail" vs pass = "Pass"\\)')
tests/testthat/test-glm-teaching-notation.R:34:  expect_match(out, 'Pr\\(pass = "Fail"\\)')
tests/testthat/test-model-question-classifier.R:17:    "What earthquake frequency would you expect for Magnitude = 5.4?",
tests/testthat/test-model-question-classifier.R:18:    "What earthquake frequency would you expect for Magnitude = 5.4 and Locn = WA?",
tests/testthat/test-model-question-classifier.R:55:    "Explain the Carat effect for a 0.1-unit increase",
tests/testthat/test-prompt-explain-research-question.R:86:  testthat::expect_match(prompt, "Do not infer course level", fixed = TRUE)
tests/testthat/test-prompt-explain-research-question.R:95:  attr(fit, "wmfm_research_question") = "What is the average final exam mark in the course data?"
tests/testthat/test-prompt-explain-research-question.R:101:  testthat::expect_match(prompt, "What is the average final exam mark for this course?", fixed = TRUE)
tests/testthat/test-prompt-explain-research-question.R:102:  testthat::expect_no_match(prompt, "What is the average final exam mark in the course data?", fixed = TRUE)
tests/testthat/test-prompt-explain-research-question.R:103:  testthat::expect_match(prompt, "do not describe it as only the average in this data set, dataset, course data, or sample", fixed = TRUE)
tests/testthat/helper-fake-wmfmScores.R:59:    clarityAdequate = c(
tests/testthat/test-stability.R:161:        scores$scores$llm[[i]]$clarityAdequate = NA_real_
tests/testthat/test-stability.R:165:    clarityRows = result$ordinalStability[
tests/testthat/test-stability.R:166:        result$ordinalStability$metric == "clarityAdequate",
tests/testthat/test-stability.R:171:    expect_equal(nrow(clarityRows), 1)
tests/testthat/test-stability.R:172:    expect_equal(clarityRows$method, "deterministic")
tests/testthat/test-app-equation-runtime.R:126:test_that("buildAppModelOutputs falls back to deterministic equations if llm equations fail", {
tests/testthat/test-app-equation-runtime.R:140:        stop("equation request failed")
tests/testthat/test-buildExplanationClaimEvidenceMap.R:101:  data(quakes, package = "datasets")
tests/testthat/test-buildExplanationClaimEvidenceMap.R:103:  df = datasets::quakes[, c("mag", "stations")]
tests/testthat/test-buildExplanationClaimEvidenceMap.R:164:  data(quakes, package = "datasets")
tests/testthat/test-buildExplanationClaimEvidenceMap.R:166:  df = datasets::quakes[, c("mag", "stations")]
tests/testthat/test-buildExplanationClaimEvidenceMap.R:167:  df$locn = factor(ifelse(seq_len(nrow(df)) %% 2 == 0, "SC", "WA"))
tests/testthat/test-buildExplanationClaimEvidenceMap.R:169:  model = stats::glm(stations ~ mag * locn, family = poisson(link = "log"), data = df)
tests/testthat/test-buildExplanationClaimEvidenceMap.R:171:    "The study asks how the expected number of earthquakes changes when magnitude grows,",
tests/testthat/test-buildExplanationClaimEvidenceMap.R:180:      "A one-magnitude rise multiplies the expected count in SC by about 0.21; the 95% confidence limits run from roughly 0.13 to 0.31.",
tests/testthat/test-buildExplanationClaimEvidenceMap.R:181:      "In WA the same one-magnitude rise multiplies the expected count by about 0.04, with confidence limits from roughly 0.04 to 0.72.",
tests/testthat/test-buildExplanationClaimEvidenceMap.R:182:      "Answer: On average, earthquake frequency falls as magnitude increases, with a steeper decline in WA."
tests/testthat/test-buildExplanationClaimEvidenceMap.R:197:    "On average, earthquake frequency falls as magnitude increases, with a steeper decline in WA."
tests/testthat/test-model-question-prediction-glm.R:31:testthat::test_that("GLM prediction with missing covariates can fail safely when completion is disabled", {
tests/testthat/test-app-explanation-diagnostics-ui.R:139:    followupText = "Explain the Carat effect for a 0.1-unit increase",
tests/testthat/test-app-explanation-diagnostics-ui.R:141:      originalText = "Explain the Carat effect for a 0.1-unit increase",
tests/testthat/test-app-explanation-diagnostics-ui.R:146:        predictorName = "Carat",
tests/testthat/test-app-explanation-diagnostics-ui.R:168:    followupText = "Explain the Magnitude effect for a 0.1 magnitude increase",
tests/testthat/test-app-explanation-diagnostics-ui.R:170:      originalText = "Explain the Magnitude effect for a 0.1 magnitude increase",
tests/testthat/test-app-explanation-diagnostics-ui.R:178:        responseName = "Freq",
tests/testthat/test-app-explanation-diagnostics-ui.R:179:        predictorName = "Magnitude",
tests/testthat/test-app-explanation-diagnostics-ui.R:209:  testthat::expect_identical(parsed$requestedPredictor, "Magnitude")
tests/testthat/test-app-explanation-diagnostics-ui.R:220:    followupText = "Explain the Carat effect for a 0.1 carat increase",
tests/testthat/test-app-explanation-diagnostics-ui.R:222:      originalText = "Explain the Carat effect for a 0.1 carat increase",
tests/testthat/test-app-explanation-diagnostics-ui.R:228:        predictorName = "Carat",
tests/testthat/test-generate-bad-explanation.R:130:test_that("generated bad explanations can be passed directly to grade", {
tests/testthat/test-app-server-chat-provider-observers.R:10:  expect_false(grepl("verifyProviderSwitchPassword", chatProviderText, fixed = TRUE))
tests/testthat/test-app-server-chat-provider-observers.R:12:  expect_false(grepl("buildClaudeProviderIncorrectPasswordMessage", chatProviderText, fixed = TRUE))
tests/testthat/test-app-server-chat-provider-observers.R:20:test_that("provider config save no longer depends on Claude password verification", {
tests/testthat/test-app-server-chat-provider-observers.R:37:  expect_false(grepl("verifyProviderSwitchPassword", saveBlock, fixed = TRUE))
tests/testthat/test-app-server-chat-provider-observers.R:38:  expect_false(grepl("providerSwitchPassword", saveBlock, fixed = TRUE))
tests/testthat/test-app-server-chat-provider-observers.R:41:test_that("Ollama model refresh is capability-aware and keeps failure fallback", {
tests/testthat/test-diagnose-all-metrics.R:8:            clarityAdequate = 1
tests/testthat/test-diagnose-all-metrics.R:12:            clarityAdequate = 1
tests/testthat/test-diagnose-all-metrics.R:16:            clarityAdequate = 2
tests/testthat/test-diagnose-all-metrics.R:22:            clarityAdequate = 1
tests/testthat/test-diagnose-all-metrics.R:26:            clarityAdequate = 2
tests/testthat/test-diagnose-all-metrics.R:30:            clarityAdequate = 2
tests/testthat/test-diagnose-all-metrics.R:52:  expect_true(all(c("numericExpressionAdequate", "clarityAdequate") %in% names(dxAll$metricDiagnoses)))
tests/testthat/test-diagnose-all-metrics.R:62:            clarityAdequate = 1
tests/testthat/test-diagnose-all-metrics.R:66:            clarityAdequate = 1
tests/testthat/test-diagnose-all-metrics.R:70:            clarityAdequate = 2
tests/testthat/test-diagnose-all-metrics.R:76:            clarityAdequate = 1
tests/testthat/test-diagnose-all-metrics.R:80:            clarityAdequate = 2
tests/testthat/test-diagnose-all-metrics.R:84:            clarityAdequate = 2
tests/testthat/test-score.wmfmRuns.R:14:    clarityAdequate = c(2L, 2L, 2L),
tests/testthat/test-score.wmfmRuns.R:21:    clarityScore = c(1.8, 1.7, 1.6),
tests/testthat/test-score.wmfmRuns.R:24:    overallPass = c(TRUE, TRUE, TRUE),
tests/testthat/test-score.wmfmRuns.R:63:      clarityAdequate = 2L,
tests/testthat/test-score.wmfmRuns.R:70:      clarityScore = 1.9,
tests/testthat/test-score.wmfmRuns.R:73:      overallPass = TRUE,
tests/testthat/test-prompt-response-scale-control.R:13:  courseDf = utils::read.table(
tests/testthat/test-prompt-response-scale-control.R:20:  fit = stats::glm(Pass ~ Assign, family = stats::binomial(), data = courseDf)
tests/testthat/test-explanation-structural-answer-selection.R:30:  data(quakes, package = "datasets")
tests/testthat/test-explanation-structural-answer-selection.R:32:  df = datasets::quakes[, c("mag", "stations")]
tests/testthat/test-explanation-structural-answer-selection.R:33:  df$locn = factor(ifelse(seq_len(nrow(df)) %% 2 == 0, "SC", "WA"))
tests/testthat/test-explanation-structural-answer-selection.R:35:  model = stats::glm(stations ~ mag * locn, family = poisson(link = "log"), data = df)
tests/testthat/test-explanation-structural-answer-selection.R:37:    "The study asks how the expected number of earthquakes changes when magnitude grows,",
tests/testthat/test-explanation-structural-answer-selection.R:46:      "These figures indicate that higher magnitudes are associated with fewer earthquakes in both regions, with a stronger decrease in WA than in SC.",
tests/testthat/test-explanation-structural-answer-selection.R:47:      "In short, on average earthquake frequency falls sharply as magnitude increases in both locations, with WA showing an even steeper decrease than SC, according to the fitted model and its uncertainty.",
tests/testthat/test-explanation-structural-answer-selection.R:48:      "This pattern applies on average and does not predict individual earthquakes."
tests/testthat/test-explanation-structural-answer-selection.R:86:  data(course.df, package = "s20x")
tests/testthat/test-explanation-structural-answer-selection.R:87:  df = course.df[, c("Exam", "Attend", "Test")]
tests/testthat/test-plot.wmfmScores.R:4:  x$clarityScore = c(1.5, 1.6, 1.7, 1.4, 1.5, 1.6)
tests/testthat/test-model-question-prediction-lm.R:100:testthat::test_that("unsupported model types fail safely", {
tests/testthat/test-model-question-prediction-lm.R:144:testthat::test_that("non-numeric values for numeric predictors fail safely", {
tests/testthat/test-model-question-prediction-lm.R:153:testthat::test_that("unsupported predictor types fail safely", {
tests/testthat/test-model-question-prediction-lm.R:163:testthat::test_that("unsupported GLM prediction interval requests fail safely", {
tests/testthat/test-model-question-prediction-lm.R:182:testthat::test_that("unsupported separator x:5 fails safely as missing predictor values", {
tests/testthat/test-model-question-prediction-lm.R:191:testthat::test_that("Course Follow-Up resolves Attend regular from model factor levels", {
tests/testthat/test-model-question-prediction-lm.R:229:testthat::test_that("ambiguous factor wording fails safely", {
tests/testthat/test-model-question-prediction-lm.R:323:testthat::test_that("Course Follow-Up resolves regular attendance for Yes/No factors", {
tests/testthat/test-model-question-prediction-lm.R:343:    predictor = "log(carat)",
tests/testthat/test-model-question-prediction-lm.R:344:    text = "Predict log price when log(carat) = 0.5"
tests/testthat/test-model-question-prediction-lm.R:349:    predictor = "log(carat)",
tests/testthat/test-model-question-prediction-lm.R:350:    text = "Predict log price for 0.5 on log(carat)"
tests/testthat/test-model-question-prediction-lm.R:355:    predictor = "log(carat)",
tests/testthat/test-model-question-prediction-lm.R:356:    text = "Predict log price for 0.5 on log(carat)."
tests/testthat/test-explanation-final-answer-tagging.R:25:  data(quakes, package = "datasets")
tests/testthat/test-explanation-final-answer-tagging.R:27:  df = datasets::quakes[, c("mag", "stations")]
tests/testthat/test-explanation-final-answer-tagging.R:28:  df$locn = factor(ifelse(seq_len(nrow(df)) %% 2 == 0, "SC", "WA"))
tests/testthat/test-explanation-final-answer-tagging.R:30:  model = stats::glm(stations ~ mag * locn, family = poisson(link = "log"), data = df)
tests/testthat/test-explanation-final-answer-tagging.R:32:    "The study asks how the expected number of earthquakes changes when magnitude grows,",
tests/testthat/test-explanation-final-answer-tagging.R:40:    claimText = "On average, earthquake frequency falls as magnitude increases, with a steeper decline in WA than in SC.",
tests/testthat/test-explanation-final-answer-tagging.R:102:  data(quakes, package = "datasets")
tests/testthat/test-explanation-final-answer-tagging.R:104:  df = datasets::quakes[, c("mag", "stations")]
tests/testthat/test-explanation-final-answer-tagging.R:105:  df$locn = factor(ifelse(seq_len(nrow(df)) %% 2 == 0, "SC", "WA"))
tests/testthat/test-explanation-final-answer-tagging.R:107:  model = stats::glm(stations ~ mag * locn, family = poisson(link = "log"), data = df)
tests/testthat/test-explanation-final-answer-tagging.R:109:    "The study asks how the expected number of earthquakes changes when magnitude grows,",
tests/testthat/test-explanation-final-answer-tagging.R:118:      "In SC, the expected count falls as magnitude increases.",
tests/testthat/test-explanation-final-answer-tagging.R:119:      "On average, earthquake frequency falls as magnitude increases, with a steeper decline in WA than in SC."
tests/testthat/test-prompt-anchored-baseline-values.R:3:    Locn = factor(c(rep("SC", 9), rep("WA", 9))),
tests/testthat/test-prompt-anchored-baseline-values.R:4:    Magnitude = c(
tests/testthat/test-prompt-anchored-baseline-values.R:8:    Freq = c(32, 27, 10, 9, 6, 6, 3, 2, 2, 13, 6, 2, 1, 0, 0, 1, 0, 0)
tests/testthat/test-prompt-anchored-baseline-values.R:11:  fit = stats::glm(Freq ~ Locn * Magnitude, family = "poisson", data = dat)
tests/testthat/test-prompt-anchored-baseline-values.R:19:  expect_match(block, "settings: Locn = SC; Magnitude = 6.25.", fixed = TRUE)
tests/testthat/test-prompt-anchored-baseline-values.R:20:  expect_match(block, "settings: Locn = WA; Magnitude = 6.25.", fixed = TRUE)
tests/testthat/test-prompt-anchored-baseline-values.R:25:    Locn = factor(c(rep("SC", 9), rep("WA", 9))),
tests/testthat/test-prompt-anchored-baseline-values.R:26:    Magnitude = c(
tests/testthat/test-prompt-anchored-baseline-values.R:30:    Freq = c(32, 27, 10, 9, 6, 6, 3, 2, 2, 13, 6, 2, 1, 0, 0, 1, 0, 0)
tests/testthat/test-prompt-anchored-baseline-values.R:33:  fit = stats::glm(Freq ~ Locn * Magnitude, family = "poisson", data = dat)
tests/testthat/test-prompt-anchored-baseline-values.R:56:  courseDf = utils::read.table(
tests/testthat/test-prompt-anchored-baseline-values.R:63:  fit = stats::glm(Pass ~ Assign, family = stats::binomial(), data = courseDf)
tests/testthat/helper-fake-wmfmRuns.R:9:    exampleName = "Course",
tests/testthat/helper-fake-wmfmRuns.R:47:    exampleName = "Course",
tests/testthat/test-getMetricComparisonData.R:33:    result = getMetricComparisonData(comparison, metric = "clarityScore")
tests/testthat/test-getMetricComparisonData.R:48:    result = getMetricComparisonData(comparison, metric = "clarityScore")
tests/testthat/test-equation-spec.R:71:    pass = factor(c("Fail", "Fail", "Pass", "Pass", "Fail", "Pass")),
tests/testthat/test-equation-spec.R:75:  model = glm(pass ~ x, data = df, family = binomial(link = "logit"))
tests/testthat/test-equation-spec.R:80:  expect_identical(out$notation$probabilitySuccess, 'Pr(pass = "Pass")')
tests/testthat/test-equation-spec.R:81:  expect_identical(out$notation$oddsSuccess, 'Odds(pass = "Pass" vs pass = "Fail")')
tests/testthat/test-prompt-comparison-control.R:45:  courseDf = utils::read.table(
tests/testthat/test-prompt-comparison-control.R:52:  fit = stats::glm(Pass ~ Attend, family = stats::binomial(), data = courseDf)
tests/testthat/test-prompt-comparison-control.R:53:  attr(fit, "wmfm_research_question") = "Do students who regularly attended class appear to have different odds of passing?"
tests/testthat/test-app-explanation-claim-evidence-ui.R:102:testthat::test_that("claim evidence map passes developer mode to every card", {
tests/testthat/helper-valid-parsed-scores.R:12:    clarityAdequate = 2,
tests/testthat/helper-valid-parsed-scores.R:19:    clarityScore = 1.9,
tests/testthat/helper-valid-parsed-scores.R:22:    overallPass = TRUE,
tests/testthat/helper-valid-parsed-scores.R:34:      clarityAdequate = "ok",
tests/testthat/test-scoreWmfmRunsWithLlm.R:13:    '"clarityAdequate":2,',
tests/testthat/test-scoreWmfmRunsWithLlm.R:20:    '"clarityScore":1.9,',
tests/testthat/test-scoreWmfmRunsWithLlm.R:23:    '"overallPass":true,',
tests/testthat/test-scoreWmfmRunsWithLlm.R:35:    '"clarityAdequate":"ok",',
tests/testthat/test-explanation-validation.R:15:    claimText = "The odds of passing are about 0.70.",
tests/testthat/test-explanation-validation.R:22:    claimText = "The odds of passing are about 0.70:1.",
tests/testthat/test-explanation-validation.R:40:    claimText = "The odds of passing are about 0.70 (95% CI 0.4 to 1.2).",
tests/testthat/test-explanation-validation.R:49:    claimText = "The effect of magnitude is negative.",
tests/testthat/test-explanation-validation.R:56:    claimText = "The effect of a one-unit increase in magnitude is negative.",
tests/testthat/test-glm-followup-odds-predictions.R:2:  data(course.df, package = "s20x")
tests/testthat/test-glm-followup-odds-predictions.R:3:  fit = stats::glm(Pass ~ Assign, data = course.df, family = stats::binomial())
tests/testthat/test-glm-followup-odds-predictions.R:7:    followupQuestion = "A student scored 15 on Assign. What odds of passing would you predict?"
tests/testthat/test-glm-followup-odds-predictions.R:26:  data(course.df, package = "s20x")
tests/testthat/test-glm-followup-odds-predictions.R:27:  fit = stats::glm(Pass ~ Attend + Test, data = course.df, family = stats::binomial())
tests/testthat/test-glm-followup-odds-predictions.R:31:    followupQuestion = "Suppose a student has Test = 10 and Attend = Yes. What odds of passing would you predict?"
tests/testthat/test-glm-followup-odds-predictions.R:35:    Attend = factor("Yes", levels = levels(course.df$Attend)),
tests/testthat/test-glm-followup-odds-predictions.R:67:  data(course.df, package = "s20x")
tests/testthat/test-glm-followup-odds-predictions.R:68:  fit = stats::glm(Pass ~ Test, data = course.df, family = stats::binomial())
tests/testthat/test-glm-followup-odds-predictions.R:72:    followupQuestion = "What are the odds of passing for someone with a test mark of 10?"
tests/testthat/test-completeConfidenceIntervalNewData.R:4:    Freq = c(32, 27, 10, 9, 6, 6, 3, 2, 2, 13, 6, 2, 1, 0, 0, 1, 0, 0),
tests/testthat/test-completeConfidenceIntervalNewData.R:5:    Locn = factor(
tests/testthat/test-completeConfidenceIntervalNewData.R:9:    Magnitude = c(
tests/testthat/test-completeConfidenceIntervalNewData.R:15:  fit = glm(Freq ~ Locn * Magnitude, data = d, family = poisson())
tests/testthat/test-completeConfidenceIntervalNewData.R:19:    newData = data.frame(Locn = factor("WA", levels = c("SC", "WA"))),
tests/testthat/test-completeConfidenceIntervalNewData.R:23:  testthat::expect_identical(names(out), c("Locn", "Magnitude"))
tests/testthat/test-completeConfidenceIntervalNewData.R:24:  testthat::expect_equal(out$Magnitude[1], mean(d$Magnitude))
tests/testthat/test-diagnose-evidence-logic.R:141:        metric = "clarityAdequate"
tests/testthat/test-buildWmfmRunRecord.R:4:    exampleName = "Course",
tests/testthat/test-buildWmfmRunRecord.R:19:  testthat::expect_identical(out$exampleName, "Course")
tests/testthat/test-buildWmfmRunRecord.R:31:    "overallPass",
tests/testthat/test-buildWmfmRunRecord.R:39:    exampleName = "Course",
tests/testthat/test-buildExplanationRuleProfile.R:78:    predictorTypes = list(numeric = "magnitude", factor = "locn", other = character(0)),
tests/testthat/test-model-plots.R:73:testthat::test_that("unsupported model plot choices do not make plotting helpers fail", {
tests/testthat/test-print-wmfmScoresDiagnosis.R:8:            clarityAdequate = 1
tests/testthat/test-print-wmfmScoresDiagnosis.R:12:            clarityAdequate = 1
tests/testthat/test-print-wmfmScoresDiagnosis.R:18:            clarityAdequate = 1
tests/testthat/test-print-wmfmScoresDiagnosis.R:22:            clarityAdequate = 2
tests/testthat/helper-wmfm-test-fixtures.R:37:                        overallPass = TRUE
tests/testthat/helper-wmfm-test-fixtures.R:43:                        overallPass = TRUE
tests/testthat/helper-wmfm-test-fixtures.R:61:                        overallPass = TRUE,
tests/testthat/helper-wmfm-test-fixtures.R:63:                        clarityAdequate = 2,
tests/testthat/helper-wmfm-test-fixtures.R:71:                        overallPass = TRUE,
tests/testthat/helper-wmfm-test-fixtures.R:73:                        clarityAdequate = 1,
tests/testthat/helper-wmfm-test-fixtures.R:81:                        overallPass = FALSE,
tests/testthat/helper-wmfm-test-fixtures.R:83:                        clarityAdequate = 2,
tests/testthat/helper-wmfm-test-fixtures.R:93:                        overallPass = TRUE,
tests/testthat/helper-wmfm-test-fixtures.R:95:                        clarityAdequate = 2,
tests/testthat/helper-wmfm-test-fixtures.R:103:                        overallPass = TRUE,
tests/testthat/helper-wmfm-test-fixtures.R:105:                        clarityAdequate = 2,
tests/testthat/helper-wmfm-test-fixtures.R:113:                        overallPass = TRUE,
tests/testthat/helper-wmfm-test-fixtures.R:115:                        clarityAdequate = 2,
tests/testthat/test-describeField.R:53:        describeField(runs, field = "overallPass", format = "list"),
tests/testthat/test-prompt-explanation-skeleton.R:20:    Locn = factor(c(rep("SC", 9), rep("WA", 9))),
tests/testthat/test-prompt-explanation-skeleton.R:21:    Magnitude = c(
tests/testthat/test-prompt-explanation-skeleton.R:25:    Freq = c(32, 27, 10, 9, 6, 6, 3, 2, 2, 13, 6, 2, 1, 0, 0, 1, 0, 0)
tests/testthat/test-prompt-explanation-skeleton.R:28:  fit = stats::glm(Freq ~ Locn * Magnitude, family = "poisson", data = dat)
tests/testthat/test-provider-config.R:215:test_that("unsupported provider adapter lookup fails clearly", {
tests/testthat/helper-metric-comparison-fixtures.R:6:                clarityScore = 2,
tests/testthat/helper-metric-comparison-fixtures.R:14:                clarityScore = 1,
tests/testthat/helper-metric-comparison-fixtures.R:22:                clarityScore = 1,
tests/testthat/helper-metric-comparison-fixtures.R:32:                clarityScore = 2,
tests/testthat/helper-metric-comparison-fixtures.R:40:                clarityScore = 2,
tests/testthat/helper-metric-comparison-fixtures.R:48:                clarityScore = 1,
tests/testthat/helper-metric-comparison-fixtures.R:82:            rep("clarityScore", 3),
tests/testthat/helper-metric-comparison-fixtures.R:99:        metricName = c("factualScore", "clarityScore", "overallScore"),
tests/testthat/helper-metric-comparison-fixtures.R:100:        label = c("Factual score", "Clarity score", "Overall score"),
tests/testthat/helper-metric-comparison-fixtures.R:108:        metric = c("factualScore", "clarityScore"),
tests/testthat/test-log-log-models.R:2:  data = ggplot2::diamonds[seq_len(200), ]
tests/testthat/test-log-log-models.R:3:  model = stats::lm(log(price) ~ log(carat), data = data)
tests/testthat/test-log-log-models.R:8:  testthat::expect_identical(out$responseVariable, "price")
tests/testthat/test-log-log-models.R:10:  testthat::expect_identical(out$logPredictors$originalName[[1]], "carat")
tests/testthat/test-log-log-models.R:14:  data = ggplot2::diamonds[seq_len(200), ]
tests/testthat/test-log-log-models.R:15:  model = stats::lm(log(price) ~ log(carat), data = data)
tests/testthat/test-log-log-models.R:19:  testthat::expect_identical(out$responseVariable, "price")
tests/testthat/test-log-log-models.R:22:  testthat::expect_identical(out$predictorTypes$numeric, "log(carat)")
tests/testthat/test-log-log-models.R:27:  data = ggplot2::diamonds[seq_len(200), ]
tests/testthat/test-log-log-models.R:28:  model = stats::lm(log(price) ~ log(carat), data = data)
tests/testthat/test-log-log-models.R:32:  testthat::expect_true(any(grepl("log\\(price\\)", out$linearPredictor)))
tests/testthat/test-log-log-models.R:33:  testthat::expect_true(any(grepl("price = ", out$responseScale, fixed = TRUE)))
tests/testthat/test-log-log-models.R:34:  testthat::expect_true(any(grepl("carat\\^", out$responseScale)))
tests/testthat/test-log-log-models.R:38:  data = ggplot2::diamonds[seq_len(600), ]
tests/testthat/test-log-log-models.R:39:  model = stats::lm(log(price) ~ log(carat) + cut + color + clarity, data = data)
tests/testthat/test-log-log-models.R:43:  testthat::expect_true(any(grepl("price = ", out$responseScale, fixed = TRUE)))
tests/testthat/test-log-log-models.R:44:  testthat::expect_true(any(grepl("carat\\^", out$responseScale)))
tests/testthat/test-log-log-models.R:45:  testthat::expect_true(any(grepl("cut", out$condition, fixed = TRUE)))
tests/testthat/test-log-log-models.R:46:  testthat::expect_true(any(grepl("color", out$condition, fixed = TRUE)))
tests/testthat/test-log-log-models.R:47:  testthat::expect_true(any(grepl("clarity", out$condition, fixed = TRUE)))
tests/testthat/test-log-log-models.R:50:testthat::test_that("Diamonds II to IV examples load with log-log formulas", {
tests/testthat/test-log-log-models.R:53:  testthat::expect_true("Diamonds II" %in% examples)
tests/testthat/test-log-log-models.R:54:  testthat::expect_true("Diamonds III" %in% examples)
tests/testthat/test-log-log-models.R:55:  testthat::expect_true("Diamonds IV" %in% examples)
tests/testthat/test-log-log-models.R:57:  diamondsII = loadExampleSpec("Diamonds II", package = "WMFM")
tests/testthat/test-log-log-models.R:58:  diamondsIII = loadExampleSpec("Diamonds III", package = "WMFM")
tests/testthat/test-log-log-models.R:59:  diamondsIV = loadExampleSpec("Diamonds IV", package = "WMFM")
tests/testthat/test-log-log-models.R:61:  testthat::expect_identical(diamondsII$spec$formula, "log(price) ~ log(carat)")
tests/testthat/test-log-log-models.R:62:  testthat::expect_identical(diamondsIII$spec$formula, "log(price) ~ log(carat)")
tests/testthat/test-log-log-models.R:64:    diamondsIV$spec$formula,
tests/testthat/test-log-log-models.R:65:    "log(price) ~ log(carat) + cut + color + clarity"
tests/testthat/test-log-log-models.R:67:  testthat::expect_match(diamondsIII$followupQuestion, "0.1 carat", fixed = TRUE)
tests/testthat/test-log-log-models.R:68:  testthat::expect_match(diamondsIV$followupQuestion, "adjusting for cut, color, and clarity", fixed = TRUE)
tests/testthat/test-log-log-models.R:69:  testthat::expect_null(diamondsII$followupQuestion)
tests/testthat/test-log-log-models.R:70:  testthat::expect_false(grepl("10-unit", diamondsIII$followupQuestion, fixed = TRUE))
tests/testthat/test-log-log-models.R:71:  testthat::expect_false(grepl("10 unit", diamondsIII$followupQuestion, fixed = TRUE))
tests/testthat/test-log-log-models.R:73:  testthat::expect_false(is.ordered(diamondsII$data$cut))
tests/testthat/test-log-log-models.R:74:  testthat::expect_false(is.ordered(diamondsII$data$color))
tests/testthat/test-log-log-models.R:75:  testthat::expect_false(is.ordered(diamondsII$data$clarity))
tests/testthat/test-log-log-models.R:76:  testthat::expect_true(is.factor(diamondsIV$data$cut))
tests/testthat/test-log-log-models.R:77:  testthat::expect_true(is.factor(diamondsIV$data$color))
tests/testthat/test-log-log-models.R:78:  testthat::expect_true(is.factor(diamondsIV$data$clarity))
tests/testthat/test-log-log-models.R:80:    diamondsIV$spec$adjustmentVariables,
tests/testthat/test-log-log-models.R:81:    c("cut", "color", "clarity")
tests/testthat/test-log-log-models.R:86:  data = ggplot2::diamonds[seq_len(300), ]
tests/testthat/test-log-log-models.R:87:  model = stats::lm(log(price) ~ log(carat), data = data)
tests/testthat/test-log-log-models.R:91:    followupQuestion = "Can you express the weight effect for a 0.1 carat increase?",
tests/testthat/test-log-log-models.R:98:  testthat::expect_identical(result$predictorName, "carat")
tests/testthat/test-log-log-models.R:99:  testthat::expect_identical(result$transformedPredictorName, "log(carat)")
tests/testthat/test-log-log-models.R:104:  testthat::expect_match(result$interpretation, "Starting from a typical carat value", fixed = TRUE)
tests/testthat/test-log-log-models.R:106:  testthat::expect_match(result$interpretation, "0.1-carat increase", fixed = TRUE)
tests/testthat/test-log-log-models.R:107:  testthat::expect_match(result$interpretation, "a fitted price", fixed = TRUE)
tests/testthat/test-log-log-models.R:108:  testthat::expect_false(grepl("log(price)", result$interpretation, fixed = TRUE))
tests/testthat/test-log-log-models.R:112:  data = ggplot2::diamonds[seq_len(300), ]
tests/testthat/test-log-log-models.R:113:  model = stats::lm(log(price) ~ log(carat), data = data)
tests/testthat/test-log-log-models.R:115:    "Can you express the weight effect for a 0.1 carat increase?"
tests/testthat/test-log-log-models.R:130:  data = ggplot2::diamonds[seq_len(300), ]
tests/testthat/test-log-log-models.R:131:  model = stats::lm(log(price) ~ log(carat), data = data)
tests/testthat/test-log-log-models.R:134:    "Can you express the weight effect for a 10% increase in carat?"
tests/testthat/test-log-log-models.R:143:  testthat::expect_identical(result$predictorName, "carat")
tests/testthat/test-log-log-models.R:147:  testthat::expect_match(result$interpretation, "10% increase in carat", fixed = TRUE)
tests/testthat/test-log-log-models.R:151:  data = ggplot2::diamonds[seq_len(300), ]
tests/testthat/test-log-log-models.R:152:  model = stats::lm(log(price) ~ log(carat), data = data)
tests/testthat/test-log-log-models.R:155:    "What happens to price if carat doubles?"
tests/testthat/test-log-log-models.R:164:  testthat::expect_match(result$interpretation, "100% increase in carat", fixed = TRUE)
tests/testthat/test-log-log-models.R:168:  data = ggplot2::diamonds[seq_len(300), ]
tests/testthat/test-log-log-models.R:169:  model = stats::lm(log(price) ~ log(carat), data = data)
tests/testthat/test-log-log-models.R:171:    "Can you explain the effect as a 10% increase in carat?"
tests/testthat/test-log-log-models.R:185:  data = ggplot2::diamonds[seq_len(600), ]
tests/testthat/test-log-log-models.R:186:  model = stats::lm(log(price) ~ log(carat) + cut + color + clarity, data = data)
tests/testthat/test-log-log-models.R:189:    "Does adjusting for cut, color, and clarity improve our predictions substantially?"
tests/testthat/test-log-log-models.R:198:  testthat::expect_true(all(c("cut", "color", "clarity") %in% result$adjustmentTerms))
tests/testthat/test-log-log-models.R:215:  testthat::expect_match(result$studentFacingConclusion, "Accounting for cut, color, and clarity", fixed = TRUE)
tests/testthat/test-log-log-models.R:221:  data = ggplot2::diamonds[seq_len(600), ]
tests/testthat/test-log-log-models.R:222:  model = stats::lm(log(price) ~ log(carat) + cut + color + clarity, data = data)
tests/testthat/test-log-log-models.R:225:    "Does adjusting for cut, color, and clarity improve our predictions substantially?"
tests/testthat/test-log-log-models.R:248:  data = ggplot2::diamonds[seq_len(600), ]
tests/testthat/test-log-log-models.R:249:  model = stats::lm(log(price) ~ log(carat) + cut + color + clarity, data = data)
tests/testthat/test-log-log-models.R:252:    "Does adjusting for cut, color, and clarity improve our predictions substantially?"
tests/testthat/test-log-log-models.R:258:    "Diamond prices increase with weight.",
tests/testthat/test-log-log-models.R:259:    "After adjusting for cut, color, and clarity, heavier diamonds have higher prices."
tests/testthat/test-log-log-models.R:263:  testthat::expect_match(out, "Diamond prices increase with weight", fixed = TRUE)
tests/testthat/test-log-log-models.R:264:  testthat::expect_match(out, "Accounting for cut, color, and clarity", fixed = TRUE)
tests/testthat/test-log-log-models.R:275:  data = ggplot2::diamonds[seq_len(200), ]
tests/testthat/test-log-log-models.R:276:  model = stats::lm(log(price) ~ log(carat), data = data)
tests/testthat/test-log-log-models.R:294:testthat::test_that("Diamonds log-log examples keep bounded teaching questions", {
tests/testthat/test-log-log-models.R:295:  diamondsII = loadExampleSpec("Diamonds II", package = "WMFM")
tests/testthat/test-log-log-models.R:296:  diamondsIII = loadExampleSpec("Diamonds III", package = "WMFM")
tests/testthat/test-log-log-models.R:297:  diamondsIV = loadExampleSpec("Diamonds IV", package = "WMFM")
tests/testthat/test-log-log-models.R:300:    diamondsII$researchQuestion,
tests/testthat/test-log-log-models.R:301:    diamondsIII$researchQuestion,
tests/testthat/test-log-log-models.R:302:    diamondsIII$followupQuestion,
tests/testthat/test-log-log-models.R:303:    diamondsIV$researchQuestion,
tests/testthat/test-log-log-models.R:304:    diamondsIV$followupQuestion,
tests/testthat/test-log-log-models.R:308:  testthat::expect_match(exampleText, "predict the price of diamonds", fixed = TRUE)
tests/testthat/test-log-log-models.R:309:  testthat::expect_match(exampleText, "0.1 carat", fixed = TRUE)
tests/testthat/test-log-log-models.R:310:  testthat::expect_match(exampleText, "adjusting for cut, color, and clarity", fixed = TRUE)
tests/testthat/test-semantic-evidence-extraction.R:182:  expect_equal(scored$clarityScore, 2)
tests/testthat/test-explanation-claim-tags.R:35:  data(quakes, package = "datasets")
tests/testthat/test-explanation-claim-tags.R:37:  df = datasets::quakes[, c("mag", "stations")]
tests/testthat/test-explanation-claim-tags.R:38:  df$locn = factor(ifelse(seq_len(nrow(df)) %% 2 == 0, "SC", "WA"))
tests/testthat/test-explanation-claim-tags.R:40:  model = stats::glm(stations ~ mag * locn, family = poisson(link = "log"), data = df)
tests/testthat/test-explanation-claim-tags.R:42:    "The study asks how the expected number of earthquakes changes when magnitude grows,",
tests/testthat/test-listMetricComparisonMetrics.R:7:    expect_equal(result, c("clarityScore", "factualScore", "overallScore"))
tests/testthat/test-model-question-research-prediction.R:27:testthat::test_that("missing predictor values in research question fail safely", {
tests/testthat/test-scoreWmfmRunRecordsCore.R:20:    passThreshold = 65
tests/testthat/test-scoreWmfmRunRecordsCore.R:29:    passThreshold = 65
tests/testthat/test-scoreWmfmRunRecordsCore.R:57:    passThreshold = 65
tests/testthat/test-scoreWmfmRunRecordsCore.R:66:    passThreshold = 65
tests/testthat/test-prompt-validation-guard.R:22:    Pass = factor(c("No", "No", "Yes", "No", "Yes", "Yes", "No", "Yes")),
tests/testthat/test-prompt-validation-guard.R:26:  fit = stats::glm(Pass ~ Assign, family = stats::binomial(), data = dat)
tests/testthat/test-app-ollama-think-low-ui.R:9:test_that("app server passes the low-thinking setting to the chat provider", {
tests/testthat/test-prompt-formatted-quantities.R:3:    Locn = factor(c(rep("SC", 9), rep("WA", 9))),
tests/testthat/test-prompt-formatted-quantities.R:4:    Magnitude = c(
tests/testthat/test-prompt-formatted-quantities.R:8:    Freq = c(32, 27, 10, 9, 6, 6, 3, 2, 2, 13, 6, 2, 1, 0, 0, 1, 0, 0)
tests/testthat/test-prompt-formatted-quantities.R:11:  fit = stats::glm(Freq ~ Locn * Magnitude, family = "poisson", data = dat)
tests/testthat/test-prompt-formatted-quantities.R:21:  testthat::expect_no_match(prompt, "LocnWA:Magnitude", fixed = TRUE)
tests/testthat/test-prompt-formatted-quantities.R:57:  courseDf = utils::read.table(
tests/testthat/test-prompt-formatted-quantities.R:64:  fit = stats::glm(Pass ~ Assign, family = stats::binomial(), data = courseDf)
tests/testthat/test-prompt-formatted-quantities.R:87:  courseDf = utils::read.table(
tests/testthat/test-prompt-formatted-quantities.R:94:  fit = stats::glm(Pass ~ Attend, family = stats::binomial(), data = courseDf)
tests/testthat/test-prompt-formatted-quantities.R:160:    Pass = factor(
tests/testthat/test-prompt-formatted-quantities.R:161:      c("Fail", "Fail", "Fail", "Pass", "Pass", "Pass", "Pass", "Pass"),
tests/testthat/test-prompt-formatted-quantities.R:162:      levels = c("Fail", "Pass")
tests/testthat/test-prompt-formatted-quantities.R:166:  fit = stats::glm(Pass ~ 1, family = stats::binomial(), data = dat)
tests/testthat/test-prompt-formatted-quantities.R:171:  testthat::expect_match(promptBlock, "Pr(Pass = Pass) on the probability scale", fixed = TRUE)
tests/testthat/test-prompt-formatted-quantities.R:180:    Pass = factor(
tests/testthat/test-prompt-formatted-quantities.R:181:      c("Fail", "Fail", "Fail", "Pass", "Pass", "Pass", "Pass", "Pass"),
tests/testthat/test-prompt-formatted-quantities.R:182:      levels = c("Fail", "Pass")
tests/testthat/test-prompt-formatted-quantities.R:186:  fit = stats::glm(Pass ~ 1, family = stats::binomial(), data = dat)
tests/testthat/test-prompt-formatted-quantities.R:187:  attr(fit, "wmfm_research_question") = "What is the overall chance of passing?"
tests/testthat/test-prompt-formatted-quantities.R:191:  testthat::expect_match(prompt, "Pr(Pass = Pass) on the probability scale", fixed = TRUE)
tests/testthat/test-prompt-formatted-quantities.R:200:    Pass = factor(character(0), levels = c("Fail", "Pass"))
tests/testthat/test-prompt-formatted-quantities.R:203:  addRows = function(attend, gender, failures, passes) {
tests/testthat/test-prompt-formatted-quantities.R:205:      Attend = factor(rep(attend, failures + passes), levels = c("No", "Yes")),
tests/testthat/test-prompt-formatted-quantities.R:206:      Gender = factor(rep(gender, failures + passes), levels = c("Female", "Male")),
tests/testthat/test-prompt-formatted-quantities.R:207:      Pass = factor(c(rep("Fail", failures), rep("Pass", passes)), levels = c("Fail", "Pass"))
tests/testthat/test-prompt-formatted-quantities.R:213:    addRows("No", "Female", failures = 8, passes = 2),
tests/testthat/test-prompt-formatted-quantities.R:214:    addRows("Yes", "Female", failures = 6, passes = 4),
tests/testthat/test-prompt-formatted-quantities.R:215:    addRows("No", "Male", failures = 7, passes = 3),
tests/testthat/test-prompt-formatted-quantities.R:216:    addRows("Yes", "Male", failures = 2, passes = 8)
tests/testthat/test-prompt-formatted-quantities.R:219:  fit = stats::glm(Pass ~ Attend * Gender, family = stats::binomial(), data = d)
tests/testthat/test-prompt-formatted-quantities.R:223:  testthat::expect_match(promptBlock, "Pr(Pass = \"Pass\") when Attend = No; Gender = Female on the probability scale", fixed = TRUE)
tests/testthat/test-prompt-formatted-quantities.R:224:  testthat::expect_match(promptBlock, "Pr(Pass = \"Pass\") when Attend = Yes; Gender = Female on the probability scale", fixed = TRUE)
tests/testthat/test-prompt-formatted-quantities.R:225:  testthat::expect_match(promptBlock, "Pr(Pass = \"Pass\") when Attend = No; Gender = Male on the probability scale", fixed = TRUE)
tests/testthat/test-prompt-formatted-quantities.R:226:  testthat::expect_match(promptBlock, "Pr(Pass = \"Pass\") when Attend = Yes; Gender = Male on the probability scale", fixed = TRUE)
tests/testthat/test-prompt-formatted-quantities.R:227:  testthat::expect_match(promptBlock, "Odds(Pass = \"Pass\" vs Pass = \"Fail\") odds ratio comparing Attend = Yes with Attend = No when Gender = Female", fixed = TRUE)
tests/testthat/test-prompt-formatted-quantities.R:228:  testthat::expect_match(promptBlock, "Odds(Pass = \"Pass\" vs Pass = \"Fail\") odds ratio comparing Attend = Yes with Attend = No when Gender = Male", fixed = TRUE)
tests/testthat/test-prompt-formatted-quantities.R:229:  testthat::expect_match(promptBlock, "Ratio of odds ratios for Odds(Pass = \"Pass\" vs Pass = \"Fail\"): Attend effect at Gender = Male versus Gender = Female", fixed = TRUE)
tests/testthat/test-prompt-formatted-quantities.R:234:  testthat::expect_match(prompt, "Pr(Pass = \"Pass\") when Attend = No; Gender = Female on the probability scale", fixed = TRUE)
tests/testthat/test-prompt-formatted-quantities.R:235:  testthat::expect_match(prompt, "Odds(Pass = \"Pass\" vs Pass = \"Fail\") odds ratio comparing Attend = Yes with Attend = No when Gender = Male", fixed = TRUE)
tests/testthat/test-prompt-formatted-quantities.R:236:  testthat::expect_match(prompt, "Ratio of odds ratios for Odds(Pass = \"Pass\" vs Pass = \"Fail\"): Attend effect at Gender = Male versus Gender = Female", fixed = TRUE)
tests/testthat/test-app-developer-mode-unlock.R:57:    "background-color: #d9534f",
tests/testthat/test-app-developer-mode-unlock.R:62:    "background-color: #2e7d32",
tests/testthat/test-app-developer-mode-unlock.R:77:testthat::test_that("startup observers wire password-protected developer toggle controls", {
tests/testthat/test-app-developer-mode-unlock.R:105:    "verifyDeveloperModePassword(password)",
tests/testthat/test-parseWmfmScoringJson.R:4:    '{"effectDirectionCorrect":2,"effectScaleAppropriate":2,"referenceGroupHandledCorrectly":2,"interactionCoverageAdequate":2,"interactionSubstantiveCorrect":2,"uncertaintyHandlingAppropriate":2,"inferentialRegisterAppropriate":2,"mainEffectCoverageAdequate":2,"referenceGroupCoverageAdequate":2,"clarityAdequate":2,"numericExpressionAdequate":2,"comparisonStructureClear":2,"fatalFlawDetected":false,"factualScore":2,"inferenceScore":2,"completenessScore":2,"clarityScore":2,"calibrationScore":2,"overallScore":2,"overallPass":true,"llmScoringSummary":"Good","fieldReasons":{"effectDirectionCorrect":"ok","effectScaleAppropriate":"ok","referenceGroupHandledCorrectly":"ok","interactionCoverageAdequate":"ok","interactionSubstantiveCorrect":"ok","uncertaintyHandlingAppropriate":"ok","inferentialRegisterAppropriate":"ok","mainEffectCoverageAdequate":"ok","referenceGroupCoverageAdequate":"ok","clarityAdequate":"ok","numericExpressionAdequate":"ok","comparisonStructureClear":"ok","fatalFlawDetected":"ok"}}',
tests/testthat/test-parseWmfmScoringJson.R:13:  testthat::expect_equal(parsed$overallPass, FALSE | TRUE)
tests/testthat/test-provider-settings.R:124:test_that("provider settings UI no longer includes provider switch password input", {
tests/testthat/test-provider-settings.R:127:  expect_false(grepl("providerSwitchPassword", uiText, fixed = TRUE))
tests/testthat/test-provider-settings.R:128:  expect_false(grepl("Password required to switch/save Claude", uiText, fixed = TRUE))
tests/testthat/test-provider-settings.R:166:  expect_match(uiText, "color: #9aa0a6", fixed = TRUE)
tests/testthat/test-app-developer-scoring-grading.R:136:    Pass = c(0, 0, 1, 0, 1, 1),
tests/testthat/test-app-developer-scoring-grading.R:140:  logisticModel = stats::glm(Pass ~ StudyHours, data = data, family = stats::binomial())
tests/testthat/test-app-developer-scoring-grading.R:234:    clarityScore = 1.667,
tests/testthat/test-app-developer-scoring-grading.R:245:    clarityAdequate = 1,
tests/testthat/test-app-developer-scoring-grading.R:258:  clarityRow = metricSummary[metricSummary$metric == "clarityScore", , drop = FALSE]
tests/testthat/test-app-developer-scoring-grading.R:262:  testthat::expect_equal(clarityRow$studentValue, 1.75)
tests/testthat/test-model-numeric-anchor-prompting.R:21:    Magnitude = c(5.25, 5.5, 6, 7.25),
tests/testthat/test-model-numeric-anchor-prompting.R:22:    Locn = factor(c("SC", "SC", "WA", "WA"))
tests/testthat/test-model-numeric-anchor-prompting.R:25:  fit = stats::glm(y ~ Locn * Magnitude, family = "poisson", data = dat)
tests/testthat/test-model-numeric-anchor-prompting.R:29:  expect_match(prompt, "Magnitude: observed range = [5.25, 7.25]", fixed = TRUE)
tests/testthat/test-model-numeric-anchor-prompting.R:36:    Magnitude = c(5.25, 5.5, 6, 7.25),
tests/testthat/test-model-numeric-anchor-prompting.R:37:    Locn = factor(c("SC", "SC", "WA", "WA"))
tests/testthat/test-model-numeric-anchor-prompting.R:40:  fit = stats::glm(y ~ Locn * Magnitude, family = "poisson", data = dat)
tests/testthat/test-app-server-fit-model-helpers.R:2:  msg = buildChatProviderConnectionFailedMessage("connection refused")
tests/testthat/test-app-server-fit-model-helpers.R:19:    buildLinearModelBinaryFactorRecodingMessage("passed", c("no", "yes")),
tests/testthat/test-app-server-fit-model-helpers.R:20:    "The response variable 'passed' is a 2-level factor.\nFor linear regression, it has been recoded to numeric.\nCoding used:  no -> 0,   yes -> 1"
tests/testthat/test-load-examples-and-research-question-ui.R:214:    "inputId = \"developerModePassword\"",
tests/testthat/test-load-examples-and-research-question-ui.R:269:    "test-Course Follow-Up",
tests/testthat/test-grade-method-selection.R:66:          '"clarityAdequate": 2,',
tests/testthat/test-grade-method-selection.R:73:          '"clarityScore": 2.0,',
tests/testthat/test-grade-method-selection.R:76:          '"overallPass": true,',
tests/testthat/test-grade-method-selection.R:88:          '"clarityAdequate": "Clarity was adequate.",',
tests/testthat/test-runExample.R:52:    name = "Course",
tests/testthat/test-equation-cases.R:101:    pass = factor(c("Fail", "Fail", "Pass", "Pass", "Fail", "Pass")),
tests/testthat/test-equation-cases.R:105:  model = glm(pass ~ x, data = df, family = binomial(link = "logit"))
tests/testthat/test-plot-model-log-log.R:3:    price = c(1000, 1500, 2200, 3100, 4600, 6900),
tests/testthat/test-plot-model-log-log.R:4:    carat = c(0.3, 0.4, 0.55, 0.75, 1.0, 1.4)
tests/testthat/test-plot-model-log-log.R:7:  model = lm(log(price) ~ log(carat), data = dat)
tests/testthat/test-plot-model-log-log.R:16:    price = c(1000, 1500, 2200, 3100, 4600, 6900),
tests/testthat/test-plot-model-log-log.R:17:    carat = c(0.3, 0.4, 0.55, 0.75, 1.0, 1.4)
tests/testthat/test-plot-model-log-log.R:20:  model = lm(log(price) ~ log(carat), data = dat)
tests/testthat/test-postProcessExplanationText.R:13:  text = "A one-magnitude rise multiplies the expected count by 0.21."
tests/testthat/test-postProcessExplanationText.R:19:    "If the magnitude increases by one, the expected count is multiplied by 0.21."
tests/testthat/test-postProcessExplanationText.R:21:  testthat::expect_false(grepl("one-magnitude rise", out, ignore.case = TRUE))
tests/testthat/test-postProcessExplanationText.R:63:    "The interaction term makes the decrease with magnitude steeper in Washington than in California.",
tests/testthat/test-postProcessExplanationText.R:72:  testthat::expect_true(grepl("decrease with magnitude is steeper", out, fixed = TRUE))
tests/testthat/test-postProcessExplanationText.R:139:    "A one-magnitude rise multiplies the expected count by 0.21, with values between 0.13 and 0.31.",
tests/testthat/test-postProcessExplanationText.R:153:  text = "Answer: A one-magnitude rise multiplies the expected count by 0.21."
tests/testthat/test-postProcessExplanationText.R:161:    "If the magnitude increases by one, the expected count is multiplied by 0.21."
tests/testthat/test-postProcessExplanationText.R:313:    "The interaction term shows that a one-magnitude rise matters.",
tests/testthat/test-postProcessExplanationText.R:327:  testthat::expect_true(any(out$matchedText == "one-magnitude rise"))
tests/testthat/test-postProcessExplanationText.R:383:    "The interaction term shows that a one-magnitude rise matters.",
tests/testthat/test-postProcessExplanationText.R:451:    "A one-magnitude rise multiplies the expected count by 0.21."
tests/testthat/test-postProcessExplanationText.R:541:    "The question asks whether diamond weight can be used to predict diamond price.",
tests/testthat/test-postProcessExplanationText.R:542:    "Both price and weight were analyzed on a log scale.",
tests/testthat/test-summariseMetricComparison.R:26:    clarityRow = result[result$metric == "clarityScore", , drop = FALSE]
tests/testthat/test-summariseMetricComparison.R:28:    expect_equal(nrow(clarityRow), 1)
tests/testthat/test-summariseMetricComparison.R:29:    expect_equal(clarityRow$nUniqueDeterministic, 2)
tests/testthat/test-summariseMetricComparison.R:30:    expect_equal(clarityRow$modalProportionDeterministic, 2 / 3)
tests/testthat/test-summariseMetricComparison.R:31:    expect_true(is.finite(clarityRow$entropyDeterministic))
tests/testthat/test-summariseMetricComparison.R:75:    expect_equal(result$metric, c("clarityScore", "factualScore", "overallScore"))
tests/testthat/test-summariseMetricComparison.R:149:    expect_equal(result$metric, c("factualScore", "clarityScore", "overallScore"))
man/diagnoseExplanationSurfaceProcessing.Rd:12:\item{audit}{Optional explanation audit object passed to
man/diagnoseExplanationSurfaceProcessing.Rd:28:  "A one-magnitude rise multiplies the expected count by 0.21."
man/normaliseNumericExpressions.Rd:18:failures where decimals or percentages are written in words rather than
man/drawModelPlot.Rd:19:\code{"sandwich"}. For factor-only models, this is passed through to
man/score.Rd:12:\item{...}{Additional arguments passed to methods.}
man/getBinomialOutcomeLabels.Rd:5:\title{Get success and failure labels for a binomial response}
man/getBinomialOutcomeLabels.Rd:13:A list with \code{successLabel}, \code{failureLabel}, and \code{responseName}.
man/getBinomialOutcomeLabels.Rd:18:labels for the success and failure outcomes so the app can write
man/getBinomialOutcomeLabels.Rd:21:\code{Pr(Pass = Pass)} and \code{Odds(Pass = Pass)} rather than a generic \code{p}.
man/grade.Rd:12:\item{...}{Additional arguments passed to methods.}
man/makeDeveloperModePasswordHash.Rd:3:\name{makeDeveloperModePasswordHash}
man/makeDeveloperModePasswordHash.Rd:4:\alias{makeDeveloperModePasswordHash}
man/makeDeveloperModePasswordHash.Rd:5:\title{Create a developer-mode password hash}
man/makeDeveloperModePasswordHash.Rd:7:makeDeveloperModePasswordHash(password)
man/makeDeveloperModePasswordHash.Rd:10:\item{password}{Character scalar giving the plain-text password.}
man/makeDeveloperModePasswordHash.Rd:13:A salted password hash string.
man/makeDeveloperModePasswordHash.Rd:16:Creates a salted password hash suitable for storing in the
man/makeDeveloperModePasswordHash.Rd:17:\code{WMFM_DEVELOPER_MODE_PASSWORD_HASH} environment variable.
man/compare.Rd:14:\item{...}{Additional arguments passed to methods.}
man/buildScalePhrasingRules.Rd:29:student-facing language (e.g. \code{"oyster count"}). If \code{NULL},
man/grade.wmfmModel.Rd:50:\item{...}{Additional arguments passed to \code{score()} when \code{autoScore = TRUE}.}
man/summary.wmfmRuns.Rd:20:frequencies. Judged fields and score summaries are intentionally excluded and
man/buildPromptValidationGuardBlock.Rd:20:model to avoid known failure modes, while leaving automatic regeneration for a
man/buildAppExplanation.Rd:14:\item{useExplanationCache}{Logical. Passed through to \code{lmExplanation()} when
man/score.wmfmRuns.Rd:25:\item{useCache}{Logical. Passed to LLM scoring helpers.}
man/score.wmfmRuns.Rd:30:\item{verbose}{Logical. Passed to LLM scoring helpers.}
man/classifyWmfmScoreDisagreement.Rd:18:\item{continuousBreaks}{Numeric vector of length 3 giving cut points for the
man/buildDelimitedFileReadFailedMessage.Rd:3:\name{buildDelimitedFileReadFailedMessage}
man/buildDelimitedFileReadFailedMessage.Rd:4:\alias{buildDelimitedFileReadFailedMessage}
man/buildDelimitedFileReadFailedMessage.Rd:5:\title{Build message for a failed delimited file read.}
man/buildDelimitedFileReadFailedMessage.Rd:7:buildDelimitedFileReadFailedMessage()
man/buildDelimitedFileReadFailedMessage.Rd:13:Build message for a failed delimited file read.
man/scoreWmfmRepeatedRuns.Rd:14:  passThreshold = 65,
man/scoreWmfmRepeatedRuns.Rd:18:  clarityWeight = 0.15,
man/scoreWmfmRepeatedRuns.Rd:41:\item{passThreshold}{Numeric in \code{0} to \code{100}. Threshold used to create
man/scoreWmfmRepeatedRuns.Rd:42:\code{overallPass}.}
man/scoreWmfmRepeatedRuns.Rd:50:\item{clarityWeight}{Numeric weight for the clarity dimension.}
man/scoreWmfmRepeatedRuns.Rd:87:\item{Clarity score}{Whether the explanation is reasonably clear, sensibly
man/scoreWmfmRepeatedRuns.Rd:109:\item{clarityAdequate}{Integer in \code{0}, \code{1}, \code{2}.}
man/scoreWmfmRepeatedRuns.Rd:122:\item{clarityScore}{Numeric score on a \code{0} to \code{2} scale.}
man/scoreWmfmRepeatedRuns.Rd:125:\item{overallPass}{Logical.}
man/buildAppExplanationAudit.Rd:14:fails.
man/formatBinomialOddsLabel.Rd:9:  outcome = c("success", "failure"),
man/formatBinomialOddsLabel.Rd:16:\item{outcome}{Either \code{"success"} or \code{"failure"}.}
man/formatBinomialOddsLabel.Rd:21:A character scalar such as \code{"Odds(Pass = Pass)"} or \code{"Odds(Pass_i = Pass)"}.
man/runExample.Rd:27:\item{printOutput}{Logical. Passed to \code{runModel()}.}
man/runExample.Rd:34:\item{useExplanationCache}{Logical. Passed to \code{runModel()}.
man/runExample.Rd:40:\item{...}{Additional arguments passed to \code{runModel()}.}
man/runExample.Rd:68:x = runExample("Course")
man/runExample.Rd:69:y = runExample("Course", nRuns = 20)
man/addDerivedVariableToData.Rd:37:df = data.frame(passengers = 1:144)
man/buildDeveloperModeIncorrectPasswordMessage.Rd:3:\name{buildDeveloperModeIncorrectPasswordMessage}
man/buildDeveloperModeIncorrectPasswordMessage.Rd:4:\alias{buildDeveloperModeIncorrectPasswordMessage}
man/buildDeveloperModeIncorrectPasswordMessage.Rd:5:\title{Build the incorrect developer mode password message}
man/buildDeveloperModeIncorrectPasswordMessage.Rd:7:buildDeveloperModeIncorrectPasswordMessage()
man/buildDeveloperModeIncorrectPasswordMessage.Rd:13:Build the incorrect developer mode password message
man/generateBadExplanation.Rd:71:object. The generated explanations are returned in a form that can be passed
man/diagnose.Rd:12:\item{...}{Additional arguments passed to methods.}
man/plotWmfmExplanationClaimHeatmap.Rd:40:\item{includeLegendBreaks}{Logical. Passed to \code{orderWmfmLegendValues()}.}
man/getWmfmClaimColorMap.Rd:3:\name{getWmfmClaimColorMap}
man/getWmfmClaimColorMap.Rd:4:\alias{getWmfmClaimColorMap}
man/getWmfmClaimColorMap.Rd:7:getWmfmClaimColorMap()
man/getWmfmClaimColorMap.Rd:21:getWmfmClaimColorMap()
man/buildChatProviderConnectionFailedMessage.Rd:3:\name{buildChatProviderConnectionFailedMessage}
man/buildChatProviderConnectionFailedMessage.Rd:4:\alias{buildChatProviderConnectionFailedMessage}
man/buildChatProviderConnectionFailedMessage.Rd:5:\title{Build message for a failed chat provider connection.}
man/buildChatProviderConnectionFailedMessage.Rd:7:buildChatProviderConnectionFailedMessage(details)
man/buildChatProviderConnectionFailedMessage.Rd:16:Build message for a failed chat provider connection.
man/buildAppModelOutputs.Rd:19:\item{useExplanationCache}{Logical. Passed through to \code{lmExplanation()} when
man/buildAppModelOutputs.Rd:33:deterministic fallback if the LLM equation request fails.
man/score.wmfmGradeListObj.Rd:31:\item{...}{Additional arguments passed to \code{score.wmfmGrade()}.}
man/validResponseVars.Rd:12:\item{modelType}{Model type passed to \code{validateResponseVar()}.}
man/verifyDeveloperModePassword.Rd:3:\name{verifyDeveloperModePassword}
man/verifyDeveloperModePassword.Rd:4:\alias{verifyDeveloperModePassword}
man/verifyDeveloperModePassword.Rd:5:\title{Verify the developer-mode password}
man/verifyDeveloperModePassword.Rd:7:verifyDeveloperModePassword(password)
man/verifyDeveloperModePassword.Rd:10:\item{password}{Character scalar giving the candidate password.}
man/verifyDeveloperModePassword.Rd:13:Logical scalar. Returns \code{TRUE} when the password matches and
man/verifyDeveloperModePassword.Rd:17:Compares a candidate password against the salted hash stored in the
man/verifyDeveloperModePassword.Rd:18:\code{WMFM_DEVELOPER_MODE_PASSWORD_HASH} environment variable.
man/completeConfidenceIntervalNewData.Rd:32:the one-row \code{newData} passed to \code{predict()}, filling any omitted
man/buildGlmTeachingNotation.Rd:22:as \code{"Pr(Pass = Yes)"} and \code{"Odds(Pass = Yes)"}. The second level
man/buildGlmTeachingNotation.Rd:27:name, such as \code{"E[Freq]"}.
man/parseWeightText.Rd:23:If parsing fails, the function returns \code{NA_real_}.
man/plot.wmfmScoreComparison.Rd:14:\item{...}{Additional arguments passed to the underlying helper.}
man/buildPackageDatasetLoadFailedMessage.Rd:3:\name{buildPackageDatasetLoadFailedMessage}
man/buildPackageDatasetLoadFailedMessage.Rd:4:\alias{buildPackageDatasetLoadFailedMessage}
man/buildPackageDatasetLoadFailedMessage.Rd:5:\title{Build message for a package dataset loading failure.}
man/buildPackageDatasetLoadFailedMessage.Rd:7:buildPackageDatasetLoadFailedMessage(datasetName, packageName)
man/buildPackageDatasetLoadFailedMessage.Rd:18:Build message for a package dataset loading failure.
man/runWmfmCliStage.Rd:14:\item{code}{Function with no arguments to execute.}
man/parseNaturalLogCall.Rd:10:\item{expr}{Character expression such as \code{log(price)}.}
man/formatBinomialProbabilityLabel.Rd:9:  outcome = c("success", "failure"),
man/formatBinomialProbabilityLabel.Rd:16:\item{outcome}{Either \code{"success"} or \code{"failure"}.}
man/formatBinomialProbabilityLabel.Rd:21:A character scalar such as \code{"Pr(Pass = Pass)"} or \code{"Pr(Pass_i = Pass)"}.
man/makeWmfmDeterministicCategoryColors.Rd:3:\name{makeWmfmDeterministicCategoryColors}
man/makeWmfmDeterministicCategoryColors.Rd:4:\alias{makeWmfmDeterministicCategoryColors}
man/makeWmfmDeterministicCategoryColors.Rd:7:makeWmfmDeterministicCategoryColors(values, naLabel = "(missing)")
man/makeWmfmDeterministicCategoryColors.Rd:28:makeWmfmDeterministicCategoryColors(c("yes", "no", "mixed", "(missing)"))
man/plot.wmfmRuns.Rd:15:\item{...}{Passed through to lower-level plotting helpers.}
man/plot.wmfmRuns.Rd:29:\item{\code{"claims"}}{Bar plot of extracted binary claim frequencies across runs.}
man/auditBadExplanationGrading.Rd:59:    wrongScaleError = c("factualScore", "clarityScore"),
man/auditBadExplanationGrading.Rd:61:    logicalContradiction = c("factualScore", "clarityScore")
man/describeField.Rd:16:\item{...}{Additional arguments passed to methods.}
man/storeDeveloperModePasswordHash.Rd:3:\name{storeDeveloperModePasswordHash}
man/storeDeveloperModePasswordHash.Rd:4:\alias{storeDeveloperModePasswordHash}
man/storeDeveloperModePasswordHash.Rd:5:\title{Store a developer-mode password hash}
man/storeDeveloperModePasswordHash.Rd:7:storeDeveloperModePasswordHash(password)
man/storeDeveloperModePasswordHash.Rd:10:\item{password}{Character scalar giving the plain-text password.}
man/storeDeveloperModePasswordHash.Rd:13:A salted password hash string.
man/storeDeveloperModePasswordHash.Rd:16:Backward-compatible alias for \code{makeDeveloperModePasswordHash()}.
man/getWmfmRunsClaimsData.Rd:5:\title{Build extracted-claim frequency data for a WMFM runs object}
man/getWmfmRunsClaimsData.Rd:13:A data frame summarising claim frequencies.
man/getWmfmRunsClaimsData.Rd:16:Build extracted-claim frequency data for a WMFM runs object
man/renderEquationCase.Rd:27:Supported first-pass families are Gaussian identity, binomial logit, and
man/stability.Rd:12:\item{...}{Additional arguments passed to methods.}
man/score.wmfmGrade.Rd:13:  passThreshold = 65,
man/score.wmfmGrade.Rd:27:\item{preferredMinWords}{Integer. Passed to deterministic scoring.}
man/score.wmfmGrade.Rd:29:\item{preferredMaxWords}{Integer. Passed to deterministic scoring.}
man/score.wmfmGrade.Rd:31:\item{fatalFlawCap}{Numeric. Passed to deterministic scoring.}
man/score.wmfmGrade.Rd:33:\item{passThreshold}{Numeric. Passed to deterministic scoring.}
man/score.wmfmGrade.Rd:37:\item{useCache}{Logical. Passed to LLM scoring.}
man/score.wmfmGrade.Rd:42:\item{verbose}{Logical. Passed to LLM scoring.}
man/score.wmfmGrade.Rd:47:\item{...}{Additional arguments passed to the relevant scoring helper.}
man/computeFactorOnlyContrast.Rd:25:\item{hcType}{Sandwich type passed to \code{sandwich::vcovHC()} (e.g. \code{"HC0"}, \code{"HC3"}).}
NEWS.md:6:- Updated log-log tests to require the improved Diamonds IV wording and to keep technical diagnostics out of the appended student answer.
NEWS.md:68:- Keep the summary text student-facing and separate from diagnostic pass/fail language.
NEWS.md:112:* Restore password protection for the maintainer-only developer-mode toggle while
NEWS.md:180:* Change the Stage 29 runner workflow so failed build attempts still consume the
NEWS.md:190:* Harden Ollama discovery so failures are handled gracefully and tests do not
NEWS.md:201:  Diamonds II, Diamonds III, and Diamonds IV examples.
NEWS.md:249:- Allowed the log-log percentage-language post-processing rule to bypass numeric-token preservation while keeping the guard active for other rules.
NEWS.md:256:- Refined log-log unit-change follow-up wording so fixed carat changes keep the reference point and use carat units.
NEWS.md:258:- Added post-processing coverage for the model-estimate sentence variant seen in Diamonds IV diagnostics.
```

## Candidate production files to inspect first

```text
inst/extdata/developer-scoring/stage18-9/wmfm-scoring-export-20260508-143946.json
inst/extdata/developer-scoring/stage18-9/wmfm-scoring-export-20260508-144104.json
inst/extdata/developer-scoring/stage18-9/wmfm-scoring-export-20260508-144814.json
inst/extdata/developer-scoring/stage18-9/wmfm-scoring-export-20260508-145041.json
inst/extdata/developer-scoring/stage18-9/wmfm-scoring-export-20260508-145223.json
inst/extdata/developer-scoring/stage19-3-1/wmfm-scoring-export-20260510-112154.json
inst/extdata/developer-scoring/stage19-3-1/wmfm-scoring-export-20260510-112245.json
inst/extdata/developer-scoring/stage19-3-1/wmfm-scoring-export-20260510-112328.json
inst/extdata/developer-scoring/stage19-3-1/wmfm-scoring-export-20260510-112425.json
inst/extdata/developer-scoring/stage19-3-1/wmfm-scoring-export-20260510-112534.json
inst/extdata/examples/Course/Course.spec.yml
inst/extdata/examples/DiamondsII/DiamondsII.spec.yml
inst/extdata/examples/DiamondsIII/DiamondsIII.spec.yml
inst/extdata/examples/DiamondsIV/DiamondsIV.spec.yml
inst/extdata/examples/DiamondsUnitChange/DiamondsUnitChange.spec.yml
inst/extdata/examples/Oysters/Oysters.spec.yml
inst/extdata/examples/Quakes/Quakes.Rmd
inst/extdata/examples/Quakes/Quakes.spec.yml
inst/extdata/examples/QuakesUnitChange0_1/QuakesUnitChange0_1.Rmd
inst/extdata/examples/QuakesUnitChange0_1/QuakesUnitChange0_1.spec.yml
inst/extdata/examples/test-01-G00F/test-01-G00F.spec.yml
inst/extdata/examples/test-02-G01F/test-02-G01F.spec.yml
inst/extdata/examples/test-03-G10F/test-03-G10F.spec.yml
inst/extdata/examples/test-04-G20F/test-04-G20F.spec.yml
inst/extdata/examples/test-05-G20T/test-05-G20T.spec.yml
inst/extdata/examples/test-06-G11F/test-06-G11F.spec.yml
inst/extdata/examples/test-07-G11T/test-07-G11T.spec.yml
inst/extdata/examples/test-08-B00F/test-08-B00F.spec.yml
inst/extdata/examples/test-09-B01F/test-09-B01F.spec.yml
inst/extdata/examples/test-10-B10F/test-10-B10F.spec.yml
inst/extdata/examples/test-11-B20F/test-11-B20F.spec.yml
inst/extdata/examples/test-12-B20T/test-12-B20T.spec.yml
inst/extdata/examples/test-13-B11F/test-13-B11F.spec.yml
inst/extdata/examples/test-14-B11T/test-14-B11T.spec.yml
inst/extdata/examples/test-15-P00F/test-15-P00F.spec.yml
inst/extdata/examples/test-16-P01F/test-16-P01F.spec.yml
inst/extdata/examples/test-17-P10F/test-17-P10F.spec.yml
inst/extdata/examples/test-18-P11F/test-18-P11F.spec.yml
inst/extdata/examples/test-19-P11T/test-19-P11T.spec.yml
inst/extdata/examples/test-20-B01F-followup/test-20-B01F-followup.spec.yml
inst/extdata/examples/test-21-B11F-followup/test-21-B11F-followup.spec.yml
inst/extdata/examples/test-22-P01F-followup/test-22-P01F-followup.spec.yml
inst/extdata/examples/test-23-P11F-followup/test-23-P11F-followup.spec.yml
inst/extdata/examples/test-24-B01F-followup-odds/test-24-B01F-followup-odds.spec.yml
inst/extdata/examples/test-25-B11F-followup-odds/test-25-B11F-followup-odds.spec.yml
inst/extdata/examples/test-course-follow-up/test-course-follow-up.spec.yml
inst/extdata/examples/test-SG-1/context.md
inst/extdata/examples/test-SG-1/sg-course.csv
inst/extdata/examples/test-SG-1/test-SG-1.spec.yml
inst/extdata/examples/test-SG-2/context.md
inst/extdata/examples/test-SG-2/sg-course.csv
inst/extdata/examples/test-SG-2/test-SG-2.spec.yml
inst/extdata/examples/test-SG-3/context.md
inst/extdata/examples/test-SG-3/sg-course.csv
inst/extdata/examples/test-SG-3/test-SG-3.spec.yml
inst/extdata/examples/test-SG-4/context.md
inst/extdata/examples/test-SG-4/sg-course.csv
inst/extdata/examples/test-SG-4/test-SG-4.spec.yml
inst/extdata/examples/test-SG-5/context.md
inst/extdata/examples/test-SG-5/sg-course.csv
inst/extdata/examples/test-SG-5/test-SG-5.spec.yml
inst/extdata/STATS20x.txt
R/api-compare.R
R/api-describeField.R
R/api-diagnose.R
R/api-generateBadExplanation.R
R/api-grade.R
R/api-runModel.R
R/api-score.R
R/api-stability.R
R/app-developer-scoring-grading.R
R/app-equation-runtime.R
R/app-explanationAudit-uiHelpers.R
R/app-output-messages.R
R/app-plot-uiHelpers.R
R/app-server-contrasts.R
R/app-server-data-load-helpers.R
R/app-server-data-observers.R
R/app-server-developer-mode-helpers.R
R/app-server-fit-model-helpers.R
R/app-server-fit-model.R
R/app-server-fitted-equations.R
R/app-server-model-setup.R
R/app-server-startup.R
R/app-ui.R
R/examples-run.R
R/explain-badExplanation-response.R
R/methods-compare-wmfmGrade.R
R/methods-grade-wmfmModel.R
R/methods-plot-metricComparisonData.R
R/methods-plot-wmfmRuns.R
R/methods-plot-wmfmScoreComparison.R
R/methods-plot-wmfmScores.R
R/methods-print-summary-wmfmRuns.R
R/methods-print-wmfmGrade.R
R/methods-print-wmfmGradeComparison.R
R/methods-score-wmfmGrade.R
R/methods-score-wmfmGradeListObj.R
R/methods-score-wmfmRuns.R
R/methods-stability-wmfmScores.R
R/methods-summary-wmfmGrade.R
R/methods-summary-wmfmRuns.R
R/model-binomial-outcomes.R
R/model-ci-data.R
R/model-data.R
R/model-equation-mean.R
R/model-equation-render.R
R/model-explanation-cleanText.R
R/model-explanationAudit-supportNotes.R
R/model-explanationAudit.R
R/model-explanationClaimTagDetectors.R
R/model-factorOnly.R
R/model-glm-notation.R
R/model-logLog.R
R/model-plots.R
R/model-question-adjustment-comparison.R
R/model-question-classifier.R
R/model-question-followup-answer.R
R/model-question-prediction-lm.R
R/model-response.R
R/model-scale-phrasingRules.R
R/model-weights-parseText.R
R/plot-claims.R
R/plot-model.R
R/plot-score.R
R/prompt-core.R
R/prompt-equation.R
R/prompt-explain.R
R/prompt-score.R
R/prompt-validationGuard.R
R/scoring-comparison.R
R/scoring-explainFieldScore.R
R/scoring-fields-extract.R
R/scoring-grade-summariseLosses.R
R/scoring-llm.R
R/scoring-metricRegistry.R
R/scoring-repeatedRuns.R
R/scoring-runRecords-core.R
R/scoring-runs.R
R/scoring-semanticEvidence.R
R/text-describeField.R
R/text-utils.R
R/utils-developerModeAuth.R
R/utils-progress.R
R/utils-runRecords.R
```
