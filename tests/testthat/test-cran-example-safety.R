extractRdCommandBlock = function(text, command) {
  marker = paste0("\\", command, "{")
  start = regexpr(marker, text, fixed = TRUE)[1]

  if (start < 0) {
    return("")
  }

  openIndex = start + nchar(marker) - 1L
  closeIndex = findMatchingBrace(text, openIndex)

  if (is.na(closeIndex)) {
    return("")
  }

  substring(text, openIndex + 1L, closeIndex - 1L)
}

removeRdCommandBlock = function(text, command) {
  marker = paste0("\\", command, "{")

  repeat {
    start = regexpr(marker, text, fixed = TRUE)[1]

    if (start < 0) {
      break
    }

    openIndex = start + nchar(marker) - 1L
    closeIndex = findMatchingBrace(text, openIndex)

    if (is.na(closeIndex)) {
      break
    }

    beforeText = if (start > 1L) {
      substring(text, 1L, start - 1L)
    } else {
      ""
    }

    afterText = if (closeIndex < nchar(text)) {
      substring(text, closeIndex + 1L)
    } else {
      ""
    }

    text = paste0(beforeText, afterText)
  }

  text
}

findMatchingBrace = function(text, openIndex) {
  chars = strsplit(text, "", fixed = TRUE)[[1]]
  depth = 0L

  for (i in seq.int(openIndex, length(chars))) {
    if (identical(chars[[i]], "{") && !isEscaped(chars, i)) {
      depth = depth + 1L
    }

    if (identical(chars[[i]], "}") && !isEscaped(chars, i)) {
      depth = depth - 1L

      if (depth == 0L) {
        return(i)
      }
    }
  }

  NA_integer_
}

isEscaped = function(chars, index) {
  if (index <= 1L) {
    return(FALSE)
  }

  slashCount = 0L
  cursor = index - 1L

  while (cursor >= 1L && identical(chars[[cursor]], "\\")) {
    slashCount = slashCount + 1L
    cursor = cursor - 1L
  }

  slashCount %% 2L == 1L
}

test_that("runnable Rd examples do not contact external providers", {
  manDir = test_path("..", "..", "man")
  rdFiles = list.files(manDir, pattern = "[.]Rd$", full.names = TRUE)

  riskyCalls = c(
    "getChatProvider(",
    "runWMFMApp(",
    "runExample(",
    "generateBadExplanation(",
    "generateBadExplanationWithLlm(",
    "scoreWmfmRunWithLlm("
  )

  offenders = character(0)

  for (rdFile in rdFiles) {
    rdText = paste(readLines(rdFile, warn = FALSE), collapse = "\n")
    examplesText = extractRdCommandBlock(rdText, "examples")

    if (!nzchar(examplesText)) {
      next
    }

    runnableExamples = removeRdCommandBlock(examplesText, "dontrun")
    runnableExamples = removeRdCommandBlock(runnableExamples, "donttest")

    for (riskyCall in riskyCalls) {
      if (grepl(riskyCall, runnableExamples, fixed = TRUE)) {
        offenders = c(
          offenders,
          paste0(basename(rdFile), ": ", riskyCall)
        )
      }
    }
  }

  expect_equal(offenders, character(0))
})
