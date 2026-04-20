#' Extract text from WMFM output objects
#'
#' Converts model explanation or equation objects into plain text.
#'
#' @param x Object returned by WMFM functions.
#'
#' @return Character string.
#' @export
extractWmfmText = function(x) {
  if (is.null(x)) return(NA_character_)

  if (is.character(x)) {
    return(paste(x, collapse = "\n"))
  }

  if (inherits(x, "wmfmEquationTable")) {
    return(
      paste(
        formatWmfmEquationTableLines(x),
        collapse = "\n"
      )
    )
  }

  tryCatch(
    paste(capture.output(print(x)), collapse = "\n"),
    error = function(e) NA_character_
  )
}
#' Count words in text
#'
#' @param x Character text.
#'
#' @return Integer.
#' @export
countWmfmWords = function(x) {
  if (is.na(x) || !nzchar(x)) return(0L)
  length(strsplit(x, "[[:space:]]+")[[1]])
}
#' Count sentences in text
#'
#' @param x Character text.
#'
#' @return Integer.
#' @export
countWmfmSentences = function(x) {
  if (is.na(x)) return(NA_integer_)
  length(unlist(regmatches(x, gregexpr("[.!?]+", x))))
}
#' Parse a single assignment statement from text
#'
#' Parses user input and verifies it is exactly one assignment statement of the form
#' \code{name = expr} or \code{name <- expr}. This is designed for validating a
#' derived-variable input box in a Shiny app.
#'
#' @param txt A length-1 character string containing R code.
#'
#' @return A list with elements:
#' \itemize{
#'   \item \code{ok}: logical, whether parsing/validation succeeded.
#'   \item \code{msg}: character message if \code{ok = FALSE}.
#'   \item \code{name}: (if ok) the variable name on the LHS.
#'   \item \code{rhs}: (if ok) the RHS expression (language object).
#' }
#'
#' @examples
#' parseSingleAssignment("t = 1:10")$ok
#' parseSingleAssignment("1:10")$ok
#' parseSingleAssignment("x = log(y)")$name
#'
#' @export
parseSingleAssignment = function(txt) {
  txt = trimws(txt)

  if (txt == "") {
    return(list(ok = FALSE, msg = "Enter an assignment like: t = 1:nrow(data)"))
  }

  if (!grepl("=", txt, fixed = TRUE) || grepl("~", txt, fixed = TRUE)) {
    return(list(ok = FALSE, msg = "Enter a single assignment like: t = 1:nrow(data)"))
  }

  # Allow ":" for sequences; keep a conservative whitelist.
  # Put "-" at the end of the character class to avoid invalid ranges in TRE.
  if (!grepl("^[A-Za-z][A-Za-z0-9_]*\\s*=\\s*[0-9A-Za-z_:+*/^()., -]+$", txt)) {
    return(list(ok = FALSE, msg = "Derived-variable expression contains illegal characters."))
  }

  expr = tryCatch(parse(text = txt), error = function(e) NULL)
  if (is.null(expr) || length(expr) != 1) {
    return(list(ok = FALSE, msg = "Derived-variable expression must be a single assignment."))
  }

  e1 = expr[[1]]
  if (!is.call(e1) || as.character(e1[[1]]) != "=") {
    return(list(ok = FALSE, msg = "Expression must be of the form: name = expression"))
  }

  lhs = e1[[2]]
  if (!is.name(lhs)) {
    return(list(ok = FALSE, msg = "Left-hand side must be a single variable name."))
  }

  list(ok = TRUE, name = as.character(lhs), rhs = e1[[3]])
}

#' Normalise text for comparison
#'
#' Lowercases and removes extra whitespace.
#'
#' @param x Character text.
#'
#' @return Character string.
#' @export
normaliseWmfmText = function(x) {
  if (is.na(x)) return(NA_character_)

  x = tolower(x)
  x = gsub("[[:space:]]+", " ", x)
  trimws(x)
}
#' Normalise common worded numeric expressions in WMFM text
#'
#' Applies a lightweight set of rule-based replacements to common language-model
#' failures where decimals or percentages are written in words rather than
#' numerals. The helper is intentionally narrow and targets the specific forms
#' that WMFM most needs to control, such as "six and a half" or
#' "sixty-two percent".
#'
#' @param text A character vector.
#'
#' @return A character vector with common worded numeric expressions rewritten in
#'   numeral form.
#' @keywords internal
normaliseNumericExpressions = function(text) {

  if (!is.character(text)) {
    stop("`text` must be a character vector.", call. = FALSE)
  }

  out = text
  out = gsub("[\u2010\u2011\u2012\u2013\u2014\u2015]", "-", out, perl = TRUE)

  decimalMap = c(
    "zero and a half" = "0.5",
    "one and a half" = "1.5",
    "two and a half" = "2.5",
    "three and a half" = "3.5",
    "four and a half" = "4.5",
    "five and a half" = "5.5",
    "six and a half" = "6.5",
    "seven and a half" = "7.5",
    "eight and a half" = "8.5",
    "nine and a half" = "9.5",
    "ten and a half" = "10.5"
  )

  percentMap = c(
    "zero percent" = "0 percent",
    "one percent" = "1 percent",
    "two percent" = "2 percent",
    "three percent" = "3 percent",
    "four percent" = "4 percent",
    "five percent" = "5 percent",
    "six percent" = "6 percent",
    "seven percent" = "7 percent",
    "eight percent" = "8 percent",
    "nine percent" = "9 percent",
    "ten percent" = "10 percent",
    "eleven percent" = "11 percent",
    "twelve percent" = "12 percent",
    "thirteen percent" = "13 percent",
    "fourteen percent" = "14 percent",
    "fifteen percent" = "15 percent",
    "sixteen percent" = "16 percent",
    "seventeen percent" = "17 percent",
    "eighteen percent" = "18 percent",
    "nineteen percent" = "19 percent",
    "twenty percent" = "20 percent",
    "thirty percent" = "30 percent",
    "forty percent" = "40 percent",
    "fifty percent" = "50 percent",
    "sixty percent" = "60 percent",
    "seventy percent" = "70 percent",
    "eighty percent" = "80 percent",
    "ninety percent" = "90 percent",
    "twenty-one percent" = "21 percent",
    "twenty-two percent" = "22 percent",
    "twenty-three percent" = "23 percent",
    "twenty-four percent" = "24 percent",
    "twenty-five percent" = "25 percent",
    "twenty-six percent" = "26 percent",
    "twenty-seven percent" = "27 percent",
    "twenty-eight percent" = "28 percent",
    "twenty-nine percent" = "29 percent",
    "thirty-one percent" = "31 percent",
    "thirty-two percent" = "32 percent",
    "thirty-three percent" = "33 percent",
    "thirty-four percent" = "34 percent",
    "thirty-five percent" = "35 percent",
    "thirty-six percent" = "36 percent",
    "thirty-seven percent" = "37 percent",
    "thirty-eight percent" = "38 percent",
    "thirty-nine percent" = "39 percent",
    "forty-one percent" = "41 percent",
    "forty-two percent" = "42 percent",
    "forty-three percent" = "43 percent",
    "forty-four percent" = "44 percent",
    "forty-five percent" = "45 percent",
    "forty-six percent" = "46 percent",
    "forty-seven percent" = "47 percent",
    "forty-eight percent" = "48 percent",
    "forty-nine percent" = "49 percent",
    "fifty-one percent" = "51 percent",
    "fifty-two percent" = "52 percent",
    "fifty-three percent" = "53 percent",
    "fifty-four percent" = "54 percent",
    "fifty-five percent" = "55 percent",
    "fifty-six percent" = "56 percent",
    "fifty-seven percent" = "57 percent",
    "fifty-eight percent" = "58 percent",
    "fifty-nine percent" = "59 percent",
    "sixty-one percent" = "61 percent",
    "sixty-two percent" = "62 percent",
    "sixty-three percent" = "63 percent",
    "sixty-four percent" = "64 percent",
    "sixty-five percent" = "65 percent",
    "sixty-six percent" = "66 percent",
    "sixty-seven percent" = "67 percent",
    "sixty-eight percent" = "68 percent",
    "sixty-nine percent" = "69 percent",
    "seventy-one percent" = "71 percent",
    "seventy-two percent" = "72 percent",
    "seventy-three percent" = "73 percent",
    "seventy-four percent" = "74 percent",
    "seventy-five percent" = "75 percent",
    "seventy-six percent" = "76 percent",
    "seventy-seven percent" = "77 percent",
    "seventy-eight percent" = "78 percent",
    "seventy-nine percent" = "79 percent",
    "eighty-one percent" = "81 percent",
    "eighty-two percent" = "82 percent",
    "eighty-three percent" = "83 percent",
    "eighty-four percent" = "84 percent",
    "eighty-five percent" = "85 percent",
    "eighty-six percent" = "86 percent",
    "eighty-seven percent" = "87 percent",
    "eighty-eight percent" = "88 percent",
    "eighty-nine percent" = "89 percent",
    "ninety-one percent" = "91 percent",
    "ninety-two percent" = "92 percent",
    "ninety-three percent" = "93 percent",
    "ninety-four percent" = "94 percent",
    "ninety-five percent" = "95 percent",
    "ninety-six percent" = "96 percent",
    "ninety-seven percent" = "97 percent",
    "ninety-eight percent" = "98 percent",
    "ninety-nine percent" = "99 percent"
  )

  buildMalformedNumberMap = function() {
    unitWords = c(
      "zero" = "0",
      "one" = "1",
      "two" = "2",
      "three" = "3",
      "four" = "4",
      "five" = "5",
      "six" = "6",
      "seven" = "7",
      "eight" = "8",
      "nine" = "9"
    )

    tensWords = c(
      "twenty" = "2",
      "thirty" = "3",
      "forty" = "4",
      "fifty" = "5",
      "sixty" = "6",
      "seventy" = "7",
      "eighty" = "8",
      "ninety" = "9"
    )

    outMap = c()

    for (prefixName in names(unitWords)) {
      for (suffixName in names(unitWords)) {
        outMap[paste0(prefixName, "-", suffixName)] = paste0(unitWords[[prefixName]], unitWords[[suffixName]])
      }

      for (digit in 0:9) {
        outMap[paste0(prefixName, "-", digit)] = paste0(unitWords[[prefixName]], digit)
      }
    }

    for (prefixName in names(tensWords)) {
      for (suffixName in names(unitWords)) {
        outMap[paste0(prefixName, "-", suffixName)] = paste0(tensWords[[prefixName]], unitWords[[suffixName]])
      }

      for (digit in 0:9) {
        outMap[paste0(prefixName, "-", digit)] = paste0(tensWords[[prefixName]], digit)
      }
    }

    outMap
  }

  buildMalformedPercentMap = function() {
    unitWords = c(
      "zero" = "0",
      "one" = "1",
      "two" = "2",
      "three" = "3",
      "four" = "4",
      "five" = "5",
      "six" = "6",
      "seven" = "7",
      "eight" = "8",
      "nine" = "9"
    )

    tensWords = c(
      "twenty" = "2",
      "thirty" = "3",
      "forty" = "4",
      "fifty" = "5",
      "sixty" = "6",
      "seventy" = "7",
      "eighty" = "8",
      "ninety" = "9"
    )

    outMap = c()

    for (prefixName in names(unitWords)) {
      for (suffixName in names(unitWords)) {
        outMap[paste0(prefixName, "-", suffixName, " percent")] = paste0(unitWords[[prefixName]], unitWords[[suffixName]], " percent")
      }

      for (digit in 0:9) {
        outMap[paste0(prefixName, "-", digit, " percent")] = paste0(unitWords[[prefixName]], digit, " percent")
      }
    }

    for (prefixName in names(tensWords)) {
      for (suffixName in names(unitWords)) {
        outMap[paste0(prefixName, "-", suffixName, " percent")] = paste0(tensWords[[prefixName]], unitWords[[suffixName]], " percent")
      }

      for (digit in 0:9) {
        outMap[paste0(prefixName, "-", digit, " percent")] = paste0(tensWords[[prefixName]], digit, " percent")
      }
    }

    outMap
  }

  malformedNumberMap = buildMalformedNumberMap()
  malformedPercentMap = buildMalformedPercentMap()

  applyReplacementMap = function(x, replacementMap) {
    orderedNames = names(replacementMap)[order(nchar(names(replacementMap)), decreasing = TRUE)]

    for (patternName in orderedNames) {
      pattern = paste0("\\b", patternName, "\\b")
      x = gsub(
        pattern,
        unname(replacementMap[[patternName]]),
        x,
        perl = TRUE,
        ignore.case = TRUE
      )
    }

    x
  }

  out = applyReplacementMap(out, decimalMap)
  out = applyReplacementMap(out, malformedPercentMap)
  out = applyReplacementMap(out, malformedNumberMap)
  out = applyReplacementMap(out, percentMap)

  out = gsub("\\*\\*([^*]+)\\*\\*", "\\1", out, perl = TRUE)
  out = gsub("__([^_]+)__", "\\1", out, perl = TRUE)
  out = gsub("(?<![0-9])(-?[0-9]+)\\.0(?![0-9])", "\\1", out, perl = TRUE)

  out
}
#' Normalise one or more WMFM explanations
#'
#' Internal helper that accepts a character vector or a list of character
#' scalars and returns a named character vector suitable for batch grading.
#'
#' @param explanation Character vector or list of character scalars.
#'
#' @return A named character vector.
#' @keywords internal
normaliseWmfmExplanations = function(explanation) {

  if (is.character(explanation)) {
    explanationVec = explanation
  } else if (is.list(explanation)) {
    if (!all(vapply(explanation, function(x) {
      is.character(x) && length(x) == 1 && !is.na(x)
    }, logical(1)))) {
      stop(
        "When `explanation` is a list, each element must be a single non-missing character string.",
        call. = FALSE
      )
    }

    explanationVec = unlist(explanation, use.names = TRUE)
  } else {
    stop(
      "`explanation` must be a character vector or a list of character scalars.",
      call. = FALSE
    )
  }

  if (length(explanationVec) < 1) {
    stop("`explanation` must contain at least one explanation.", call. = FALSE)
  }

  if (anyNA(explanationVec) || any(!nzchar(explanationVec))) {
    stop(
      "All explanations must be non-missing, non-empty character strings.",
      call. = FALSE
    )
  }

  explanationNames = names(explanationVec)
  missingNames = is.null(explanationNames) | !nzchar(explanationNames)

  if (is.null(explanationNames)) {
    explanationNames = rep("", length(explanationVec))
    missingNames = rep(TRUE, length(explanationVec))
  }

  if (any(missingNames)) {
    width = nchar(as.character(length(explanationVec)))
    autoNames = paste0(
      "explanation_",
      formatC(seq_len(length(explanationVec)), width = width, flag = "0")
    )
    explanationNames[missingNames] = autoNames[missingNames]
  }

  explanationNames = make.unique(explanationNames, sep = "_")
  names(explanationVec) = explanationNames

  explanationVec
}
