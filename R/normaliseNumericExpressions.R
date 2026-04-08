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

  replaceCaseAware = function(x, pattern, replacement) {
    out = gsub(pattern, replacement, x, perl = TRUE, ignore.case = TRUE)

    capsPattern = paste0("\\b", toupper(substring(pattern, 3, 3)), substring(pattern, 4))
    out = gsub(capsPattern, replacement, out, perl = TRUE)

    out
  }

  out = text

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

  for (i in seq_along(decimalMap)) {
    pattern = paste0("\\b", names(decimalMap)[i], "\\b")
    out = gsub(pattern, unname(decimalMap[i]), out, perl = TRUE, ignore.case = TRUE)
  }

  for (i in seq_along(percentMap)) {
    pattern = paste0("\\b", names(percentMap)[i], "\\b")
    out = gsub(pattern, unname(percentMap[i]), out, perl = TRUE, ignore.case = TRUE)
  }

  out
}
