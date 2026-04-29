#' Clean generated explanation text before deterministic processing
#'
#' Removes simple LLM formatting artifacts that can leak into the visible
#' explanation, such as leading `Answer`, `Answer:`, or `Answer -` tokens.
#' This is a deterministic surface cleanup step. It does not rewrite the
#' statistical content of the explanation.
#'
#' @param text Character vector of explanation text.
#'
#' @return A character vector with formatting artifacts removed.
#' @export
cleanExplanationText = function(text) {

  if (is.null(text)) {
    return(NULL)
  }

  if (!is.character(text)) {
    stop("`text` must be a character vector or NULL.", call. = FALSE)
  }

  cleaned = text
  keep = !is.na(cleaned)

  cleaned[keep] = gsub(
    pattern = paste0(
      "(^|(?<=[.!?]\\s)|(?<=[.!?]\\n)|(?<=\\n))",
      "[[:space:]]*",
      "(?:[#>*_`~-]+[[:space:]]*)*",
      "answer",
      "(?:[[:space:]]+to[[:space:]]+the[[:space:]]+research[[:space:]]+question)?",
      "\\b",
      "[[:space:]]*(?::|-|--|\\.)?",
      "[[:space:]]*",
      "(?:[#>*_`~-]+[[:space:]]*)*"
    ),
    replacement = "\\1",
    x = cleaned[keep],
    perl = TRUE,
    ignore.case = TRUE
  )

  cleaned[keep] = trimws(cleaned[keep])
  cleaned
}

#' Post-process generated explanation text before display
#'
#' Applies deterministic surface-level cleanup to generated explanation text.
#' This step is deliberately conservative: it removes known formatting
#' artifacts, standardises a small set of recurring pedagogical phrasing
#' problems, and avoids changing numeric values or statistical meaning.
#'
#' @param text Character vector of explanation text.
#' @param audit Optional explanation audit object. Reserved for future
#'   audit-aware cleanup rules.
#' @param debug Logical. If `TRUE`, return the original text, processed text,
#'   and the names of deterministic rules that changed the text.
#'
#' @return A character vector with deterministic surface cleanup applied, or a
#'   list with `original`, `processed`, and `rulesApplied` when `debug = TRUE`.
#' @export
postProcessExplanationText = function(text, audit = NULL, debug = FALSE) {

  if (is.null(text)) {
    if (isTRUE(debug)) {
      return(list(
        original = NULL,
        processed = NULL,
        rulesApplied = character(0)
      ))
    }

    return(NULL)
  }

  if (!is.character(text)) {
    stop("`text` must be a character vector or NULL.", call. = FALSE)
  }

  if (!is.logical(debug) || length(debug) != 1 || is.na(debug)) {
    stop("`debug` must be TRUE or FALSE.", call. = FALSE)
  }

  original = text
  cleaned = cleanExplanationText(text)
  rulesApplied = character(0)

  processed = postProcessApplyRule(
    text = cleaned,
    ruleName = "cleanExplanationText",
    ruleFunction = function(x) {
      x
    },
    rulesApplied = rulesApplied,
    previous = original
  )
  cleaned = processed$text
  rulesApplied = processed$rulesApplied

  processed = postProcessApplyRule(
    text = cleaned,
    ruleName = "unitChangePhrasing",
    ruleFunction = postProcessUnitChangePhrasing,
    rulesApplied = rulesApplied
  )
  cleaned = processed$text
  rulesApplied = processed$rulesApplied

  processed = postProcessApplyRule(
    text = cleaned,
    ruleName = "verbalFractions",
    ruleFunction = postProcessVerbalFractions,
    rulesApplied = rulesApplied
  )
  cleaned = processed$text
  rulesApplied = processed$rulesApplied

  processed = postProcessApplyRule(
    text = cleaned,
    ruleName = "modelMechanismLanguage",
    ruleFunction = postProcessModelMechanismLanguage,
    rulesApplied = rulesApplied
  )
  cleaned = processed$text
  rulesApplied = processed$rulesApplied

  processed = postProcessApplyRule(
    text = cleaned,
    ruleName = "longSentencePatterns",
    ruleFunction = postProcessLongSentencePatterns,
    rulesApplied = rulesApplied
  )
  cleaned = processed$text
  rulesApplied = processed$rulesApplied

  processed = postProcessApplyRule(
    text = cleaned,
    ruleName = "whitespace",
    ruleFunction = postProcessWhitespace,
    rulesApplied = rulesApplied
  )
  cleaned = processed$text
  rulesApplied = processed$rulesApplied

  if (isTRUE(debug)) {
    return(list(
      original = original,
      processed = cleaned,
      rulesApplied = rulesApplied
    ))
  }

  cleaned
}

#' Apply a post-processing rule and record whether it changed text
#'
#' @param text Character vector.
#' @param ruleName Character scalar naming the rule.
#' @param ruleFunction Function that accepts and returns a character vector.
#' @param rulesApplied Character vector of rule names already applied.
#' @param previous Optional previous text to compare against instead of `text`.
#'
#' @return A list with `text` and `rulesApplied`.
#' @keywords internal
postProcessApplyRule = function(text,
                                 ruleName,
                                 ruleFunction,
                                 rulesApplied,
                                 previous = text) {
  changed = ruleFunction(text)

  if (!identical(changed, previous)) {
    rulesApplied = unique(c(rulesApplied, ruleName))
  }

  list(
    text = changed,
    rulesApplied = rulesApplied
  )
}

#' Standardise recurring unit-change phrasing
#'
#' @param text Character vector.
#'
#' @return A character vector with safer unit-change phrasing.
#' @keywords internal
postProcessUnitChangePhrasing = function(text) {
  text = gsub(
    pattern = "\\b[Aa] one-magnitude rise multiplies the ([^.]+?) by\\b",
    replacement = "If the magnitude increases by one, the \\1 is multiplied by",
    x = text,
    perl = TRUE
  )

  text = gsub(
    pattern = "\\b[Aa] one-magnitude increase multiplies the ([^.]+?) by\\b",
    replacement = "If the magnitude increases by one, the \\1 is multiplied by",
    x = text,
    perl = TRUE
  )

  text = gsub(
    pattern = "\\b[Aa] one-magnitude rise\\b",
    replacement = "If the magnitude increases by one,",
    x = text,
    perl = TRUE
  )

  text = gsub(
    pattern = "\\b[Oo]ne-magnitude rise\\b",
    replacement = "an increase of one in magnitude",
    x = text,
    perl = TRUE
  )

  text = gsub(
    pattern = "\\b[Aa] one-magnitude increase\\b",
    replacement = "If the magnitude increases by one,",
    x = text,
    perl = TRUE
  )

  text = gsub(
    pattern = "\\b[Oo]ne-magnitude increase\\b",
    replacement = "an increase of one in magnitude",
    x = text,
    perl = TRUE
  )

  text = gsub(
    pattern = "\\b[Aa] one-unit increase in ([[:alnum:]_.]+) multiplies the ([^.]+?) by\\b",
    replacement = "If \\1 increases by one unit, the \\2 is multiplied by",
    x = text,
    perl = TRUE
  )

  text = gsub(
    pattern = "\\b[Aa] one-unit rise in ([[:alnum:]_.]+) multiplies the ([^.]+?) by\\b",
    replacement = "If \\1 increases by one unit, the \\2 is multiplied by",
    x = text,
    perl = TRUE
  )

  text = gsub(
    pattern = "\\b[Ff]or a one-unit increase in ([[:alnum:]_.]+),",
    replacement = "If \\1 increases by one unit,",
    x = text,
    perl = TRUE
  )

  text = gsub(
    pattern = "\\b[Ff]or a one-unit rise in ([[:alnum:]_.]+),",
    replacement = "If \\1 increases by one unit,",
    x = text,
    perl = TRUE
  )

  text = gsub(
    pattern = "\\b[Aa] one-unit increase in ([[:alnum:]_.]+)\\b",
    replacement = "If \\1 increases by one unit,",
    x = text,
    perl = TRUE
  )

  text = gsub(
    pattern = "\\b[Aa] one-unit rise in ([[:alnum:]_.]+)\\b",
    replacement = "If \\1 increases by one unit,",
    x = text,
    perl = TRUE
  )

  text = gsub(
    pattern = "\\b[Ff]or a one-unit increase in ([[:alnum:]_.]+),",
    replacement = "If \\1 increases by one unit,",
    x = text,
    perl = TRUE
  )

  text = gsub(
    pattern = "\\b[Oo]ne-unit increase in ([[:alnum:]_.]+)\\b",
    replacement = "an increase of one unit in \\1",
    x = text,
    perl = TRUE
  )

  text = gsub(
    pattern = "\\b[Oo]ne-unit rise in ([[:alnum:]_.]+)\\b",
    replacement = "an increase of one unit in \\1",
    x = text,
    perl = TRUE
  )

  text
}

#' Replace verbal fractions in numeric contexts
#'
#' @param text Character vector.
#'
#' @return A character vector with verbal fractions replaced by percentages.
#' @keywords internal
postProcessVerbalFractions = function(text) {
  replacements = c(
    "one-third" = "about 33%",
    "two-thirds" = "about 67%",
    "three-quarters" = "about 75%",
    "one-quarter" = "about 25%",
    "a quarter" = "about 25%",
    "one half" = "about 50%",
    "one-half" = "about 50%",
    "a half" = "about 50%"
  )

  for (patternText in names(replacements)) {
    text = gsub(
      pattern = paste0("\\b", patternText, "\\b"),
      replacement = replacements[[patternText]],
      x = text,
      perl = TRUE,
      ignore.case = TRUE
    )
  }

  text
}

#' Remove a small set of model-mechanism phrasings
#'
#' @param text Character vector.
#' @return A character vector with recurring technical leakage reduced.
#' @keywords internal
postProcessModelMechanismLanguage = function(text) {
  text = gsub(
    pattern = "\\b[Tt]he interaction term makes the ([^.]+?) steeper in ([^.]+?) than in ([^.]+?)\\.",
    replacement = "The \\1 is steeper in \\2 than in \\3.",
    x = text,
    perl = TRUE
  )

  text = gsub(
    pattern = "\\b[Tt]he interaction term makes the ([^.]+?) steeper in ([^.]+?)\\.",
    replacement = "The \\1 is steeper in \\2.",
    x = text,
    perl = TRUE
  )

  text = gsub(
    pattern = "\\b[Tt]he interaction term shows that\\b",
    replacement = "The data suggest that",
    x = text,
    perl = TRUE
  )

  text = gsub(
    pattern = "\\b[Tt]he coefficient for ([[:alnum:]_.]+) shows that\\b",
    replacement = "The results suggest that \\1",
    x = text,
    perl = TRUE
  )

  text = gsub(
    pattern = "\\b[Tt]he slope for ([[:alnum:]_.]+) shows that\\b",
    replacement = "The results suggest that \\1",
    x = text,
    perl = TRUE
  )

  text = gsub(
    pattern = "\\b[Ii]nteraction term\\b",
    replacement = "combined pattern",
    x = text,
    perl = TRUE
  )

  text = gsub(
    pattern = "\\b[Cc]oefficient\\b",
    replacement = "estimated change",
    x = text,
    perl = TRUE
  )

  text = gsub(
    pattern = "\\b[Ii]ntercept\\b",
    replacement = "starting estimate",
    x = text,
    perl = TRUE
  )

  text = gsub(
    pattern = "\\b[Ss]lope\\b",
    replacement = "change pattern",
    x = text,
    perl = TRUE
  )

  text = gsub(
    pattern = "\\b[Pp]arameter\\b",
    replacement = "estimated quantity",
    x = text,
    perl = TRUE
  )

  text = gsub(
    pattern = "\\b[Ll]inear model\\b",
    replacement = "model",
    x = text,
    perl = TRUE
  )

  text = gsub(
    pattern = "\\b[Rr]egression model\\b",
    replacement = "model",
    x = text,
    perl = TRUE
  )

  text = gsub(
    pattern = "\\b[Ff]itted model\\b",
    replacement = "model",
    x = text,
    perl = TRUE
  )

  text
}

#' Split recurring long sentence patterns without changing values
#'
#' @param text Character vector.
#' @return A character vector with a few long sentence patterns split.
#' @keywords internal
postProcessLongSentencePatterns = function(text) {
  numberPattern = "[-+]?[0-9]+(?:\\.[0-9]+)?(?:e[-+]?[0-9]+)?%?"

  text = gsub(
    pattern = paste0(", with values between (", numberPattern, ") and (", numberPattern, ")\\."),
    replacement = ". This estimate could plausibly lie between \\1 and \\2.",
    x = text,
    perl = TRUE,
    ignore.case = TRUE
  )

  text = gsub(
    pattern = paste0(", with plausible values between (", numberPattern, ") and (", numberPattern, ")\\."),
    replacement = ". This estimate could plausibly lie between \\1 and \\2.",
    x = text,
    perl = TRUE,
    ignore.case = TRUE
  )

  text = gsub(
    pattern = paste0(", with limits from (", numberPattern, ") to (", numberPattern, ")\\."),
    replacement = ". The confidence limits run from \\1 to \\2.",
    x = text,
    perl = TRUE,
    ignore.case = TRUE
  )

  text = gsub(
    pattern = paste0(", with limits between (", numberPattern, ") and (", numberPattern, ")\\."),
    replacement = ". The confidence limits run from \\1 to \\2.",
    x = text,
    perl = TRUE,
    ignore.case = TRUE
  )

  text = gsub(
    pattern = paste0(", with a 95% confidence interval from (", numberPattern, ") to (", numberPattern, ")\\."),
    replacement = ". The 95% confidence interval runs from \\1 to \\2.",
    x = text,
    perl = TRUE,
    ignore.case = TRUE
  )

  text = gsub(
    pattern = paste0(", with a 95% confidence interval between (", numberPattern, ") and (", numberPattern, ")\\."),
    replacement = ". The 95% confidence interval runs from \\1 to \\2.",
    x = text,
    perl = TRUE,
    ignore.case = TRUE
  )

  text = gsub(
    pattern = ";[[:space:]]+",
    replacement = ". ",
    x = text,
    perl = TRUE
  )

  text
}

#' Normalise whitespace after deterministic post-processing
#'
#' @param text Character vector.
#' @return A character vector with repeated whitespace cleaned up.
#' @keywords internal
postProcessWhitespace = function(text) {
  text = gsub("[[:space:]]+([.,;:!?])", "\\1", text, perl = TRUE)
  text = gsub("([.!?])[[:space:]]+([.!?])", "\\1", text, perl = TRUE)
  text = gsub("[[:space:]]{2,}", " ", text, perl = TRUE)
  trimws(text)
}
