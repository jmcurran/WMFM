#' Build deterministic quality flags for a mapped explanation sentence
#'
#' Applies lightweight deterministic checks to a visible sentence. These flags are
#' intended for developer feedback and validation. They do not change the
#' explanation text and they do not replace the legacy claim-tag fields.
#'
#' @param claimText Character scalar.
#' @param roles Character vector of Stage 9 sentence roles.
#'
#' @return Character vector of quality-flag identifiers.
#'
#' @keywords internal
buildExplanationDeterministicQualityFlags = function(claimText, roles = character(0)) {

  text = trimws(as.character(claimText %||% ""))
  roles = normaliseExplanationSentenceRoles(roles)

  flags = c(
    if (detectExplanationScaleLeakage(text)) "technicalScaleLeakage",
    if (detectExplanationRawCoefficientLeakage(text)) "rawCoefficientShown",
    if (detectExplanationInteractionTermLeakage(text)) "interactionTermMentioned",
    if (detectExplanationExcessiveDecimalPlaces(text)) "excessiveDecimalPlaces",
    if (detectExplanationOddsShownAsPlainDecimal(text)) "oddsShownAsDecimal",
    if (detectExplanationEffectWithoutChangeLanguage(text, roles)) "effectWithoutChangeLanguage"
  )

  flags = unique(stats::na.omit(flags))

  if (length(flags) == 0L) {
    return(character(0))
  }

  as.character(flags)
}

#' Build deterministic quality flags for a claim-evidence map
#'
#' These map-level flags summarize omissions or structural problems across all
#' mapped explanation sentences. They are deliberately conservative so Stage 9.6
#' adds developer-facing diagnostics without changing existing tagging behavior.
#'
#' @param claimsTable Claim mapping data frame.
#'
#' @return Character vector of quality-flag identifiers.
#'
#' @keywords internal
buildExplanationMapQualityFlags = function(claimsTable) {

  if (!is.data.frame(claimsTable) || nrow(claimsTable) == 0) {
    return(c("missingAnswer"))
  }

  rolesList = getExplanationClaimsRolesList(claimsTable)
  text = paste(as.character(claimsTable$claimText %||% character(0)), collapse = " ")

  flags = c(
    if (!any(vapply(rolesList, function(roles) {
      "answer" %in% roles
    }, logical(1)))) "missingAnswer",
    if (detectExplanationScaleLeakage(text)) "technicalScaleLeakage",
    if (detectExplanationRawCoefficientLeakage(text)) "rawCoefficientShown",
    if (detectExplanationInteractionTermLeakage(text)) "interactionTermMentioned",
    if (detectExplanationExcessiveComparisons(claimsTable, rolesList)) "excessiveComparisons",
    if (detectExplanationInteractionNotCompared(claimsTable, rolesList)) "interactionNotCompared"
  )

  flags = unique(stats::na.omit(flags))

  if (length(flags) == 0L) {
    return(character(0))
  }

  as.character(flags)
}

getExplanationClaimsRolesList = function(claimsTable) {

  if (!"roles" %in% names(claimsTable)) {
    return(vector("list", nrow(claimsTable)))
  }

  lapply(claimsTable$roles, normaliseExplanationSentenceRoles)
}

detectExplanationScaleLeakage = function(text) {

  text = tolower(as.character(text %||% ""))

  grepl(
    paste(
      c(
        "log[- ]odds",
        "log expected count",
        "log count",
        "log-rate",
        "log rate",
        "link scale",
        "linear predictor"
      ),
      collapse = "|"
    ),
    text,
    perl = TRUE
  )
}

detectExplanationRawCoefficientLeakage = function(text) {

  text = tolower(as.character(text %||% ""))

  grepl(
    paste(
      c(
        "\\bcoefficient\\b",
        "\\bcoefficients\\b",
        "\\bintercept\\b",
        "exponentiating",
        "exponentiate",
        "\\bbeta[_ ]?[0-9]"
      ),
      collapse = "|"
    ),
    text,
    perl = TRUE
  )
}

detectExplanationInteractionTermLeakage = function(text) {

  text = tolower(as.character(text %||% ""))
  grepl("\\binteraction term\\b", text, perl = TRUE)
}

detectExplanationExcessiveDecimalPlaces = function(text) {

  text = as.character(text %||% "")
  grepl("(?<![A-Za-z0-9])[-+]?[0-9]+\\.[0-9]{4,}(?![A-Za-z0-9])", text, perl = TRUE)
}

detectExplanationOddsShownAsPlainDecimal = function(text) {

  text = tolower(as.character(text %||% ""))

  if (!grepl("\\bodds\\b", text, perl = TRUE)) {
    return(FALSE)
  }

  hasRatio = grepl("[0-9]+(?:\\.[0-9]+)?\\s*:\\s*[0-9]+(?:\\.[0-9]+)?", text, perl = TRUE)
  hasDecimal = grepl("(?<![A-Za-z0-9])0\\.[0-9]+(?![A-Za-z0-9])", text, perl = TRUE)

  isTRUE(hasDecimal && !hasRatio)
}

detectExplanationEffectWithoutChangeLanguage = function(text, roles) {

  roles = normaliseExplanationSentenceRoles(roles)

  if (!"effect" %in% roles) {
    return(FALSE)
  }

  text = tolower(as.character(text %||% ""))

  if (!grepl("\\beffect of\\b", text, perl = TRUE)) {
    return(FALSE)
  }

  !grepl(
    paste(
      c(
        "change in",
        "increase in",
        "decrease in",
        "one[- ]unit",
        "per unit",
        "compared with",
        "compared to",
        "than"
      ),
      collapse = "|"
    ),
    text,
    perl = TRUE
  )
}

detectExplanationExcessiveComparisons = function(claimsTable, rolesList) {

  comparisonCount = sum(vapply(rolesList, function(roles) {
    "comparison" %in% roles
  }, logical(1)))

  comparisonCount > 4L
}

detectExplanationInteractionNotCompared = function(claimsTable, rolesList) {

  text = tolower(paste(as.character(claimsTable$claimText %||% character(0)), collapse = " "))
  mentionsInteraction = grepl("interaction|different relationship|effect .* different|changes depending", text, perl = TRUE)

  if (!mentionsInteraction) {
    return(FALSE)
  }

  hasComparison = any(vapply(rolesList, function(roles) {
    "comparison" %in% roles
  }, logical(1)))

  !hasComparison
}
