#' Get the default WMFM claim colour map
#'
#' Returns a named character vector of colours for common semantic values used
#' in WMFM repeated-run heatmaps.
#'
#' The mapping is designed for the revised repeated-run evaluation schema and is
#' intended to work across three broad groups of fields:
#'
#' \itemize{
#'   \item extracted claim fields such as `effectDirectionClaim` and
#'   `interactionSubstantiveClaim`,
#'   \item judged quality fields such as `interactionEvidenceAppropriate` and
#'   `clarityAdequate`, and
#'   \item aggregate flags such as `fatalFlawDetected` and `overallPass`.
#' }
#'
#' The palette is semantic rather than purely algorithmic. In broad terms:
#' \itemize{
#'   \item green/teal tones indicate appropriate, adequate, present, or pass,
#'   \item blue tones indicate substantive interaction or inferential language,
#'   \item orange tones indicate weakness, partial adequacy, or caution,
#'   \item red tones indicate problematic, incorrect, or overstrong claims,
#'   \item purple/magenta tones indicate ambiguity or mixed content, and
#'   \item grey tones indicate absence, missingness, or not-applicable values.
#' }
#'
#' The returned vector can be used directly as a lookup table before assigning
#' deterministic fallback colours to previously unseen values.
#'
#' @return A named character vector of hexadecimal colours.
#' @examples
#' getWmfmClaimColorMap()
#' @export
getWmfmClaimColorMap = function() {
  c(
    # --- Binary / flags ---
    "TRUE" = "#1B9E77",
    "FALSE" = "#D95F02",
    "fatal" = "#B22222",
    "pass" = "#1B9E77",
    "fail" = "#B22222",

    # --- Generic missingness / structural values ---
    "(missing)" = "#D9D9D9",
    "missing" = "#D9D9D9",
    "not_mentioned" = "#999999",
    "not_stated" = "#999999",
    "not_applicable" = "#C7C7C7",
    "none" = "#BDBDBD",

    # --- Direction / scale claims ---
    "increase" = "#1B9E77",
    "decrease" = "#D95F02",
    "mixed_or_both" = "#CC79A7",
    "mixed_or_unclear" = "#7570B3",
    "mixed" = "#7570B3",
    "unclear" = "#7570B3",
    "additive" = "#66A61E",
    "multiplicative" = "#1F78B4",
    "probability_or_odds" = "#4C78A8",

    # --- Interaction substantive claims ---
    "difference_claimed" = "#4C78A8",
    "difference_claimed_cautiously" = "#6BAED6",
    "difference_claimed_strongly" = "#1F3A5F",
    "no_clear_difference" = "#E6AB02",

    # --- Inferential / evidential judgements ---
    "appropriate" = "#1B9E77",
    "adequate" = "#1B9E77",
    "too_weak" = "#E6AB02",
    "too_strong" = "#E41A1C",
    "overclaim" = "#E41A1C",
    "overclaimed" = "#E41A1C",
    "underclaim" = "#F0A202",
    "underclaimed" = "#F0A202",

    # --- Register / uncertainty ---
    "inferential" = "#1F78B4",
    "descriptive_only" = "#66A61E",
    "descriptive" = "#66A61E",
    "generic_uncertainty" = "#E6AB02",
    "confidence_interval" = "#1F78B4",

    # --- Ordinal judged fields ---
    "0" = "#E41A1C",
    "1" = "#FDBF6F",
    "2" = "#1B9E77"
  )
}
