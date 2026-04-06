#' Get the default WMFM claim colour map
#'
#' Returns a named character vector of colours for semantic values used in raw
#' `wmfmRuns` claim-profile heatmaps.
#'
#' The palette is designed for the current raw-only repeated-run workflow. It
#' aims to keep broad semantic groups visually distinct while avoiding overly
#' similar shades within the same heatmap:
#' 
#' \itemize{
#'   \item binary presence or absence values use contrasting teal and orange,
#'   \item directional or scale claims use clearly separated green, red-orange,
#'   blue, and purple hues,
#'   \item inferential and uncertainty-related values use distinct blues and
#'   mustard tones,
#'   \item structural values such as `none`, `not_stated`, and
#'   `not_applicable` use separated greys, and
#'   \item true missingness uses white and is intended to be overlaid with an X
#'   in the heatmap.
#' }
#'
#' The returned vector can be used directly as a lookup table before assigning
#' fallback colours to previously unseen values.
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
        "Present" = "#1B9E77",
        "Absent" = "#D95F02",

        # --- Missingness / structural values ---
        "(missing)" = "#FFFFFF",
        "missing" = "#FFFFFF",
        "none" = "#8F8F8F",
        "not_stated" = "#5F5F5F",
        "not_mentioned" = "#A8A8A8",
        "not_applicable" = "#D7D7D7",

        # --- Direction / scale claims ---
        "increase" = "#2CA25F",
        "decrease" = "#E34A33",
        "mixed_or_both" = "#CC79A7",
        "mixed_or_unclear" = "#7B61A8",
        "mixed" = "#6A51A3",
        "unclear" = "#8C6BB1",
        "additive" = "#66A61E",
        "multiplicative" = "#1F78B4",
        "probability_or_odds" = "#08519C",

        # --- Interaction substantive claims ---
        "difference_claimed_cautiously" = "#6BAED6",
        "difference_claimed_strongly" = "#084594",
        "no_clear_difference" = "#E6AB02",

        # --- Register / uncertainty ---
        "inferential" = "#2171B5",
        "descriptive_only" = "#31A354",
        "descriptive" = "#31A354",
        "generic_uncertainty" = "#E6AB02",
        "confidence_interval" = "#084594"
    )
}
