#' Get the default WMFM claim colour map
#'
#' Returns a named character vector of colours for semantic values used in WMFM
#' raw repeated-run claim-profile heatmaps.
#'
#' The palette is designed for raw extracted claim fields and aims to keep
#' semantically different values visually distinct.
#'
#' @return A named character vector of hexadecimal colours.
#' @examples
#' getWmfmClaimColorMap()
#' @export
getWmfmClaimColorMap = function() {
  c(
    # --- Binary / flags ---
    "TRUE" = "#009E73",
    "FALSE" = "#D55E00",

    # --- Missingness / structural values ---
    "(missing)" = "#FFFFFF",
    "missing" = "#FFFFFF",
    "none" = "#8F8F8F",
    "not_applicable" = "#D7D7D7",
    "not_mentioned" = "#A6A6A6",
    "not_stated" = "#4D4D4D",

    # --- Direction / scale claims ---
    "increase" = "#4DAF4A",
    "decrease" = "#E66101",
    "mixed_or_both" = "#CC79A7",
    "mixed_or_unclear" = "#8073AC",
    "mixed" = "#6A51A3",
    "unclear" = "#984EA3",
    "additive" = "#7CAE00",
    "multiplicative" = "#1F78B4",
    "probability_or_odds" = "#08519C",

    # --- Interaction substantive claims ---
    "difference_claimed" = "#4292C6",
    "difference_claimed_cautiously" = "#6BAED6",
    "difference_claimed_strongly" = "#084594",
    "no_clear_difference" = "#E6AB02",

    # --- Register / uncertainty ---
    "inferential" = "#2171B5",
    "descriptive_only" = "#66A61E",
    "descriptive" = "#66A61E",
    "generic_uncertainty" = "#E6AB02",
    "confidence_interval" = "#084594",

    # --- Legacy ordinal values, kept in case they appear ---
    "0" = "#E41A1C",
    "1" = "#FDBF6F",
    "2" = "#1B9E77"
  )
}
