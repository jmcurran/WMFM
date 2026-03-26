#' Get the default WMFM claim colour map
#'
#' Returns a named character vector of colours for common semantic claim values
#' used in WMFM explanation heatmaps.
#'
#' The mapping is intentionally semantic rather than purely algorithmic. Common
#' values such as `TRUE`, `FALSE`, `appropriate`, `inferential`, and `unclear`
#' are assigned stable colours that are easier to interpret across plots.
#'
#' The palette is designed so that:
#' \itemize{
#'   \item green/teal tones indicate presence or appropriate claims,
#'   \item blue indicates inferential style,
#'   \item orange/red indicates absence, weakness, or problematic claims,
#'   \item purple/magenta indicates ambiguity or mixed claims, and
#'   \item grey indicates missing or not-mentioned values.
#' }
#'
#' The returned vector can be used directly, or as a first-pass lookup before a
#' fallback palette is used for previously unseen values.
#'
#' @return A named character vector of hexadecimal colours.
#' @examples
#' getWmfmClaimColorMap()
#' @export
getWmfmClaimColorMap = function() {
  c(
    # --- Presence (binary) ---
    "TRUE" = "#1B9E77",        # teal-green
    "FALSE" = "#D95F02",       # orange

    # --- Claim strength (use BLUE scale, not green) ---
    "difference_claimed" = "#4C78A8",          # medium blue
    "difference_claimed_strongly" = "#1F3A5F", # dark navy

    # --- Quality judgement ---
    "appropriate" = "#1B9E77",   # green
    "too_weak" = "#E6AB02",      # yellow
    "overclaim" = "#E41A1C",     # red
    "overclaimed" = "#E41A1C",

    # --- Ambiguity ---
    "unclear" = "#7570B3",            # purple
    "mixed_or_unclear" = "#7570B3",
    "mixed_or_both" = "#CC79A7",      # magenta

    # --- Structural / style ---
    "inferential" = "#1F78B4",        # blue
    "descriptive" = "#66A61E",

    # --- Missing ---
    "not_mentioned" = "#999999",
    "(missing)" = "#D9D9D9"
  )
}
