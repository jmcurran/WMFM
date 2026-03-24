#' Format elapsed time for WMFM progress messages
#'
#' Converts elapsed time in seconds to a compact human-readable string for
#' console progress reporting.
#'
#' @param seconds Numeric. Elapsed time in seconds.
#'
#' @return Character string such as `"12s"`, `"3m 08s"`, or `"1h 02m 15s"`.
#'
#' @export
formatWmfmElapsedTime = function(seconds) {

  if (length(seconds) != 1 || is.na(seconds) || !is.finite(seconds) || seconds < 0) {
    return(NA_character_)
  }

  totalSeconds = as.integer(round(seconds))

  hours = totalSeconds %/% 3600
  minutes = (totalSeconds %% 3600) %/% 60
  secs = totalSeconds %% 60

  if (hours > 0) {
    return(sprintf("%dh %02dm %02ds", hours, minutes, secs))
  }

  if (minutes > 0) {
    return(sprintf("%dm %02ds", minutes, secs))
  }

  sprintf("%ds", secs)
}
