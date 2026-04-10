#' Make readable WMFM legend labels
#'
#' Converts raw semantic claim values into nicer display labels for plot
#' legends.
#'
#' @param values Character vector of raw legend values.
#'
#' @return A character vector of display labels.
#' @examples
#' makeWmfmLegendLabels(c("TRUE", "mixed_or_unclear", "(missing)", ""))
#' @export
makeWmfmLegendLabels = function(values) {
  labels = values

  labels[labels == ""] = " "
  labels = gsub("_", " ", labels, fixed = TRUE)
  labels = ifelse(labels == "TRUE", "Present", labels)
  labels = ifelse(labels == "FALSE", "Absent", labels)
  labels = ifelse(labels == "(missing)", "Missing", labels)
  labels
}
