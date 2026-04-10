#' Make deterministic category colours for WMFM heatmaps
#'
#' Generates a stable, perceptually stronger mapping from category values to
#' colours using only base R / grDevices. The same category value always maps
#' to the same colour, regardless of the order in which values appear.
#'
#' Colours are generated in HCL space using a simple deterministic string hash.
#' This gives better separation than short fixed palettes, especially when many
#' distinct values are present.
#'
#' @param values A vector of category values.
#' @param naLabel Character label used for missing values after coercion.
#' @return A named character vector of colours, where names are category values.
#' @examples
#' makeWmfmDeterministicCategoryColors(c("yes", "no", "mixed", "(missing)"))
#' @export
makeWmfmDeterministicCategoryColors = function(values,
                                               naLabel = "(missing)") {

  values = as.character(values)
  uniqueValues = sort(unique(values))

  if (length(uniqueValues) == 0) {
    return(stats::setNames(character(0), character(0)))
  }

  hashString = function(x) {
    ints = utf8ToInt(enc2utf8(x))
    hash = 2166136261

    for (i in seq_along(ints)) {
      hash = bitwXor(hash, ints[i])
      hash = (hash * 16777619) %% 2147483647
    }

    as.integer(abs(hash))
  }

  makeColour = function(x) {
    if (identical(x, naLabel)) {
      return("#D9D9D9")
    }

    hash = hashString(x)

    hue = (hash %% 360) + (((hash %/% 360) %% 1000) / 1000)
    chromaOptions = c(55, 70, 85)
    luminanceOptions = c(42, 55, 68)

    chroma = chromaOptions[(hash %% length(chromaOptions)) + 1]
    luminance = luminanceOptions[((hash %/% 7) %% length(luminanceOptions)) + 1]

    grDevices::hcl(
      h = hue,
      c = chroma,
      l = luminance,
      fixup = TRUE
    )
  }

  colours = vapply(uniqueValues, makeColour, character(1), USE.NAMES = FALSE)

  duplicatedColours = duplicated(colours) | duplicated(colours, fromLast = TRUE)

  if (any(duplicatedColours)) {
    dupIdx = which(duplicatedColours)

    for (k in seq_along(dupIdx)) {
      i = dupIdx[k]

      if (identical(uniqueValues[i], naLabel)) {
        next
      }

      hash = hashString(uniqueValues[i]) + (k * 137)
      hue = (hash %% 360) + 0.5
      chromaOptions = c(55, 70, 85)
      luminanceOptions = c(42, 55, 68)

      chroma = chromaOptions[(hash %% length(chromaOptions)) + 1]
      luminance = luminanceOptions[((hash %/% 7) %% length(luminanceOptions)) + 1]

      colours[i] = grDevices::hcl(
        h = hue,
        c = chroma,
        l = luminance,
        fixup = TRUE
      )
    }
  }

  stats::setNames(colours, uniqueValues)
}
