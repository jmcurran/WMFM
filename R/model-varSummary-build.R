#' Build a per-variable summary for a data frame
#'
#' Creates a compact summary of each column in a data frame, intended for
#' display in a Shiny "data help" panel. For each variable, the summary
#' includes its name, class, number of missing values, and a short "details"
#' string:
#' \itemize{
#'   \item Factors: levels (up to \code{maxLevels})
#'   \item Numeric/integer: range (min to max) computed on non-missing values
#'   \item Character/logical: a small sample of unique non-missing values
#'     (up to \code{maxUnique})
#'   \item Other types: a brief note based on class
#' }
#'
#' @param df A data frame.
#' @param maxLevels Maximum number of factor levels to display in the details
#'   string before truncating with an ellipsis.
#' @param maxUnique Maximum number of unique values to display for character
#'   or logical variables before truncating with an ellipsis.
#'
#' @return A data frame with one row per column of \code{df} and columns:
#' \describe{
#'   \item{\code{var}}{Variable name.}
#'   \item{\code{class}}{First class label (e.g., \code{"numeric"}, \code{"factor"}).}
#'   \item{\code{missing}}{Count of \code{NA} values.}
#'   \item{\code{details}}{A short human-readable summary string.}
#' }
#'
#' @examples
#' df = data.frame(
#'   a = factor(c("L1", "L2", "L1")),
#'   b = c(1, NA, 3),
#'   c = c("x", "y", "x"),
#'   stringsAsFactors = FALSE
#' )
#'
#' WMFM:::buildVarSummary(df)
#'
#' @keywords internal
buildVarSummary = function(df, maxLevels = 30, maxUnique = 12) {

  if (!is.data.frame(df)) {
    stop("df must be a data frame.")
  }

  if (!is.numeric(maxLevels) || length(maxLevels) != 1 || maxLevels < 1) {
    stop("maxLevels must be a single number >= 1.")
  }

  if (!is.numeric(maxUnique) || length(maxUnique) != 1 || maxUnique < 1) {
    stop("maxUnique must be a single number >= 1.")
  }

  fmt = function(x) {
    format(x, trim = TRUE, scientific = FALSE)
  }

  buildDetails = function(x) {

    xNoNa = x[!is.na(x)]

    if (is.factor(x)) {

      levs = levels(x)

      shown = head(levs, maxLevels)
      details = paste(shown, collapse = ", ")

      if (length(levs) > maxLevels) {
        details = paste0(details, ", ...")
      }

      return(paste0("Levels: ", details))
    }

    if (is.numeric(x) || is.integer(x)) {

      if (length(xNoNa) == 0) {
        return("Range: (all missing)")
      }

      rng = range(xNoNa)
      return(paste0("Range: ", fmt(rng[1]), " to ", fmt(rng[2])))
    }

    if (is.logical(x) || is.character(x)) {

      if (length(xNoNa) == 0) {
        return("Values: (all missing)")
      }

      uniq = unique(xNoNa)
      uniq = uniq[order(as.character(uniq))]

      shown = head(uniq, maxUnique)
      details = paste(fmt(shown), collapse = ", ")

      if (length(uniq) > maxUnique) {
        details = paste0(details, ", ...")
      }

      return(paste0("Values: ", details))
    }

    # Fallback for other types (e.g., Date, POSIXct, list-cols)
    cls = class(x)
    if (length(cls) == 0) {
      cls = "unknown"
    } else {
      cls = cls[1]
    }

    paste0("Type: ", cls)
  }

  vars = names(df)

  out = data.frame(
    var = vars,
    class = character(length(vars)),
    missing = integer(length(vars)),
    details = character(length(vars)),
    stringsAsFactors = FALSE
  )

  for (i in seq_along(vars)) {

    v = vars[i]
    x = df[[v]]

    cls = class(x)
    out$class[i] = if (length(cls) > 0) cls[1] else "unknown"
    out$missing[i] = sum(is.na(x))
    out$details[i] = buildDetails(x)
  }

  out
}
