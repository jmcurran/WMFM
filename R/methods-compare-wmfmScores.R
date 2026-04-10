#' Compare WMFM score results
#'
#' Compares score results either:
#' \itemize{
#'   \item within a single `wmfmScores` object containing two methods, or
#'   \item between two `wmfmScores` objects.
#' }
#'
#' The comparison summarizes agreement separately for binary, ordinal,
#' and continuous score fields using the WMFM metric registry.
#'
#' @param x A `wmfmScores` object.
#' @param y Optional second `wmfmScores` object.
#' @param xMethod Optional character string naming the method to use from `x`.
#' @param yMethod Optional character string naming the method to use from `y`.
#' @param registry Optional WMFM metric registry. Defaults to
#'   `getWmfmMetricRegistry()`.
#' @param ... Unused. Included for S3 compatibility.
#'
#' @return An object of class `wmfmScoreComparison`.
#' @export
compare.wmfmScores = function(
  x,
  y = NULL,
  xMethod = NULL,
  yMethod = NULL,
  registry = getWmfmMetricRegistry(),
  ...
) {
  if (!inherits(x, "wmfmScores")) {
    stop("`x` must inherit from `wmfmScores`.", call. = FALSE)
  }

  validateWmfmMetricRegistry(registry)

  getAvailableMethods = function(obj) {
    methods = obj$methods

    if (is.null(methods)) {
      return(character(0))
    }

    as.character(methods)
  }

  getSingleMethod = function(obj, method = NULL) {
    available = getAvailableMethods(obj)

    if (length(available) == 0) {
      stop("No scoring methods are available.", call. = FALSE)
    }

    if (!is.null(method)) {
      if (!method %in% available) {
        stop("Requested method not present.", call. = FALSE)
      }

      return(method)
    }

    if (length(available) != 1) {
      stop("Ambiguous method selection.", call. = FALSE)
    }

    available[1]
  }

  getWithinObjectMethods = function(obj, xMethod = NULL, yMethod = NULL) {
    available = getAvailableMethods(obj)

    if (length(available) < 2) {
      stop(
        "A within-object comparison requires at least two methods.",
        call. = FALSE
      )
    }

    if (!is.null(xMethod) && !xMethod %in% available) {
      stop("Requested `xMethod` not present in `x`.", call. = FALSE)
    }

    if (!is.null(yMethod) && !yMethod %in% available) {
      stop("Requested `yMethod` not present in `x`.", call. = FALSE)
    }

    if (!is.null(xMethod) && !is.null(yMethod)) {
      if (identical(xMethod, yMethod)) {
        stop("`xMethod` and `yMethod` must be different.", call. = FALSE)
      }

      return(c(xMethod, yMethod))
    }

    preferred = c("deterministic", "llm")

    if (all(preferred %in% available)) {
      return(preferred)
    }

    if (!is.null(xMethod)) {
      remaining = setdiff(available, xMethod)
      return(c(xMethod, remaining[1]))
    }

    if (!is.null(yMethod)) {
      remaining = setdiff(available, yMethod)
      return(c(remaining[1], yMethod))
    }

    available[1:2]
  }

  scoreDfForMethod = function(obj, method) {
    df = as.data.frame(obj, format = "long")
    df = df[df$method == method, , drop = FALSE]

    if (nrow(df) == 0) {
      stop("No rows for requested method.", call. = FALSE)
    }

    rownames(df) = NULL
    df
  }

  if (is.null(y)) {
    methods = getWithinObjectMethods(
      obj = x,
      xMethod = xMethod,
      yMethod = yMethod
    )

    leftMethod = methods[1]
    rightMethod = methods[2]

    return(
      buildWmfmScoreComparison(
        leftDf = scoreDfForMethod(x, leftMethod),
        rightDf = scoreDfForMethod(x, rightMethod),
        leftMethod = leftMethod,
        rightMethod = rightMethod,
        sourceLabel = "within_object",
        registry = registry
      )
    )
  }

  if (!inherits(y, "wmfmScores")) {
    stop("`y` must inherit from `wmfmScores`.", call. = FALSE)
  }

  leftMethod = getSingleMethod(x, xMethod)
  rightMethod = getSingleMethod(y, yMethod)

  buildWmfmScoreComparison(
    leftDf = scoreDfForMethod(x, leftMethod),
    rightDf = scoreDfForMethod(y, rightMethod),
    leftMethod = leftMethod,
    rightMethod = rightMethod,
    sourceLabel = "between_objects",
    registry = registry
  )
}
