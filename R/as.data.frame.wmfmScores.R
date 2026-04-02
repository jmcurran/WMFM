#' Coerce a WMFM scores object to a data frame
#'
#' Flattens a `wmfmScores` object into a long or wide data frame for analysis,
#' plotting, and comparison.
#'
#' @param x A `wmfmScores` object.
#' @param row.names Ignored. Included for S3 compatibility.
#' @param optional Ignored. Included for S3 compatibility.
#' @param format Character. One of `"long"` or `"wide"`.
#' @param ... Unused. Included for S3 compatibility.
#'
#' @return A data frame.
#' @export
as.data.frame.wmfmScores = function(
    x,
    row.names = NULL,
    optional = FALSE,
    format = c("long", "wide"),
    ...
) {

  format = match.arg(format)

  if (!inherits(x, "wmfmScores")) {
    stop("`x` must inherit from `wmfmScores`.", call. = FALSE)
  }

  methods = x$methods %||% character(0)
  runIds = x$runIds %||% integer(0)

  if (length(methods) == 0 || length(runIds) == 0) {
    return(data.frame())
  }

  longPieces = list()
  pieceIndex = 0L

  for (method in methods) {
    methodScores = x$scores[[method]]

    if (is.null(methodScores)) {
      next
    }

    if (length(methodScores) != length(runIds)) {
      stop(
        "Scores stored for method `", method,
        "` are not aligned with `runIds`.",
        call. = FALSE
      )
    }

    for (i in seq_along(runIds)) {
      result = methodScores[[i]]

      if (is.null(result)) {
        rowDf = data.frame(
          runId = runIds[i],
          method = method,
          stringsAsFactors = FALSE
        )
      } else {
        rowDf = as.data.frame(result, stringsAsFactors = FALSE)

        if (nrow(rowDf) != 1) {
          stop(
            "Each per-run score result must convert to a one-row data frame.",
            call. = FALSE
          )
        }

        rowDf$runId = runIds[i]
        rowDf$method = method
        rowDf = rowDf[, c("runId", "method", setdiff(names(rowDf), c("runId", "method"))), drop = FALSE]
      }

      pieceIndex = pieceIndex + 1L
      longPieces[[pieceIndex]] = rowDf
    }
  }

  if (length(longPieces) == 0) {
    return(data.frame())
  }

  longDf = do.call(rbind, longPieces)
  rownames(longDf) = NULL

  if (identical(format, "long")) {
    return(longDf)
  }

  nonKeyCols = setdiff(names(longDf), c("runId", "method"))

  if (length(nonKeyCols) == 0) {
    wideDf = unique(longDf[, c("runId"), drop = FALSE])
    rownames(wideDf) = NULL
    return(wideDf)
  }

  wideList = lapply(seq_len(nrow(longDf)), function(i) {
    row = longDf[i, , drop = FALSE]
    method = row$method[[1]]
    out = data.frame(runId = row$runId[[1]], stringsAsFactors = FALSE)

    for (colName in nonKeyCols) {
      out[[paste0(method, ".", colName)]] = row[[colName]][[1]]
    }

    out
  })

  wideDf = Reduce(
    function(left, right) {
      merge(left, right, by = "runId", all = TRUE, sort = TRUE)
    },
    wideList
  )

  rownames(wideDf) = NULL
  wideDf
}
