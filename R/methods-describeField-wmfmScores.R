#' Describe a field for a WMFM scores object
#'
#' Describes a field that is present in a `wmfmScores` object. The method accepts
#' canonical field names, recognised aliases, and supported pretty plot labels.
#' It validates that the resolved canonical field is actually stored in the
#' scored records before delegating to `describeWmfmField()`.
#'
#' @param x A `wmfmScores` object.
#' @param field Character scalar naming the field to describe.
#' @param format Character. One of `"text"`, `"list"`, or `"data.frame"`.
#' @param includeExamples Logical. Should examples be included?
#' @param includeAliases Logical. Should recognised aliases be included?
#' @param ... Unused. Included for S3 compatibility.
#'
#' @return The result of `describeWmfmField()` in the requested format.
#'   This is a character scalar for `format = "text"`, a named list for
#'   `format = "list"`, or a one-row `data.frame` for
#'   `format = "data.frame"`.
#' @export
describeField.wmfmScores = function(
    x,
    field,
    format = c("text", "list", "data.frame"),
    includeExamples = TRUE,
    includeAliases = TRUE,
    ...
) {
    format = match.arg(format)

    if (!inherits(x, "wmfmScores")) {
        stop("`x` must inherit from `wmfmScores`.", call. = FALSE)
    }

    if (is.null(x$scores) || !is.list(x$scores)) {
        stop("`x` does not contain a valid `scores` element.", call. = FALSE)
    }

    scoreRecords = unlist(
        x$scores[vapply(x$scores, is.list, logical(1))],
        recursive = FALSE,
        use.names = FALSE
    )

    if (length(scoreRecords) == 0) {
        stop("`x$scores` does not contain any stored score records.", call. = FALSE)
    }

    badIndex = which(!vapply(
        scoreRecords,
        function(oneScore) {
            is.list(oneScore) && !is.null(names(oneScore))
        },
        logical(1)
    ))

    if (length(badIndex) > 0) {
        stop(
            "Stored score records must be named lists.",
            call. = FALSE
        )
    }

    resolved = describeWmfmField(
        field = field,
        format = "list",
        includeExamples = includeExamples,
        includeAliases = includeAliases
    )

    availableFields = sort(unique(unlist(lapply(scoreRecords, names), use.names = FALSE)))

    if (!resolved$canonicalName %in% availableFields) {
        stop(
            "Field `", resolved$canonicalName,
            "` is not stored in this `wmfmScores` object. Available fields include: ",
            paste(availableFields, collapse = ", "),
            call. = FALSE
        )
    }

    describeWmfmField(
        field = resolved$canonicalName,
        format = format,
        includeExamples = includeExamples,
        includeAliases = includeAliases
    )
}
