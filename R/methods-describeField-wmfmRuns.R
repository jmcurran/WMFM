#' Describe a field for a WMFM runs object
#'
#' Describes a field that is present in a `wmfmRuns` object. The method accepts
#' canonical field names, recognised aliases, and supported pretty plot labels.
#' It validates that the resolved canonical field is actually stored in the raw
#' run records before delegating to `describeWmfmField()`.
#'
#' @param x A `wmfmRuns` object.
#' @param field Character scalar naming the field to describe.
#' @param format Character. One of `"text"`, `"list"`, or `"data.frame"`.
#' @param includeExamples Logical. Should examples be included?
#' @param includeAliases Logical. Should recognised aliases be included?
#' @param ... Unused. Included for S3 compatibility.
#'
#' @return The output of `describeWmfmField()`.
#' @export
describeField.wmfmRuns = function(
    x,
    field,
    format = c("text", "list", "data.frame"),
    includeExamples = TRUE,
    includeAliases = TRUE,
    ...
) {
    format = match.arg(format)

    if (!inherits(x, "wmfmRuns")) {
        stop("`x` must inherit from `wmfmRuns`.", call. = FALSE)
    }

    if (!is.list(x$runs) || length(x$runs) == 0) {
        stop("`x$runs` must be a non-empty list of run records.", call. = FALSE)
    }

    badIndex = which(!vapply(
        x$runs,
        function(run) {
            is.list(run) && !is.null(names(run))
        },
        logical(1)
    ))

    if (length(badIndex) > 0) {
        stop(
            "All elements of `x$runs` must be named run-record lists.",
            call. = FALSE
        )
    }

    resolved = describeWmfmField(
        field = field,
        format = "list",
        includeExamples = includeExamples,
        includeAliases = includeAliases
    )

    availableFields = sort(unique(unlist(lapply(x$runs, names), use.names = FALSE)))

    if (!resolved$canonicalName %in% availableFields) {
        stop(
            "Field `", resolved$canonicalName,
            "` is not stored in this `wmfmRuns` object. Available fields include: ",
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
