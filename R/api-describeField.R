#' Describe a field for a WMFM object
#'
#' S3 generic for describing fields stored in WMFM objects.
#'
#' @param x A WMFM object.
#' @param field Character scalar naming the field to describe. This may be a
#'   canonical field name, a recognised alias, or a pretty plot label if the
#'   underlying field registry supports it.
#' @param ... Additional arguments passed to methods.
#'
#' @return Method-specific description output.
#' @export
describeField = function(x, field, ...) {
    UseMethod("describeField")
}
