#' Create an empty WMFM scores object
#'
#' Creates a `wmfmScores` object aligned to a `wmfmRuns` object but containing
#' no scoring results yet. This is a constructor/helper for the scoring
#' workflow. Actual score values are added later by `score()`.
#'
#' @param x A `wmfmRuns` object.
#' @param methods Character vector of scoring methods to reserve. Allowed values
#'   are `"deterministic"` and `"llm"`.
#'
#' @return An object of class `wmfmScores`.
#' @export
newWmfmScores = function(
    x,
    methods = character(0)
) {

  if (!inherits(x, "wmfmRuns")) {
    stop("`x` must inherit from `wmfmRuns`.", call. = FALSE)
  }

  if (!is.character(methods)) {
    stop("`methods` must be a character vector.", call. = FALSE)
  }

  if (length(methods) > 0) {
    badMethods = setdiff(methods, c("deterministic", "llm"))

    if (length(badMethods) > 0) {
      stop(
        "Unsupported scoring method(s): ",
        paste(badMethods, collapse = ", "),
        call. = FALSE
      )
    }

    methods = unique(methods)
  }

  runIds = vapply(
    x$runs,
    function(run) {
      if (!is.list(run) || is.null(run$runId)) {
        return(NA_integer_)
      }

      as.integer(run$runId)
    },
    integer(1)
  )

  scores = setNames(vector("list", length(methods)), methods)

  for (method in methods) {
    scores[[method]] = vector("list", length(x$runs))
  }

  out = list(
    exampleName = x$exampleName %||% NA_character_,
    package = x$package %||% NA_character_,
    runIds = runIds,
    methods = methods,
    scores = scores,
    meta = list(
      createdAt = as.character(Sys.time()),
      sourceClass = class(x)[1],
      nRuns = length(x$runs)
    )
  )

  class(out) = c("wmfmScores", class(out))
  out
}
