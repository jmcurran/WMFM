#' Detect a response transformation from an expression string
#'
#' Attempts to detect a common response transformation from a text expression.
#' This is intended for lightweight UI labeling (e.g., "log", "sqrt") rather
#' than for full parsing of arbitrary R code.
#'
#' Recognised transformations (returned values):
#' \itemize{
#'   \item \code{"log"} for expressions starting with \code{log(}
#'   \item \code{"log1p"} for expressions starting with \code{log1p(}
#'   \item \code{"sqrt"} for expressions starting with \code{sqrt(}
#' }
#'
#' If no recognised transformation is detected, returns \code{NULL}.
#'
#' @param expr A character string containing an expression.
#'
#' @return A single character string naming the detected transformation, or
#'   \code{NULL} if none is detected.
#'
#' @examples
#' WMFM:::detectRespTransform("log(y)")
#' WMFM:::detectRespTransform(" log1p(count) ")
#' WMFM:::detectRespTransform("sqrt(x)")
#' WMFM:::detectRespTransform("y")
#'
#' @keywords internal
detectRespTransform = function(expr) {

  if (is.null(expr)) {
    return(NULL)
  }

  expr = trimws(as.character(expr))

  if (!nzchar(expr)) {
    return(NULL)
  }

  # Normalise whitespace for detection
  exprNoSpace = gsub("\\s+", "", expr)

  if (grepl("^log1p\\(", exprNoSpace)) {
    return("log1p")
  }

  if (grepl("^log\\(", exprNoSpace)) {
    return("log")
  }

  if (grepl("^sqrt\\(", exprNoSpace)) {
    return("sqrt")
  }

  NULL
}


#' Extract a noun phrase for the response variable from dataset documentation
#'
#' Attempts to infer a short human-friendly description of the response variable
#' (e.g. "exam score", "weekly income", "probability of success") from a plain-text
#' dataset help/documentation string.
#'
#' The extraction is heuristic: it looks for common patterns such as:
#' \itemize{
#'   \item A line like \code{"responseVar: ..."} or \code{"responseVar - ..."}
#'   \item A variables table/bullets where the response variable is described
#' }
#' If nothing reliable is found, it returns \code{NULL}.
#'
#' @param dsDoc Character scalar. Plain-text dataset documentation.
#' @param responseVar Character scalar. Name of the response variable.
#' @return Character scalar noun phrase, or \code{NULL} if not found.
#' @keywords internal
extractResponseNounPhraseFromDoc = function(dsDoc, responseVar) {
  if (is.null(dsDoc) || !nzchar(dsDoc) || is.null(responseVar) || !nzchar(responseVar)) {
    return(NULL)
  }

  lines = strsplit(dsDoc, "\n", fixed = TRUE)[[1]]
  lines = trimws(lines)

  # Match patterns like:
  #   y: Exam score out of 100
  #   y - Exam score out of 100
  #   y = Exam score out of 100
  pat = paste0("^", responseVar, "\\s*[:=\\-]\\s*(.+)$")
  hit = grep(pat, lines, value = TRUE, ignore.case = FALSE)

  if (length(hit) > 0) {
    noun = sub(pat, "\\1", hit[1], perl = TRUE)
    noun = trimws(noun)

    # keep it short-ish; strip trailing full stops
    noun = sub("\\.+$", "", noun)
    if (nzchar(noun)) return(noun)
  }

  # Another common pattern in help dumps is "responseVar (unit/meaning...)"
  pat2 = paste0("^", responseVar, "\\s*\\((.+)\\)\\s*$")
  hit2 = grep(pat2, lines, value = TRUE)
  if (length(hit2) > 0) {
    noun = sub(pat2, "\\1", hit2[1], perl = TRUE)
    noun = trimws(sub("\\.+$", "", noun))
    if (nzchar(noun)) return(noun)
  }

  NULL
}


#' Resolve the response noun phrase for a fitted model
#'
#' @param model Fitted model object.
#' @param responseVar Character scalar response variable name.
#' @return Character scalar noun phrase.
#' @keywords internal
resolveResponseNounPhrase = function(model, responseVar) {
  dsDoc = attr(model, "wmfm_dataset_doc", exact = TRUE)

  noun = extractResponseNounPhraseFromDoc(dsDoc, responseVar)
  if (!is.null(noun) && nzchar(noun)) {
    return(noun)
  }

  # Fallback: the raw response variable name is better than guessing.
  responseVar
}


#' List valid response variables for a model type
#'
#' Returns the names of variables in a data frame that are supported
#' as response variables for the specified model type.
#'
#' @param data A data frame.
#' @param modelType Model type passed to \code{validateResponseVar()}.
#'
#' @return A character vector of variable names.
#'
#' @keywords internal
validResponseVars = function(data, modelType) {

  if (!is.data.frame(data)) {
    return(character(0))
  }

  vars = names(data)

  ok = vapply(
    vars,
    function(v) validateResponseVar(data, v, modelType)$ok,
    logical(1)
  )

  vars[ok]
}


#' Validate a response variable for a given model type
#'
#' Determines whether a candidate response variable is compatible with the
#' model-fitting logic implemented in the package.
#'
#' This function exists to prevent unsupported modelling choices such as
#' multi-level categorical responses for logistic regression (multinomial
#' models are not implemented).
#'
#' Supported response types:
#' \itemize{
#'   \item \strong{lm}: numeric responses or 2-level factors
#'   \item \strong{logistic}: binary responses only
#'   \item \strong{poisson}: non-negative integer counts
#' }
#'
#' @param data A data frame containing the response variable.
#' @param responseVar A character string naming the response variable.
#' @param modelType A character string: \code{"lm"}, \code{"logistic"},
#'   or \code{"poisson"}.
#'
#' @return A list with components:
#' \describe{
#'   \item{ok}{Logical; whether the response is supported.}
#'   \item{reason}{Character string explaining the decision.}
#' }
#'
#' @importFrom stats na.omit setNames
#'
#' @keywords internal
validateResponseVar = function(data, responseVar, modelType) {

  if (!is.data.frame(data)) {
    return(list(ok = FALSE, reason = "No data available."))
  }

  if (!nzchar(responseVar) || !(responseVar %in% names(data))) {
    return(list(ok = FALSE, reason = "No valid response variable selected."))
  }

  y = data[[responseVar]]

  nDistinct = function(x) {
    length(unique(na.omit(x)))
  }

  if (identical(modelType, "lm")) {

    if (is.numeric(y)) {
      return(list(ok = TRUE, reason = "Numeric response."))
    }

    if (is.factor(y) && nlevels(y) == 2) {
      return(list(ok = TRUE, reason = "2-level factor response."))
    }

    if (is.logical(y)) {
      return(list(ok = TRUE, reason = "Logical response."))
    }

    return(list(
      ok = FALSE,
      reason = "Linear regression supports numeric or 2-level factor responses only."
    ))
  }

  if (identical(modelType, "logistic")) {

    if (is.factor(y)) {
      if (nlevels(y) == 2) {
        return(list(ok = TRUE, reason = "Binary factor response."))
      }
      return(list(
        ok = FALSE,
        reason = "Multinomial logistic regression is not implemented."
      ))
    }

    if (is.character(y)) {
      if (nDistinct(y) == 2) {
        return(list(ok = TRUE, reason = "Binary character response."))
      }
      return(list(
        ok = FALSE,
        reason = "Logistic regression requires exactly two response values."
      ))
    }

    if (is.logical(y)) {
      return(list(ok = TRUE, reason = "Logical response."))
    }

    if (is.numeric(y)) {
      uy = unique(na.omit(y))
      if (length(uy) == 0) {
        return(list(ok = FALSE, reason = "Response has no non-missing values."))
      }
      if (all(uy %in% c(0, 1))) {
        return(list(ok = TRUE, reason = "Numeric 0/1 response."))
      }
      return(list(
        ok = FALSE,
        reason = "Numeric logistic responses must be coded 0/1."
      ))
    }

    return(list(
      ok = FALSE,
      reason = "Logistic regression requires a binary response."
    ))
  }

  if (identical(modelType, "poisson")) {

    if (!is.numeric(y)) {
      return(list(
        ok = FALSE,
        reason = "Poisson regression requires a numeric count response."
      ))
    }

    yy = na.omit(y)

    if (length(yy) == 0) {
      return(list(ok = FALSE, reason = "Response has no non-missing values."))
    }

    if (any(yy < 0) || any(yy %% 1 != 0)) {
      return(list(
        ok = FALSE,
        reason = "Poisson regression requires non-negative integer counts."
      ))
    }

    return(list(ok = TRUE, reason = "Count response."))
  }

  list(ok = FALSE, reason = "Unknown model type.")
}
