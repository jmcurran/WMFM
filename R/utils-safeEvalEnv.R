#' Create a safe evaluation environment for derived-variable expressions
#'
#' Builds an environment that contains the columns of \code{data} and a small
#' allowlist of safe base functions. This is intended for evaluating the
#' right-hand side (RHS) of user-entered derived-variable assignments in a
#' Shiny app without exposing powerful functions like file I/O or system calls.
#'
#' The returned environment contains:
#' \itemize{
#'   \item All columns of \code{data} as symbols.
#'   \item A symbol \code{data} bound to the full data frame.
#'   \item Only the allowlisted functions (from \code{base}) in the parent environment.
#' }
#'
#' @param data A data frame.
#'
#' @return An environment suitable for \code{eval(rhs, envir = env)}.
#'
#' @examples
#' df = data.frame(x = 1:5)
#' env = makeSafeEvalEnv(df)
#' eval(parse(text = "log(x)"), envir = env)
#' eval(parse(text = "factor(rep(1:2, length.out = nrow(data)))"), envir = env)
#'
#' @export
makeSafeEvalEnv = function(data) {
  env = new.env(parent = emptyenv())

  # data columns
  for (nm in names(data)) {
    env[[nm]] = data[[nm]]
  }
  env[["data"]] = data

  # allowlisted functions (whatever you already add)
  env[["asin"]]    = base::asin
  env[["arcsin"]]    = base::asin
  env[["factor"]] = base::factor
  env[["I"]]    = base::I
  env[["log"]]    = base::log
  env[["log1p"]]    = base::log1p
  env[["nrow"]] = base::nrow
  env[["rep"]]    = base::rep
  env[["seq_len"]] = base::seq_len
  env[["sqrt"]] = base::sqrt
  # etc.

  # IMPORTANT: allow ":" for sequences like 1:144
  env[[":"]] = get(":", baseenv())

  env
}
