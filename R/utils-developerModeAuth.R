#' Create a developer-mode password hash
#'
#' Creates a salted password hash suitable for storing in the
#' `WMFM_DEVELOPER_MODE_PASSWORD_HASH` environment variable.
#'
#' @param password Character scalar giving the plain-text password.
#'
#' @return A salted password hash string.
#'
#' @export
makeDeveloperModePasswordHash = function(password) {
  if (!requireNamespace("sodium", quietly = TRUE)) {
    stop("The sodium package is required to create the developer-mode password hash.")
  }

  if (!is.character(password) || length(password) != 1 || !nzchar(password)) {
    stop("password must be a single non-empty string.")
  }

  sodium::password_store(password)
}

#' Store a developer-mode password hash
#'
#' Backward-compatible alias for `makeDeveloperModePasswordHash()`.
#'
#' @param password Character scalar giving the plain-text password.
#'
#' @return A salted password hash string.
#'
#' @keywords internal
storeDeveloperModePasswordHash = function(password) {
  makeDeveloperModePasswordHash(password)
}

#' Verify the developer-mode password
#'
#' Compares a candidate password against the salted hash stored in the
#' `WMFM_DEVELOPER_MODE_PASSWORD_HASH` environment variable.
#'
#' @param password Character scalar giving the candidate password.
#'
#' @return Logical scalar. Returns `TRUE` when the password matches and
#'   `FALSE` otherwise.
#'
#' @keywords internal
verifyDeveloperModePassword = function(password) {
  if (!requireNamespace("sodium", quietly = TRUE)) {
    stop("The sodium package is required to verify the developer-mode password.")
  }

  storedHash = Sys.getenv("WMFM_DEVELOPER_MODE_PASSWORD_HASH", unset = "")

  if (!nzchar(storedHash)) {
    stop("WMFM_DEVELOPER_MODE_PASSWORD_HASH is not set.")
  }

  if (!is.character(password) || length(password) != 1 || !nzchar(password)) {
    return(FALSE)
  }

  isTRUE(sodium::password_verify(storedHash, password))
}
