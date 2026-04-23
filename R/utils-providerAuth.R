#' Store a provider-switch password hash
#'
#' Creates a salted password hash suitable for storing in the
#' `WMFM_PROVIDER_SWITCH_PASSWORD_HASH` environment variable.
#'
#' @param password Character scalar giving the plain-text password.
#'
#' @return A salted password hash string.
#'
#' @keywords internal
storeProviderSwitchPasswordHash = function(password) {
  if (!requireNamespace("sodium", quietly = TRUE)) {
    stop("The sodium package is required to create the password hash.")
  }

  if (!is.character(password) || length(password) != 1 || !nzchar(password)) {
    stop("password must be a single non-empty string.")
  }

  sodium::password_store(password)
}

#' Verify the provider-switch password
#'
#' Compares a candidate password against the salted hash stored in the
#' `WMFM_PROVIDER_SWITCH_PASSWORD_HASH` environment variable.
#'
#' @param password Character scalar giving the candidate password.
#'
#' @return Logical scalar. Returns `TRUE` when the password matches and
#'   `FALSE` otherwise.
#'
#' @keywords internal
verifyProviderSwitchPassword = function(password) {
  if (!requireNamespace("sodium", quietly = TRUE)) {
    stop("The sodium package is required to verify the provider-switch password.")
  }

  storedHash = Sys.getenv("WMFM_PROVIDER_SWITCH_PASSWORD_HASH", unset = "")

  if (!nzchar(storedHash)) {
    stop("WMFM_PROVIDER_SWITCH_PASSWORD_HASH is not set.")
  }

  if (!is.character(password) || length(password) != 1 || !nzchar(password)) {
    return(FALSE)
  }

  isTRUE(sodium::password_verify(storedHash, password))
}
