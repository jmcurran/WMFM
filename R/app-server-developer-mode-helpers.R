#' Build developer mode status text
#'
#' @param isUnlocked Logical value indicating whether developer mode is
#'   currently unlocked.
#'
#' @return A character string for the developer mode status display.
#' @keywords internal
buildDeveloperModeStatus = function(isUnlocked) {
  if (isTRUE(isUnlocked)) {
    return("Developer mode is unlocked.")
  }

  "Developer mode is locked."
}

#' Build the incorrect developer mode password message
#'
#' @return A character string for the notification.
#' @keywords internal
buildDeveloperModeIncorrectPasswordMessage = function() {
  "Incorrect password. Developer mode remains locked."
}

#' Build the developer mode unlocked message
#'
#' @return A character string for the notification.
#' @keywords internal
buildDeveloperModeUnlockedMessage = function() {
  "Developer mode unlocked."
}

#' Build the developer mode locked message
#'
#' @return A character string for the notification.
#' @keywords internal
buildDeveloperModeLockedMessage = function() {
  "Developer mode locked."
}
