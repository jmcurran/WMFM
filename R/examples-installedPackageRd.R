#' Get an Rd object for a topic from an installed package
#'
#' Looks up an Rd help topic in an installed package using `tools::Rd_db()`
#' and returns the matching Rd object when found.
#'
#' @param topic Character scalar. Topic or alias to look up.
#' @param package Character scalar. Installed package name.
#'
#' @return An Rd object, or `NULL` if no matching help topic is found.
#' @keywords internal
#' @importFrom tools Rd_db
getInstalledPackageRd = function(topic, package) {
  if (!is.character(topic) || length(topic) != 1 || is.na(topic) || !nzchar(topic)) {
    stop("`topic` must be a non-empty character scalar.", call. = FALSE)
  }

  if (!is.character(package) || length(package) != 1 || is.na(package) || !nzchar(package)) {
    stop("`package` must be a non-empty character scalar.", call. = FALSE)
  }

  if (!requireNamespace(package, quietly = TRUE)) {
    return(NULL)
  }

  rdDb = Rd_db(package = package)

  if (length(rdDb) == 0L) {
    return(NULL)
  }

  for (rd in rdDb) {
    aliases = getRdAliases(rd)

    if (topic %in% aliases) {
      return(rd)
    }
  }

  NULL
}

#' Extract aliases from an Rd object
#'
#' @param rd An Rd object.
#'
#' @return Character vector of aliases found in the Rd object.
#' @keywords internal
getRdAliases = function(rd) {
  aliases = character(0)

  for (element in rd) {
    tag = attr(element, "Rd_tag", exact = TRUE)

    if (identical(tag, "\\name") || identical(tag, "\\alias")) {
      value = paste(as.character(element), collapse = "")
      value = trimws(value)

      if (nzchar(value)) {
        aliases = c(aliases, value)
      }
    }
  }

  unique(aliases)
}
