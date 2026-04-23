#' Ensure that the s20x package is installed
#'
#' Confirms that the teaching-data dependency \pkg{s20x} is installed before
#' app features that rely on package datasets continue.
#'
#' @return Invisibly returns `TRUE` when \pkg{s20x} is installed.
#' @keywords internal
#' @import s20x
ensureS20xInstalled = function() {
  if (!requireNamespace("s20x", quietly = TRUE)) {
    stop(
      paste(
        "The s20x package must be installed to use the built-in teaching datasets.",
        "Please install it and restart the app."
      ),
      call. = FALSE
    )
  }

  invisible(TRUE)
}

#' List installed packages that appear to contain datasets
#'
#' Returns the names of installed packages whose installed metadata indicates
#' that they contain datasets. This avoids calling `utils::data(package = ...)`
#' for every installed package during app startup, which can be slow on shared
#' systems and on Shiny Server.
#'
#' The result is sorted alphabetically and is useful for populating a package
#' selector in the app's Load Data tab. Dataset names are still resolved lazily
#' for the selected package via `getPackageDatasetNames()`.
#'
#' @return A character vector of installed package names that appear to contain
#'   one or more datasets.
#' @keywords internal
#' @importFrom tools Rd2HTML Rd2txt Rd_db
#' @importFrom utils installed.packages data capture.output
getInstalledPackagesWithData = function() {
  pkgMatrix = installed.packages()

  if (nrow(pkgMatrix) == 0) {
    return(character(0))
  }

  pkgs = pkgMatrix[, "Package"]
  libPaths = pkgMatrix[, "LibPath"]

  hasData = file.exists(file.path(libPaths, pkgs, "Meta", "data.rds"))

  sort(unique(pkgs[hasData]))
}

#' List datasets available in an installed package
#'
#' Looks up the datasets reported by `utils::data(package = pkg)`
#' for a single installed package.
#'
#' @param pkg A package name.
#'
#' @return A sorted character vector of dataset names. Returns
#'   `character(0)` if the package name is empty, the package is not
#'   installed, or no datasets are found.
#'
#' @keywords internal
getPackageDatasetNames = function(pkg) {
  pkg = trimws(pkg %||% "")

  if (!nzchar(pkg) || !requireNamespace(pkg, quietly = TRUE)) {
    return(character(0))
  }

  out = tryCatch(
    data(package = pkg),
    error = function(e) {
      NULL
    }
  )

  if (is.null(out) || is.null(out$results) || nrow(out$results) == 0) {
    return(character(0))
  }

  sort(unique(out$results[, "Item"]))
}

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

#' Get s20x dataset documentation as plain text
#'
#' @param dataset Character string, name of a data set in the s20x package.
#'
#' @return A single character string with the Rd help text, or `NULL` if not found.
#' @keywords internal
getS20xDocText = function(dataset) {
  rd = getInstalledPackageRd(
    topic = dataset,
    package = "s20x"
  )

  if (is.null(rd)) {
    return(NULL)
  }

  txt = capture.output(Rd2txt(rd, out = ""))

  paste(txt, collapse = "\n")
}

#' Get s20x dataset documentation as HTML
#'
#' @param dataset Character string, name of a data set in the s20x package.
#'
#' @return A single character string containing HTML for the help page, or `NULL` if not found.
#' @keywords internal
getS20xDocHtml = function(dataset) {
  rd = getInstalledPackageRd(
    topic = dataset,
    package = "s20x"
  )

  if (is.null(rd)) {
    return(NULL)
  }

  tmpHtml = tempfile(fileext = ".html")

  ok = tryCatch(
    {
      Rd2HTML(rd, out = tmpHtml)
      TRUE
    },
    error = function(e) {
      FALSE
    }
  )

  if (!ok || !file.exists(tmpHtml)) {
    return(NULL)
  }

  paste(readLines(tmpHtml, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
}
