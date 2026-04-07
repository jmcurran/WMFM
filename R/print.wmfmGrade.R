#' Print a WMFM grade object
#'
#' @param x A `wmfmGrade` object.
#' @param method Optional character. One of "deterministic" or "llm".
#' @param format Character. One of "plaintext" or "html".
#' @param digits Number of digits for numeric output.
#' @param maxRows Maximum rows per section.
#' @param ... Unused.
#'
#' @export
print.wmfmGrade = function(
    x,
    method = NULL,
    format = c("plaintext", "html"),
    digits = 2,
    maxRows = 6,
    ...
) {

  if (!inherits(x, "wmfmGrade")) {
    stop("`x` must inherit from `wmfmGrade`.", call. = FALSE)
  }

  format = match.arg(format)

  `%||%` = function(a, b) if (is.null(a)) b else a

  fmt = function(v) {
    if (is.na(v)) {
      return("NA")
    }
    format(round(v, digits), nsmall = digits)
  }

  chooseMethod = function() {
    available = names(x$scores$byMethod %||% list())

    if (is.null(method)) {
      m = x$meta$lastScoredMethod %||% available[1]
    } else {
      m = match.arg(method, c("deterministic", "llm"))
    }

    if (!m %in% available) {
      stop("No grade available for method ", m, call. = FALSE)
    }

    m
  }

  buildLines = function(df, labelCol, textCol, lossCol = NULL) {
    if (!is.data.frame(df) || nrow(df) == 0) {
      return(character(0))
    }

    df = utils::head(df, maxRows)

    vapply(seq_len(nrow(df)), function(i) {
      label = df[[labelCol]][i]
      txt = df[[textCol]][i]

      if (!is.null(lossCol) && lossCol %in% names(df)) {
        loss = suppressWarnings(as.numeric(df[[lossCol]][i]))
        lossTxt = if (!is.na(loss) && loss > 0) paste0(" (-", fmt(loss), ")") else ""
        paste0("* ", label, lossTxt, ": ", txt)
      } else {
        paste0("* ", label, ": ", txt)
      }
    }, character(1))
  }

  buildDims = function(ms) {
    if (!is.data.frame(ms)) return(character(0))

    keep = ms$metric %in% c(
      "factualScore",
      "inferenceScore",
      "completenessScore",
      "clarityScore",
      "calibrationScore"
    )

    df = ms[keep, ]

    vapply(seq_len(nrow(df)), function(i) {
      paste0(
        "  ",
        df$label[i], ": ",
        fmt(df$studentValue[i]), " / ",
        fmt(df$maxValue[i])
      )
    }, character(1))
  }

  m = chooseMethod()

  scoreBlock = x$scores$byMethod[[m]]
  fb = x$feedback$byMethod[[m]]

  overall = scoreBlock$overallScore
  mark = scoreBlock$mark
  scale = x$scoreScale
  words = scoreBlock$student$wordCount[1] %||% NA

  dims = buildDims(scoreBlock$metricSummary)
  strengths = buildLines(fb$strengths, "label", "comment")
  weaknesses = buildLines(fb$weaknesses, "label", "reason", "marksLost")
  missing = buildLines(fb$missingElements, "label", "detail", "marksLost")
  losses = buildLines(fb$whereMarksLost, "label", "reason", "marksLost")
  advisory = buildLines(fb$advisoryFlags, "label", "detail", "severity")
  compare = buildLines(fb$modelAnswerComparison, "label", "comment", "referenceDelta")

  # -------------------------
  # PLAINTEXT
  # -------------------------
  if (format == "plaintext") {

    cat("WMFM grade\n")
    cat("----------\n")
    cat("Method:", m, "\n")
    cat("Mark:", fmt(mark), "/", scale, "\n")
    cat("Overall score:", fmt(overall), "/ 100\n")

    if (!is.na(words)) {
      cat("Words:", words, "\n")
    }

    if (m == "llm" && isTRUE(scoreBlock$overallDerivedFromDimensions)) {
      cat("Note: overall score derived from dimension scores\n")
    }

    if (length(dims)) {
      cat("\nDimension scores\n")
      cat(paste(dims, collapse = "\n"), "\n")
    }

    if (length(strengths)) {
      cat("\nStrengths\n")
      cat(paste(strengths, collapse = "\n"), "\n")
    }

    if (length(weaknesses)) {
      cat("\nWeaknesses\n")
      cat(paste(weaknesses, collapse = "\n"), "\n")
    }

    if (length(missing)) {
      cat("\nMissing or underdeveloped elements\n")
      cat(paste(missing, collapse = "\n"), "\n")
    }

    cat("\nDetailed mark losses\n")
    if (length(losses)) {
      cat(paste(losses, collapse = "\n"), "\n")
    } else {
      cat("None detected by the current rubric.\n")
    }

    if (length(advisory)) {
      cat("\nAdditional rubric flags\n")
      cat(paste(advisory, collapse = "\n"), "\n")
    }

    if (length(compare)) {
      cat("\nCompared with model answer\n")
      cat(paste(compare, collapse = "\n"), "\n")
    }

    return(invisible(x))
  }

  # -------------------------
  # HTML
  # -------------------------

  makeList = function(lines) {
    if (!length(lines)) return(NULL)
    htmltools::tags$ul(lapply(lines, function(l) {
      htmltools::tags$li(sub("^\\*\\s*", "", l))
    }))
  }

  body = list(
    htmltools::tags$h1("WMFM grade"),
    htmltools::tags$p(strong("Method: "), m),
    htmltools::tags$p(strong("Mark: "), paste0(fmt(mark), " / ", scale)),
    htmltools::tags$p(strong("Overall: "), paste0(fmt(overall), " / 100"))
  )

  if (!is.na(words)) {
    body = c(body, list(htmltools::tags$p(strong("Words: "), words)))
  }

  add = function(title, lines) {
    if (!length(lines)) return(NULL)
    list(htmltools::tags$h2(title), makeList(lines))
  }

  body = c(
    body,
    add("Dimension scores", dims),
    add("Strengths", strengths),
    add("Weaknesses", weaknesses),
    add("Missing", missing),
    add("Detailed mark losses", losses),
    add("Advisory flags", advisory),
    add("Comparison", compare)
  )

  doc = htmltools::browsable(
    htmltools::tags$html(
      htmltools::tags$body(body)
    )
  )

  tmp = tempfile(fileext = ".html")
  htmltools::save_html(doc, tmp)

  if (requireNamespace("rstudioapi", quietly = TRUE) &&
      rstudioapi::isAvailable()) {
    rstudioapi::viewer(tmp)
  } else {
    utils::browseURL(tmp)
  }

  invisible(doc)
}
