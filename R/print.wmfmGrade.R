#' Print a WMFM grade object
#'
#' @param x A `wmfmGrade` object.
#' @param method Optional character. One of "deterministic" or "llm".
#' @param format Character. One of "plaintext" or "html".
#' @param digits Number of digits for numeric output.
#' @param maxRows Maximum rows per section.
#' @param ... Unused.
#'
#' @return Invisibly returns `x` for plaintext and an HTML document for html.
#' @importFrom htmltools tags browsable save_html HTML
#' @importFrom utils browseURL
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
    if (length(v) != 1 || is.na(v)) {
      return("NA")
    }
    format(round(v, digits), nsmall = digits, trim = TRUE)
  }

  chooseMethod = function() {
    available = names(x$scores$byMethod %||% list())

    if (is.null(method)) {
      m = x$meta$lastScoredMethod %||% available[1]
    } else {
      m = match.arg(method, c("deterministic", "llm"))
    }

    if (!m %in% available) {
      stop("No grade available for method `", m, "`.", call. = FALSE)
    }

    m
  }

  buildLines = function(df, labelCol, textCol, lossCol = NULL) {
    if (!is.data.frame(df) || nrow(df) == 0) {
      return(character(0))
    }

    df = utils::head(df, maxRows)

    vapply(seq_len(nrow(df)), function(i) {
      label = as.character(df[[labelCol]][i])
      txt = as.character(df[[textCol]][i])

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

    df = ms[keep, , drop = FALSE]

    if (nrow(df) == 0) return(character(0))

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

  # ---------- PLAINTEXT ----------
  if (format == "plaintext") {

    cat("WMFM grade\n")
    cat("----------\n")
    cat("Method:", m, "\n")
    cat("Mark:", fmt(mark), "/", scale, "\n")
    cat("Overall score:", fmt(overall), "/ 100\n")

    if (!is.na(words)) cat("Words:", words, "\n")

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

  # ---------- HTML ----------

  makeList = function(lines) {
    if (!length(lines)) return(NULL)
    tags$ul(lapply(lines, function(l) tags$li(sub("^\\*\\s*", "", l))))
  }

  addSection = function(title, lines) {
    if (!length(lines)) return(NULL)
    list(tags$h2(title), makeList(lines))
  }

  body = list(
    tags$h1("WMFM grade"),
    tags$p(tags$strong("Method: "), m),
    tags$p(tags$strong("Mark: "), paste0(fmt(mark), " / ", scale)),
    tags$p(tags$strong("Overall score: "), paste0(fmt(overall), " / 100"))
  )

  body = c(
    body,
    addSection("Dimension scores", dims),
    addSection("Strengths", strengths),
    addSection("Weaknesses", weaknesses),
    addSection("Missing", missing),
    addSection("Detailed mark losses", losses),
    addSection("Advisory flags", advisory),
    addSection("Comparison", compare)
  )

  doc = browsable(
    tags$html(
      tags$head(tags$title("WMFM grade")),
      tags$body(body)
    )
  )

  tmp = tempfile(fileext = ".html")
  save_html(doc, tmp)

  if (requireNamespace("rstudioapi", quietly = TRUE)) {
    viewer = getFromNamespace("viewer", "rstudioapi")
    isAvailable = getFromNamespace("isAvailable", "rstudioapi")

    if (isAvailable()) {
      viewer(tmp)
    } else {
      browseURL(tmp)
    }
  } else {
    browseURL(tmp)
  }

  invisible(doc)
}
