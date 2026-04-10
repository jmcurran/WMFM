#' Derive paired metric comparison data directly from wmfmScores
#'
#' Internal helper that reconstructs run-level paired deterministic and LLM
#' scores for a single metric from a \code{wmfmScores} object.
#'
#' @param scores A \code{wmfmScores} object.
#' @param metric A single metric name.
#'
#' @return A data frame with columns \code{runId}, \code{detValue}, and
#'   \code{llmValue}.
#'
#' @keywords internal
deriveMetricComparisonDataFromScores = function(scores, metric) {
  if (!inherits(scores, "wmfmScores")) {
    stop("scores must be a wmfmScores object.")
  }

  if (is.null(scores$scores) || !is.list(scores$scores)) {
    stop("scores$scores must exist and be a list.")
  }

  detList = scores$scores$deterministic
  llmList = scores$scores$llm

  if (is.null(detList) || is.null(llmList)) {
    stop(
      "Both deterministic and llm scores must be present in scores$scores ",
      "to diagnose disagreement."
    )
  }

  if (!is.list(detList) || !is.list(llmList)) {
    stop("scores$scores$deterministic and scores$scores$llm must both be lists.")
  }

  if (length(detList) != length(llmList)) {
    stop("Deterministic and LLM score lists have different lengths.")
  }

  detDf = do.call(
    rbind,
    lapply(seq_along(detList), function(i) {
      run = detList[[i]]

      if (!is.list(run) || is.null(names(run))) {
        stop("Deterministic score records must be named lists.")
      }

      if (!metric %in% names(run)) {
        stop("Metric not found in deterministic score record: ", metric)
      }

      data.frame(
        runId = i,
        detValue = suppressWarnings(as.numeric(run[[metric]])),
        stringsAsFactors = FALSE
      )
    })
  )

  llmDf = do.call(
    rbind,
    lapply(seq_along(llmList), function(i) {
      run = llmList[[i]]

      if (!is.list(run) || is.null(names(run))) {
        stop("LLM score records must be named lists.")
      }

      if (!metric %in% names(run)) {
        stop("Metric not found in llm score record: ", metric)
      }

      data.frame(
        runId = i,
        llmValue = suppressWarnings(as.numeric(run[[metric]])),
        stringsAsFactors = FALSE
      )
    })
  )

  merge(detDf, llmDf, by = "runId", all = TRUE, sort = FALSE)
}
