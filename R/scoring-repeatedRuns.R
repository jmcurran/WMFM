#' Score repeated WMFM explanation runs using a multidimensional rubric
#'
#' Applies a rubric-based scoring framework to repeated WMFM explanation runs.
#' The scoring structure is designed to align with the revised
#' `buildWmfmRunRecord()` schema by separating:
#'
#' \enumerate{
#'   \item descriptive metadata about the run,
#'   \item extracted claim variables describing what the explanation said,
#'   \item judged quality variables describing whether those claims were
#'     appropriate, and
#'   \item aggregate dimension scores.
#' }
#'
#' The function accepts either:
#' \itemize{
#'   \item a data.frame of run records, or
#'   \item a list containing a `runsDf` element.
#' }
#'
#' ## Rubric dimensions
#'
#' The function scores explanations across five dimensions:
#'
#' \describe{
#'   \item{Factual score}{How well the explanation states the main effect,
#'   effect scale, reference-group structure, and interaction substance.}
#'   \item{Inference score}{How appropriate the explanation's inferential
#'   language is, especially for uncertainty and interaction evidence.}
#'   \item{Completeness score}{Whether the explanation covers the important
#'   ingredients that the model structure suggests should be present.}
#'   \item{Clarity score}{Whether the explanation is reasonably clear, sensibly
#'   expressed, and not excessively short or long.}
#'   \item{Calibration score}{Whether the explanation avoids overclaiming and
#'   severe underclaiming.}
#' }
#'
#' ## Judged fields created here
#'
#' This function fills or overwrites the following judged fields when they are
#' missing or `NA`:
#'
#' \describe{
#'   \item{effectDirectionCorrect}{Integer in `0`, `1`, `2`.}
#'   \item{effectScaleAppropriate}{Integer in `0`, `1`, `2`.}
#'   \item{referenceGroupHandledCorrectly}{Integer in `0`, `1`, `2`.}
#'   \item{interactionCoverageAdequate}{Integer in `0`, `1`, `2`.}
#'   \item{interactionSubstantiveCorrect}{Integer in `0`, `1`, `2`.}
#'   \item{uncertaintyHandlingAppropriate}{Integer in `0`, `1`, `2`.}
#'   \item{inferentialRegisterAppropriate}{Integer in `0`, `1`, `2`.}
#'   \item{mainEffectCoverageAdequate}{Integer in `0`, `1`, `2`.}
#'   \item{referenceGroupCoverageAdequate}{Integer in `0`, `1`, `2`.}
#'   \item{clarityAdequate}{Integer in `0`, `1`, `2`.}
#'   \item{numericExpressionAdequate}{Integer in `0`, `1`, `2`.}
#'   \item{comparisonStructureClear}{Integer in `0`, `1`, `2`.}
#'   \item{fatalFlawDetected}{Logical.}
#' }
#'
#' ## Aggregate fields created here
#'
#' \describe{
#'   \item{factualScore}{Numeric score on a `0` to `2` scale.}
#'   \item{inferenceScore}{Numeric score on a `0` to `2` scale.}
#'   \item{completenessScore}{Numeric score on a `0` to `2` scale.}
#'   \item{clarityScore}{Numeric score on a `0` to `2` scale.}
#'   \item{calibrationScore}{Numeric score on a `0` to `2` scale.}
#'   \item{overallScore}{Numeric weighted score on a `0` to `100` scale.}
#'   \item{overallPass}{Logical.}
#' }
#'
#' Duplicate detection is based on `normalizedExplanation` when available,
#' otherwise on trimmed explanation text.
#'
#' Fatal flaws do not force the score to zero, but they cap the final overall
#' score using `fatalFlawCap`.
#'
#' @param runsDf A data.frame of repeated-run outputs, or a list containing a
#'   `runsDf` element.
#' @param preferredMinWords Integer. Lower bound for a preferred explanation
#'   length band.
#' @param preferredMaxWords Integer. Upper bound for a preferred explanation
#'   length band.
#' @param penaliseDuplicates Logical. Should exact duplicate explanations be
#'   penalised?
#' @param duplicatePenalty Numeric penalty subtracted from the final
#'   `overallScore` for exact duplicates.
#' @param fatalFlawCap Numeric in `0` to `100`. Maximum allowed overall score
#'   when `fatalFlawDetected` is `TRUE`.
#' @param passThreshold Numeric in `0` to `100`. Threshold used to create
#'   `overallPass`.
#' @param factualWeight Numeric weight for the factual dimension.
#' @param inferenceWeight Numeric weight for the inference dimension.
#' @param completenessWeight Numeric weight for the completeness dimension.
#' @param clarityWeight Numeric weight for the clarity dimension.
#' @param calibrationWeight Numeric weight for the calibration dimension.
#'
#' @return A data.frame with judged quality columns and dimension scores added.
#' @export
scoreWmfmRepeatedRuns = function(
    runsDf,
    preferredMinWords = 80L,
    preferredMaxWords = 220L,
    penaliseDuplicates = TRUE,
    duplicatePenalty = 5,
    fatalFlawCap = 40,
    passThreshold = 65,
    factualWeight = 0.30,
    inferenceWeight = 0.25,
    completenessWeight = 0.20,
    clarityWeight = 0.15,
    calibrationWeight = 0.10
) {

  extractRunsDf = function(x) {
    if (is.data.frame(x)) {
      return(x)
    }

    if (is.list(x) && "runsDf" %in% names(x) && is.data.frame(x$runsDf)) {
      return(x$runsDf)
    }

    stop(
      "`runsDf` must be a data.frame or a list containing a data.frame named `runsDf`.",
      call. = FALSE
    )
  }

  runsDf = extractRunsDf(runsDf)

  scoreWmfmRunRecordsCore(
    runsDf = runsDf,
    preferredMinWords = preferredMinWords,
    preferredMaxWords = preferredMaxWords,
    penaliseDuplicates = penaliseDuplicates,
    duplicatePenalty = duplicatePenalty,
    fatalFlawCap = fatalFlawCap,
    passThreshold = passThreshold,
    factualWeight = factualWeight,
    inferenceWeight = inferenceWeight,
    completenessWeight = completenessWeight,
    clarityWeight = clarityWeight,
    calibrationWeight = calibrationWeight
  )
}
