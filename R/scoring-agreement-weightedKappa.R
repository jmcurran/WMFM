#' Compute quadratic weighted kappa for ordinal values
#'
#' Computes quadratic weighted kappa for two ordinal vectors that have
#' already been converted to integer scores.
#'
#' @param leftVec Integer-like vector of left-side ordinal values.
#' @param rightVec Integer-like vector of right-side ordinal values.
#'
#' @return A numeric scalar, or `NA_real_` if it cannot be computed.
#' @keywords internal
computeWeightedKappa = function(leftVec, rightVec) {
  ok = !(is.na(leftVec) | is.na(rightVec))
  leftVec = as.integer(leftVec[ok])
  rightVec = as.integer(rightVec[ok])

  if (length(leftVec) == 0) {
    return(NA_real_)
  }

  cats = sort(unique(c(leftVec, rightVec)))
  k = length(cats)

  if (k <= 1) {
    return(1)
  }

  li = match(leftVec, cats)
  ri = match(rightVec, cats)

  obs = matrix(0, nrow = k, ncol = k)

  for (i in seq_along(li)) {
    obs[li[i], ri[i]] = obs[li[i], ri[i]] + 1
  }

  obs = obs / sum(obs)
  rowM = rowSums(obs)
  colM = colSums(obs)
  exp = outer(rowM, colM)

  w = outer(
    seq_len(k),
    seq_len(k),
    FUN = function(i, j) {
      (abs(i - j) / (k - 1))^2
    }
  )

  observedWeightedDisagreement = sum(w * obs)
  expectedWeightedDisagreement = sum(w * exp)

  if (expectedWeightedDisagreement == 0) {
    return(NA_real_)
  }

  1 - (observedWeightedDisagreement / expectedWeightedDisagreement)
}
