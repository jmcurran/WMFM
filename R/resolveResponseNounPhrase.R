#' Resolve the response noun phrase for a fitted model
#'
#' @param model Fitted model object.
#' @param responseVar Character scalar response variable name.
#' @return Character scalar noun phrase.
#' @keywords internal
resolveResponseNounPhrase = function(model, responseVar) {
  dsDoc = attr(model, "wmfm_dataset_doc", exact = TRUE)

  noun = extractResponseNounPhraseFromDoc(dsDoc, responseVar)
  if (!is.null(noun) && nzchar(noun)) {
    return(noun)
  }

  # Fallback: the raw response variable name is better than guessing.
  responseVar
}
