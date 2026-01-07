#' Format a contrast result
#'
#' @export
formatContrast = function(res, model, rv) {

  isGlm = inherits(model, "glm")
  link = if (isGlm) family(model)$link else "identity"
  showEta = isGlm && link != "identity"

  out = c(res$label)

  if (showEta) {
    out = c(out, paste0("  eta contrast: ", res$eta))
  }

  out = c(out, paste0("  ", res$response))

  if (!is.null(res$plain)) {
    out = c(out, paste0("  Interpretation: ", res$plain))
  }

  if (!is.null(rv$chatProvider)) {
    cache = rv$contrastLlmCache
    key = res$key
    if (!exists(key, envir = cache, inherits = FALSE)) {
      cache[[key]] = llmInterpret(rv$chatProvider, res$prompt)
    }
    out = c(out, paste0("  LLM interpretation: ", cache[[key]]))
  }

  paste(out, collapse = "\n")
}

llmInterpret = function(provider, prompt) {
  ans = ellmer::chat(provider, prompt)
  gsub("[[:space:]]+", " ", trimws(ans))
}
