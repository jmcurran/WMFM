#' Environment for caching LLM results
#'
#' Internal environment used to cache equations and explanations for
#' previously fitted models.
#'
#' @keywords internal
.env_cache = new.env(parent = emptyenv())
