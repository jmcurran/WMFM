#' Create app server observer dependencies
#'
#' @param input Shiny input object.
#' @param session Shiny session object.
#' @param rv App reactive values object.
#' @param modelFit Reactive value containing the fitted model.
#'
#' @return A list of helper functions used by observer registration code.
#'
#' @keywords internal
createAppServerObserverDependencies = function(input, session, rv, modelFit) {
  serverStateHelpers = createAppServerStateHelpers(
    input = input,
    session = session,
    rv = rv,
    modelFit = modelFit
  )

  list(
    setBucketState = serverStateHelpers$setBucketState,
    resetModelPage = serverStateHelpers$resetModelPage,
    applyLoadedExampleToInputs = serverStateHelpers$applyLoadedExampleToInputs
  )
}
