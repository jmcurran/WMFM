makeOfflineWmfmModel = function() {
  df = data.frame(
    y = c(1, 2, 3, 4, 5),
    x = c(0, 1, 2, 3, 4)
  )

  model = stats::lm(y ~ x, data = df)

  newWmfmModel(
    model = model,
    formula = y ~ x,
    modelType = "lm",
    data = df,
    researchQuestion = "Does x help explain y?",
    equations = "y = 1.00 + 1.00 * x",
    explanation = paste(
      "The fitted model shows that y increases by about one unit",
      "for each one-unit increase in x."
    )
  )
}
