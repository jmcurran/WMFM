makeDiagnoseScoresFixture = function() {
    scores = list(
        deterministic = list(
            list(factualScore = 0),
            list(factualScore = 0),
            list(factualScore = 0)
        ),
        llm = list(
            list(factualScore = 2),
            list(factualScore = 2),
            list(factualScore = 2)
        )
    )

    structure(
        list(
            scores = scores,
            methods = c("deterministic", "llm"),
            runIds = c(1, 2, 3)
        ),
        class = "wmfmScores"
    )
}
