# Stage 47.9.2.1: Evaluation test fixture and archive cleanup repair

Stage 47.9.2 introduced an end-to-end `runModel()` test using the Course model
`Exam ~ Attend + Test`. The test selected `Attend` from
`getStats20xExamTestData()`, but that reduced fixture deliberately contains only
`Exam`, `Test`, and `Gender`.

This repair uses the complete packaged STATS20x fixture for the end-to-end test,
so the test exercises the intended Course model without simulated data.

The stage runner also now removes the consumed change-set archive after a
successful run regardless of whether files were installed explicitly by the
runner or previously installed by the `runStage` wrapper.
