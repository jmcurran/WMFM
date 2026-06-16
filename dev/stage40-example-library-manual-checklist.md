# Stage 40.6 example-library manual validation checklist

## Purpose

Stage 40 reorganised the installed WMFM example library so ordinary classroom examples remain easy to find while developer and regression fixtures are grouped away from the release-mode list.

This checklist records the manual validation that should be done after the Stage 40.5.1 physical file move and path-length repair.

## Expected installed layout

Public and classroom examples should remain direct children of `inst/extdata/examples/`:

- `Course`
- `DiamondsII`
- `DiamondsIII`
- `DiamondsIV`
- `DiamondsUnitChange`
- `Mtcars`
- `Oysters`
- `Quakes`
- `QuakesUnitChange0_1`

Developer-only examples should be grouped under `inst/extdata/examples/developer/`:

- `developer/adjustment/`
- `developer/followups/`
- `developer/scoring-grading/`

Systematic model-grid fixtures should be grouped under the short portable path `inst/extdata/examples/t/grid/`.

## Manual checks after installation

Run these checks from an R session after installing the completed Stage 40 package.

```r
library(WMFM)

publicExamples = listWMFMExamples()
developerExamples = listWMFMExamples(includeTestExamples = TRUE)

stopifnot("Course" %in% publicExamples)
stopifnot("DiamondsII" %in% publicExamples)
stopifnot("Quakes" %in% publicExamples)
stopifnot(!"test-01-G00F" %in% publicExamples)
stopifnot(!"test-SG-1" %in% publicExamples)
stopifnot("test-01-G00F" %in% developerExamples)
stopifnot("test-SG-1" %in% developerExamples)
stopifnot("test-course-follow-up" %in% developerExamples)
```

Then start the app and check:

1. In ordinary release mode, the example selector shows classroom examples and does not show `test-*` fixtures.
2. After developer mode is unlocked, developer examples become available.
3. At least one classroom example loads successfully.
4. At least one moved model-grid fixture loads successfully through developer-mode listing.
5. At least one moved scoring/grading fixture remains available to developer scoring workflows.

## Follow-up work

No additional implementation is expected solely because of the Stage 40 reorganisation if the automated checks and the manual checks above pass.

Possible future polish, outside Stage 40, would be to improve the visual presentation of example metadata in the app rather than only using it for filtering and lookup.
