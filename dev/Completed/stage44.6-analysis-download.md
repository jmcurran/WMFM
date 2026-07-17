# Stage 44.6: reproducible analysis download

Stage 44.6 adds the first user-facing delivery path for the deterministic
analysis recipe.

## Decisions

- The analysis recipe continues to be created when the model is fitted.
- The downloadable artifact is generated only when the user requests it.
- Package datasets produce a single `wmfm_analysis.qmd` file.
- Uploaded datasets produce a ZIP file containing `wmfm_analysis.qmd` and a
  portable `data/analysis_data.csv` copy of the data actually used by WMFM.
- The generated document uses relative paths and does not expose the Shiny
  upload path or the user's original absolute file path.
- The first UI entry point is a `Download reproducible analysis` button on the
  Fitted Model tab.
- Rendering the Quarto document to HTML, Word or PDF remains outside this
  stage. The downloaded `.qmd` is readable and executable independently.

## Compatibility boundary

The portable CSV copy deliberately represents the data frame used by WMFM.
This makes uploads reproducible even when the original input was an RDA or a
text file whose separator was selected interactively. It does not claim to
reproduce every property of the original file container.
