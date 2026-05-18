testthat::test_that("renderSafeExplanationHtml escapes unsafe HTML and preserves paragraphs", {
  text = "First paragraph <script>alert('x')</script>.\nLine two.\n\nSecond paragraph <b>bold</b>."

  html = renderSafeExplanationHtml(text = text, zoomLevel = "presentation")
  rendered = as.character(html)

  testthat::expect_match(rendered, "wmfm-explanation-body")
  testthat::expect_match(rendered, "font-size: 1.6rem", fixed = TRUE)
  testthat::expect_match(rendered, "&lt;script&gt;alert\\(&#39;x&#39;\\)&lt;/script&gt;", perl = TRUE)
  testthat::expect_match(rendered, "&lt;b&gt;bold&lt;/b&gt;", fixed = TRUE)
  testthat::expect_match(rendered, "<br/>", fixed = TRUE)
})

testthat::test_that("mapExplanationZoomToFontSize returns default for invalid values", {
  testthat::expect_identical(mapExplanationZoomToFontSize("small"), "0.95rem")
  testthat::expect_identical(mapExplanationZoomToFontSize("normal"), "1.08rem")
  testthat::expect_identical(mapExplanationZoomToFontSize("unknown"), "1rem")
  testthat::expect_identical(mapExplanationZoomToFontSize(NULL), "1rem")
})
