copyTestOutput = function() {
  txt = paste(
    capture.output(devtools::test()),
    collapse = "\n"
  )

  clipr::write_clip(txt)
  invisible(txt)
}
