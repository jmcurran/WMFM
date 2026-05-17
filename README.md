# Stage 20.16 changes

This archive accompanies `run_stage20_16.sh`.

The script applies the targeted Stage 20.16 prompt wording fix, then follows the WMFM stage workflow:

1. optionally install package-relative files with `--install-files`
2. apply the prompt wording patch
3. configure R library paths
4. run `devtools::document()`
5. run strict `devtools::test(stop_on_failure = TRUE, stop_on_warning = TRUE)`
6. run strict `devtools::check(args = c("--no-manual", "--ignore-vignettes"), error_on = "note")`
7. bump the final DESCRIPTION version component
8. commit controlled WMFM paths
9. create `stage20_16_completed.zip`
10. build the package
11. install the exact built package
12. create a ChatGPT bundle if the helper exists

The script is fail-fast. It contains no focused testthat test_file calls.
