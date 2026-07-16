# Stage 44.3.1: stable package build and installation

## Purpose

Stage 44.3 completed package validation and built the source package, but the stage runner asked `devtools::build()` to place the tarball in the R session temporary directory. That directory was removed when the R process ended, so the following shell step could not install the recorded tarball path.

## Decision

Stage runners must build into a stable project-local temporary directory rather than an R session temporary directory. The runner should:

1. create `.wmfm-build/` in the WMFM package root;
2. build the package into that directory;
3. record and verify the exact tarball path;
4. install that exact tarball; and
5. remove `.wmfm-build/` and the path record only after installation succeeds.

This preserves the exact-build installation rule while keeping the tarball available across separate `Rscript` and `R CMD INSTALL` processes.
