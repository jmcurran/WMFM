test_that("listPackagesWithDatasetMetadata finds packages with dataset metadata", {
  libPath = withr::local_tempdir()

  pkgWithData = file.path(libPath, "pkgWithData")
  dir.create(file.path(pkgWithData, "Meta"), recursive = TRUE)
  writeLines(
    c(
      "Package: zPkgWithData",
      "Version: 0.0.1"
    ),
    file.path(pkgWithData, "DESCRIPTION")
  )
  saveRDS(data.frame(Item = "example"), file.path(pkgWithData, "Meta", "data.rds"))

  pkgWithoutData = file.path(libPath, "pkgWithoutData")
  dir.create(pkgWithoutData, recursive = TRUE)
  writeLines(
    c(
      "Package: aPkgWithoutData",
      "Version: 0.0.1"
    ),
    file.path(pkgWithoutData, "DESCRIPTION")
  )

  expect_identical(
    listPackagesWithDatasetMetadata(libPath),
    "zPkgWithData"
  )
})

test_that("listPackagesWithDatasetMetadata ignores invalid library paths and malformed packages", {
  libPath = withr::local_tempdir()

  pkgWithoutDescription = file.path(libPath, "pkgWithoutDescription")
  dir.create(file.path(pkgWithoutDescription, "Meta"), recursive = TRUE)
  saveRDS(data.frame(Item = "example"), file.path(pkgWithoutDescription, "Meta", "data.rds"))

  expect_identical(
    listPackagesWithDatasetMetadata(c(file.path(libPath, "missing"), libPath)),
    character(0)
  )
})

test_that("getPackageNameFromDescription returns an empty string for invalid packages", {
  packageDir = withr::local_tempdir()

  expect_identical(getPackageNameFromDescription(packageDir), "")

  writeLines("Version: 0.0.1", file.path(packageDir, "DESCRIPTION"))
  expect_identical(getPackageNameFromDescription(packageDir), "")
})
