# This tests the behavior of the various HDF5-based seeds.
# library(testthat); library(chihaya); source("test-HDF5ArraySeed.R")

library(HDF5Array)

test_that("HDF5ArraySeeds are saved correctly", {
    X <- writeHDF5Array(matrix(runif(100), ncol=20))
    temp <- tempfile(fileext=".h5")
    saveDelayed(X, temp)

    expect_identical(as.character(rhdf5::h5read(temp, "delayed/file")), as.character(path(X)))

    out <- loadDelayed(temp)
    expect_identical(X, out)

    # Raises an error if we block its use.
    olda <- allowExternalSeeds(FALSE)
    expect_false(allowExternalSeeds())
    on.exit(allowExternalSeeds(olda))
    expect_error(saveDelayed(X, temp, "foo"), "external reference")
})

test_that("H5SparseMatrixSeeds are saved correctly", {
    x0 <- rsparsematrix(20, 10, 0.1)
    x <- DelayedArray(x0)
    tmp <- tempfile(fileext=".h5")
    saveDelayed(x, tmp, "dummy")

    mat <- H5SparseMatrix(tmp, "dummy")
    temp <- tempfile(fileext=".h5")
    saveDelayed(mat, temp)

    expect_identical(as.character(rhdf5::h5read(temp, "delayed/file")), as.character(path(mat)))

    out <- loadDelayed(temp)
    expect_identical(out, mat)

    # Raises an error if we block its use.
    olda <- allowExternalSeeds(FALSE)
    on.exit(allowExternalSeeds(olda))
    expect_error(saveDelayed(mat, temp, "bar"), "external reference")
})
