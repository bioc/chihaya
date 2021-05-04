# This tests various odds and ends not covered by other tests.
# library(testthat); library(DelayedArraySaver); source("test-other.R")

library(Matrix)

test_that("saving of a CsparseMatrix works correctly", {
    x <- DelayedArray(rsparsematrix(20, 10, 0.1))
    tmp <- tempfile(fileext=".h5")
    saveDelayed(x, tmp)

    # Check that it follows H5SparseMatrix conventions.
    library(HDF5Array)
    stuff <- H5SparseMatrix(tmp, "delayed")
    expect_identical(unname(as.matrix(stuff)), unname(as.matrix(x)))

    out <- loadDelayed(tmp)
    expect_identical(x, out)

    # Supports dimnames.
    dimnames(x) <- list(LETTERS[1:20], letters[1:10])
    tmp <- tempfile(fileext=".h5")
    saveDelayed(x, tmp)

    out <- loadDelayed(tmp)
    expect_identical(x, out)
})
