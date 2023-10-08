# This tests the behavior of the various HDF5-based seeds.
# library(testthat); library(chihaya); source("test-ConstantArraySeed.R")

library(DelayedArray)

test_that("ConstantArrays are saved correctly", {
    # Behaves correctly for logical values.
    X <- ConstantArray(dim=c(12, 7))
    temp <- tempfile(fileext=".h5")
    saveDelayed(X, temp)

    out <- loadDelayed(temp)
    expect_identical(X, out)

    # Behaves correctly for string values.
    X <- ConstantArray(dim=c(12, 7), value="foo")
    temp <- tempfile(fileext=".h5")
    saveDelayed(X, temp)

    out <- loadDelayed(temp)
    expect_identical(X, out)

    # Behaves correctly for numeric values.
    X <- ConstantArray(dim=c(12, 7), value=2L)
    temp <- tempfile(fileext=".h5")
    saveDelayed(X, temp)

    out <- loadDelayed(temp)
    expect_identical(X, out)
})

test_that("ConstantArrays are still saved correctly after some deep nesting", {
    X <- ConstantArray(dim=c(12, 7), value=23)
    Y <- DelayedArray(matrix(runif(60), nrow=12))
    Z <- cbind(X, Y)

    temp <- tempfile(fileext=".h5")
    saveDelayed(Z, temp)

    out <- loadDelayed(temp)
    expect_identical(Z, out)
})

test_that("ConstantArrays behave correctly with NAs", {
    X <- ConstantArray(dim=c(12, 7), value=NA)
    temp <- tempfile(fileext=".h5")
    saveDelayed(X, temp)

    out <- loadDelayed(temp)
    expect_identical(X, out)

    # Trying a non-Default NA.
    X <- ConstantArray(dim=c(12, 7), value=1.2)
    temp <- tempfile(fileext=".h5")
    saveDelayed(X, temp)

    library(rhdf5)
    (function() {
        fhandle <- H5Fopen(temp)
        on.exit(H5Fclose(fhandle), add=TRUE)
        dhandle <- H5Dopen(fhandle, "delayed/value")
        on.exit(H5Dclose(dhandle), add=TRUE)
        h5writeAttribute(1.2, dhandle, "missing_placeholder")
    })()

    out <- loadDelayed(temp)
    expect_identical(ConstantArray(c(12, 7), value=NA_real_), out)
})
