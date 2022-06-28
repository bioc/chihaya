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

