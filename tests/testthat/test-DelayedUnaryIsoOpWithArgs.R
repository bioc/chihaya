# This tests the DelayedUnaryIsoOpWithArgs saving/loading functionality.
# library(testthat); library(DelayedArraySaver); source("test-DelayedUnaryIsoOpWithArgs.R")

library(DelayedArray)
X <- DelayedArray(matrix(runif(100), ncol=20))

test_that("DelayedUnaryIsoOpWithArgs works as expected", {
    Z <- X - runif(5)
    temp <- tempfile(fileext=".h5")
    saveDelayed(Z, temp)

    expect_identical(as.vector(rhdf5::h5read(temp, "delayed/operation")), "-")
    expect_identical(as.vector(rhdf5::h5read(temp, "delayed/left_along")), integer(0))
    expect_identical(as.vector(rhdf5::h5read(temp, "delayed/right_along")), 1L)

    roundtrip <- loadDelayed(temp)
    expect_identical(as.matrix(Z), as.matrix(roundtrip))
    expect_s4_class(roundtrip@seed, "DelayedUnaryIsoOpWithArgs")
})

test_that("DelayedUnaryIsoOpWithArgs handles side-ness correctly", {
    Z <- runif(5) / X
    temp <- tempfile(fileext=".h5")
    saveDelayed(Z, temp)

    expect_identical(as.vector(rhdf5::h5read(temp, "delayed/operation")), "/")
    expect_identical(as.vector(rhdf5::h5read(temp, "delayed/right_along")), integer(0))
    expect_identical(as.vector(rhdf5::h5read(temp, "delayed/left_along")), 1L)

    roundtrip <- loadDelayed(temp)
    expect_identical(as.matrix(Z), as.matrix(roundtrip))
    expect_s4_class(roundtrip@seed, "DelayedUnaryIsoOpWithArgs")
})
