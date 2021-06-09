# This tests the DelayedUnaryIsoOpWithArgs saving/loading functionality.
# library(testthat); library(DelayedArraySaver); source("test-DelayedUnaryIsoOpWithArgs.R")

library(DelayedArray)
X <- DelayedArray(matrix(runif(100), ncol=20))

test_that("DelayedUnaryIsoOpWithArgs works as expected", {
    Z <- X - runif(5)
    temp <- tempfile(fileext=".h5")
    saveDelayed(Z, temp)

    expect_identical(rhdf5::h5readAttributes(temp, "delayed")$delayed_type[2], "unary isometric")

    expect_identical(as.vector(rhdf5::h5read(temp, "delayed/operation")), "-")
    expect_identical(length(rhdf5::h5read(temp, "delayed/parameters/value")), nrow(X))
    expect_identical(as.vector(rhdf5::h5read(temp, "delayed/parameters/along")), 1L)
    expect_identical(as.vector(rhdf5::h5read(temp, "delayed/parameters/side")), "right")

    roundtrip <- loadDelayed(temp)
    expect_identical(as.matrix(Z), as.matrix(roundtrip))
    expect_s4_class(roundtrip@seed, "DelayedUnaryIsoOpWithArgs")
})

test_that("DelayedUnaryIsoOpWithArgs handles side-ness correctly", {
    Z <- runif(5) / X
    temp <- tempfile(fileext=".h5")
    saveDelayed(Z, temp)

    expect_identical(as.vector(rhdf5::h5read(temp, "delayed/operation")), "/")
    expect_identical(length(as.vector(rhdf5::h5read(temp, "delayed/parameters/value"))), nrow(X))
    expect_identical(as.vector(rhdf5::h5read(temp, "delayed/parameters/along")), 1L)
    expect_identical(as.vector(rhdf5::h5read(temp, "delayed/parameters/side")), "left")

    roundtrip <- loadDelayed(temp)
    expect_identical(as.matrix(Z), as.matrix(roundtrip))
    expect_s4_class(roundtrip@seed, "DelayedUnaryIsoOpWithArgs")
})

test_that("DelayedUnaryIsoOpWithArgs handles repeated operations correctly (same side)", {
    # Same side
    Z <- (X + runif(5)) + runif(5)
    temp <- tempfile(fileext=".h5")
    saveDelayed(Z, temp)

    expect_identical(as.vector(rhdf5::h5read(temp, "delayed/operation")), "+")
    expect_identical(length(as.vector(rhdf5::h5read(temp, "delayed/parameters/value"))), nrow(X))
    expect_identical(as.vector(rhdf5::h5read(temp, "delayed/parameters/along")), 1L)
    expect_identical(as.vector(rhdf5::h5read(temp, "delayed/parameters/side")), "right")

    expect_identical(as.vector(rhdf5::h5read(temp, "delayed/seed/operation")), "+")
    expect_identical(length(as.vector(rhdf5::h5read(temp, "delayed/seed/parameters/value"))), nrow(X))
    expect_identical(as.vector(rhdf5::h5read(temp, "delayed/seed/parameters/along")), 1L)
    expect_identical(as.vector(rhdf5::h5read(temp, "delayed/seed/parameters/side")), "right")

    roundtrip <- loadDelayed(temp)
    expect_identical(as.matrix(Z), as.matrix(roundtrip))
    expect_s4_class(roundtrip@seed, "DelayedUnaryIsoOpWithArgs")
})

test_that("DelayedUnaryIsoOpWithArgs handles repeated operations correctly (opposite sides)", {
    # Same side
    Z <- runif(5) + (X + runif(5))
    temp <- tempfile(fileext=".h5")
    saveDelayed(Z, temp)

    expect_identical(as.vector(rhdf5::h5read(temp, "delayed/operation")), "+")
    expect_identical(length(as.vector(rhdf5::h5read(temp, "delayed/parameters/value"))), nrow(X))
    expect_identical(as.vector(rhdf5::h5read(temp, "delayed/parameters/along")), 1L)
    expect_identical(as.vector(rhdf5::h5read(temp, "delayed/parameters/side")), "left")

    expect_identical(as.vector(rhdf5::h5read(temp, "delayed/seed/operation")), "+")
    expect_identical(length(as.vector(rhdf5::h5read(temp, "delayed/seed/parameters/value"))), nrow(X))
    expect_identical(as.vector(rhdf5::h5read(temp, "delayed/seed/parameters/along")), 1L)
    expect_identical(as.vector(rhdf5::h5read(temp, "delayed/seed/parameters/side")), "right")

    roundtrip <- loadDelayed(temp)
    expect_identical(as.matrix(Z), as.matrix(roundtrip))
    expect_s4_class(roundtrip@seed, "DelayedUnaryIsoOpWithArgs")
})
