# This tests the DelayedUnaryIsoOpWithArgs saving/loading functionality.
# library(testthat); library(chihaya); source("test-DelayedUnaryIsoOpWithArgs.R")

library(DelayedArray)
X <- DelayedArray(matrix(runif(100), ncol=20))

test_that("DelayedUnaryIsoOpWithArgs works as expected", {
    Z <- X - runif(5)
    temp <- tempfile(fileext=".h5")
    saveDelayed(Z, temp)

    expect_identical(rhdf5::h5readAttributes(temp, "delayed")$delayed_operation, "unary arithmetic")

    expect_identical(as.vector(rhdf5::h5read(temp, "delayed/method")), "-")
    expect_identical(length(rhdf5::h5read(temp, "delayed/value")), nrow(X))
    expect_identical(as.vector(rhdf5::h5read(temp, "delayed/along")), 0L)
    expect_identical(as.vector(rhdf5::h5read(temp, "delayed/side")), "right")

    roundtrip <- loadDelayed(temp)
    expect_identical(as.matrix(Z), as.matrix(roundtrip))
    expect_s4_class(roundtrip@seed, "DelayedUnaryIsoOpWithArgs")
})

test_that("DelayedUnaryIsoOpWithArgs handles logical renaming", {
    Z <- X & runif(5) > 5
    temp <- tempfile(fileext=".h5")
    saveDelayed(Z, temp)

    expect_identical(rhdf5::h5readAttributes(temp, "delayed")$delayed_operation, "unary logic")
    expect_identical(as.vector(rhdf5::h5read(temp, "delayed/method")), "&&")

    roundtrip <- loadDelayed(temp)
    expect_identical(as.matrix(Z), as.matrix(roundtrip))
    expect_s4_class(roundtrip@seed, "DelayedUnaryIsoOpWithArgs")
})

test_that("DelayedUnaryIsoOpWithArgs handles side-ness correctly", {
    Z <- runif(5) / X
    temp <- tempfile(fileext=".h5")
    saveDelayed(Z, temp)

    expect_identical(as.vector(rhdf5::h5read(temp, "delayed/method")), "/")
    expect_identical(length(as.vector(rhdf5::h5read(temp, "delayed/value"))), nrow(X))
    expect_identical(as.vector(rhdf5::h5read(temp, "delayed/along")), 0L)
    expect_identical(as.vector(rhdf5::h5read(temp, "delayed/side")), "left")

    roundtrip <- loadDelayed(temp)
    expect_identical(as.matrix(Z), as.matrix(roundtrip))
    expect_s4_class(roundtrip@seed, "DelayedUnaryIsoOpWithArgs")
})

test_that("DelayedUnaryIsoOpWithArgs handles repeated operations correctly (same side)", {
    # Same side
    Z <- (X + runif(5)) + runif(5)
    temp <- tempfile(fileext=".h5")
    saveDelayed(Z, temp)

    expect_identical(as.vector(rhdf5::h5read(temp, "delayed/method")), "+")
    expect_identical(length(as.vector(rhdf5::h5read(temp, "delayed/value"))), nrow(X))
    expect_identical(as.vector(rhdf5::h5read(temp, "delayed/along")), 0L)
    expect_identical(as.vector(rhdf5::h5read(temp, "delayed/side")), "right")

    expect_identical(as.vector(rhdf5::h5read(temp, "delayed/seed/method")), "+")
    expect_identical(length(as.vector(rhdf5::h5read(temp, "delayed/seed/value"))), nrow(X))
    expect_identical(as.vector(rhdf5::h5read(temp, "delayed/seed/along")), 0L)
    expect_identical(as.vector(rhdf5::h5read(temp, "delayed/seed/side")), "right")

    roundtrip <- loadDelayed(temp)
    expect_identical(as.matrix(Z), as.matrix(roundtrip))
    expect_s4_class(roundtrip@seed, "DelayedUnaryIsoOpWithArgs")
})

test_that("DelayedUnaryIsoOpWithArgs handles repeated operations correctly (opposite sides)", {
    # Same side
    Z <- runif(5) + (X + runif(5))
    temp <- tempfile(fileext=".h5")
    saveDelayed(Z, temp)

    expect_identical(as.vector(rhdf5::h5read(temp, "delayed/method")), "+")
    expect_identical(length(as.vector(rhdf5::h5read(temp, "delayed/value"))), nrow(X))
    expect_identical(as.vector(rhdf5::h5read(temp, "delayed/along")), 0L)
    expect_identical(as.vector(rhdf5::h5read(temp, "delayed/side")), "left")

    expect_identical(as.vector(rhdf5::h5read(temp, "delayed/seed/method")), "+")
    expect_identical(length(as.vector(rhdf5::h5read(temp, "delayed/seed/value"))), nrow(X))
    expect_identical(as.vector(rhdf5::h5read(temp, "delayed/seed/along")), 0L)
    expect_identical(as.vector(rhdf5::h5read(temp, "delayed/seed/side")), "right")

    roundtrip <- loadDelayed(temp)
    expect_identical(as.matrix(Z), as.matrix(roundtrip))
    expect_s4_class(roundtrip@seed, "DelayedUnaryIsoOpWithArgs")
})
