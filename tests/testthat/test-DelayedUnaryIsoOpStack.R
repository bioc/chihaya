# This tests the DelayedUnaryIsoOpStack saving/loading functionality.
# library(testthat); library(DelayedArraySaver); source("test-DelayedUnaryIsoOpStack.R")

library(DelayedArray)
X <- DelayedArray(matrix(runif(100), ncol=20))

test_that("DelayedUnaryIsoOpStack works as expected", {
    Z <- log2(X + 10)
    temp <- tempfile(fileext=".h5")
    saveDelayed(Z, temp)

    manifest <- rhdf5::h5ls(temp)
    all.paths <- file.path(manifest$group, manifest$name)
    expect_identical(as.vector(rhdf5::h5read(temp, "delayed/operations/1/operation")), "+")
    expect_identical(as.vector(rhdf5::h5read(temp, "delayed/operations/1/side")), "left")
    expect_identical(as.vector(rhdf5::h5read(temp, "delayed/operations/2/operation")), "log2")

    roundtrip <- loadDelayed(temp)
    expect_identical(as.matrix(Z), as.matrix(roundtrip))
    expect_s4_class(roundtrip@seed, "DelayedUnaryIsoOpStack")
})

test_that("DelayedUnaryIsoOpStack works correctly for the operation side", {
    Z <- 5 / X
    temp <- tempfile(fileext=".h5")
    saveDelayed(Z, temp)

    manifest <- rhdf5::h5ls(temp)
    all.paths <- file.path(manifest$group, manifest$name)
    expect_identical(as.vector(rhdf5::h5read(temp, "delayed/operations/1/operation")), "/")
    expect_identical(as.vector(rhdf5::h5read(temp, "delayed/operations/1/side")), "right")

    roundtrip <- loadDelayed(temp)
    expect_identical(as.matrix(Z), as.matrix(roundtrip))
    expect_s4_class(roundtrip@seed, "DelayedUnaryIsoOpStack")

    # Trying on the other side with another non-commutative op.
    Z <- X - 10
    temp <- tempfile(fileext=".h5")
    saveDelayed(Z, temp)

    manifest <- rhdf5::h5ls(temp)
    all.paths <- file.path(manifest$group, manifest$name)
    expect_identical(as.vector(rhdf5::h5read(temp, "delayed/operations/1/operation")), "-")
    expect_identical(as.vector(rhdf5::h5read(temp, "delayed/operations/1/side")), "left")

    roundtrip <- loadDelayed(temp)
    expect_identical(as.matrix(Z), as.matrix(roundtrip))
    expect_s4_class(roundtrip@seed, "DelayedUnaryIsoOpStack")
})

test_that("DelayedUnaryIsoOpStack works for Math2", {
    Z <- round(X)
    temp <- tempfile(fileext=".h5")
    saveDelayed(Z, temp)

    manifest <- rhdf5::h5ls(temp)
    all.paths <- file.path(manifest$group, manifest$name)
    expect_identical(as.vector(rhdf5::h5read(temp, "delayed/operations/1/operation")), "round")
    expect_identical(as.vector(rhdf5::h5read(temp, "delayed/operations/1/digits")), 0L)

    roundtrip <- loadDelayed(temp)
    expect_identical(as.matrix(Z), as.matrix(roundtrip))
    expect_s4_class(roundtrip@seed, "DelayedUnaryIsoOpStack")
})
