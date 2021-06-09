# This tests the DelayedUnaryIsoOpStack saving/loading functionality.
# library(testthat); library(DelayedArraySaver); source("test-DelayedUnaryIsoOpStack.R")

library(DelayedArray)
X <- DelayedArray(matrix(runif(100), ncol=20))

test_that("DelayedUnaryIsoOpStack works as expected", {
    Z <- log2(X + 10)
    temp <- tempfile(fileext=".h5")
    saveDelayed(Z, temp)

    expect_identical(rhdf5::h5readAttributes(temp, "delayed")$delayed_type[2], "unary isometric")

    expect_identical(as.vector(rhdf5::h5read(temp, "delayed/operation")), "log")
    expect_identical(as.vector(rhdf5::h5read(temp, "delayed/parameters/base")), 2)
    expect_identical(as.vector(rhdf5::h5read(temp, "delayed/seed/operation")), "+")

    info.pack <- rhdf5::h5read(temp, "delayed/seed/parameters")
    expect_identical(as.vector(info.pack$value), 10)
    expect_identical(as.vector(info.pack$side), "right")
    expect_identical(as.vector(info.pack$along), 0L)

    roundtrip <- loadDelayed(temp)
    expect_identical(as.matrix(Z), as.matrix(roundtrip))
    expect_s4_class(roundtrip@seed, "DelayedUnaryIsoOpStack")
})

test_that("DelayedUnaryIsoOpStack works correctly for the Ops side", {
    Z <- 5 / X
    temp <- tempfile(fileext=".h5")
    saveDelayed(Z, temp)

    manifest <- rhdf5::h5ls(temp)
    all.paths <- file.path(manifest$group, manifest$name)
    expect_identical(as.vector(rhdf5::h5read(temp, "delayed/operation")), "/")
    expect_identical(as.vector(rhdf5::h5read(temp, "delayed/parameters/side")), "left")
    expect_identical(as.vector(rhdf5::h5read(temp, "delayed/parameters/along")), 0L)
    expect_identical(as.vector(rhdf5::h5read(temp, "delayed/parameters/value")), 5)

    roundtrip <- loadDelayed(temp)
    expect_identical(as.matrix(Z), as.matrix(roundtrip))
    expect_s4_class(roundtrip@seed, "DelayedUnaryIsoOpStack")

    # Trying on the other side with another non-commutative op.
    Z <- X - 10
    temp <- tempfile(fileext=".h5")
    saveDelayed(Z, temp)

    manifest <- rhdf5::h5ls(temp)
    all.paths <- file.path(manifest$group, manifest$name)
    expect_identical(as.vector(rhdf5::h5read(temp, "delayed/operation")), "-")
    expect_identical(as.vector(rhdf5::h5read(temp, "delayed/parameters/side")), "right")
    expect_identical(as.vector(rhdf5::h5read(temp, "delayed/parameters/along")), 0L)
    expect_identical(as.vector(rhdf5::h5read(temp, "delayed/parameters/value")), 10)

    roundtrip <- loadDelayed(temp)
    expect_identical(as.matrix(Z), as.matrix(roundtrip))
    expect_s4_class(roundtrip@seed, "DelayedUnaryIsoOpStack")
})

test_that("DelayedUnaryIsoOpStack works for log", {
    Z <- log(X)
    temp <- tempfile(fileext=".h5")
    saveDelayed(Z, temp)

    expect_identical(as.vector(rhdf5::h5read(temp, "delayed/operation")), "log")
    expect_identical(as.vector(rhdf5::h5read(temp, "delayed/parameters/base")), exp(1))

    # Works with non-default base.
    Z <- log(X, base=3)
    temp <- tempfile(fileext=".h5")
    saveDelayed(Z, temp)
    out <- loadDelayed(temp)
    expect_equal(Z, out)
})

test_that("DelayedUnaryIsoOpStack works for unary arithmetic", {
    Z <- -X
    temp <- tempfile(fileext=".h5")
    saveDelayed(Z, temp)

    expect_identical(as.vector(rhdf5::h5read(temp, "delayed/operation")), "-")
    expect_identical(as.vector(rhdf5::h5read(temp, "delayed/parameters/side")), "none")

    out <- loadDelayed(temp)
    expect_equal(out, Z)

    Z <- +X
    temp <- tempfile(fileext=".h5")
    saveDelayed(Z, temp)

    expect_identical(as.vector(rhdf5::h5read(temp, "delayed/operation")), "+")
    expect_identical(as.vector(rhdf5::h5read(temp, "delayed/parameters/side")), "none")

    out <- loadDelayed(temp)
    expect_equal(out, Z)
})

test_that("DelayedUnaryIsoOpStack works for other unary operations", {
    Z <- !X
    temp <- tempfile(fileext=".h5")
    saveDelayed(Z, temp)

    expect_identical(as.vector(rhdf5::h5read(temp, "delayed/operation")), "!")

    out <- loadDelayed(temp)
    expect_equal(out, Z)

    # Trying with something that gets renamed.
    Z <- is.na(X)
    temp <- tempfile(fileext=".h5")
    saveDelayed(Z, temp)
    expect_identical(as.vector(rhdf5::h5read(temp, "delayed/operation")), "is_na")

    out <- loadDelayed(temp)
    expect_equal(out, Z)
})

test_that("DelayedUnaryIsoOpStack works for Math2", {
    Z <- round(X)
    temp <- tempfile(fileext=".h5")
    saveDelayed(Z, temp)

    manifest <- rhdf5::h5ls(temp)
    all.paths <- file.path(manifest$group, manifest$name)
    expect_identical(as.vector(rhdf5::h5read(temp, "delayed/operation")), "round")
    expect_identical(as.vector(rhdf5::h5read(temp, "delayed/parameters/digits")), 0L)

    roundtrip <- loadDelayed(temp)
    expect_identical(as.matrix(Z), as.matrix(roundtrip))
    expect_s4_class(roundtrip@seed, "DelayedUnaryIsoOpStack")
})
