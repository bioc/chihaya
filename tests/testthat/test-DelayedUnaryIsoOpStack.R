# This tests the DelayedUnaryIsoOpStack saving/loading functionality.
# library(testthat); library(chihaya); source("test-DelayedUnaryIsoOpStack.R")

library(DelayedArray)
X <- DelayedArray(matrix(runif(100), ncol=20))

test_that("DelayedUnaryIsoOpStack works as expected", {
    Z <- log2(X + 10)
    temp <- tempfile(fileext=".h5")
    saveDelayed(Z, temp)

    expect_identical(rhdf5::h5readAttributes(temp, "delayed")$delayed_operation, "unary math")

    expect_identical(as.vector(rhdf5::h5read(temp, "delayed/method")), "log")
    expect_identical(as.vector(rhdf5::h5read(temp, "delayed/base")), 2)
    expect_identical(as.vector(rhdf5::h5read(temp, "delayed/seed/method")), "+")

    expect_identical(as.vector(rhdf5::h5read(temp, "delayed/seed/value")), 10)
    expect_identical(as.vector(rhdf5::h5read(temp, "delayed/seed/side")), "right")

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
    expect_identical(as.vector(rhdf5::h5read(temp, "delayed/method")), "/")
    expect_identical(as.vector(rhdf5::h5read(temp, "delayed/side")), "left")
    expect_identical(as.vector(rhdf5::h5read(temp, "delayed/value")), 5)

    roundtrip <- loadDelayed(temp)
    expect_identical(as.matrix(Z), as.matrix(roundtrip))
    expect_s4_class(roundtrip@seed, "DelayedUnaryIsoOpStack")

    # Trying on the other side with another non-commutative op.
    Z <- X - 10
    temp <- tempfile(fileext=".h5")
    saveDelayed(Z, temp)

    manifest <- rhdf5::h5ls(temp)
    all.paths <- file.path(manifest$group, manifest$name)
    expect_identical(as.vector(rhdf5::h5read(temp, "delayed/method")), "-")
    expect_identical(as.vector(rhdf5::h5read(temp, "delayed/side")), "right")
    expect_identical(as.vector(rhdf5::h5read(temp, "delayed/value")), 10)

    roundtrip <- loadDelayed(temp)
    expect_identical(as.matrix(Z), as.matrix(roundtrip))
    expect_s4_class(roundtrip@seed, "DelayedUnaryIsoOpStack")
})

test_that("DelayedUnaryIsoOpStack works for log", {
    Z <- log(X)
    temp <- tempfile(fileext=".h5")
    saveDelayed(Z, temp)

    expect_identical(as.vector(rhdf5::h5read(temp, "delayed/method")), "log")

    # Works with non-default base.
    Z <- log(X, base=3)
    temp <- tempfile(fileext=".h5")
    saveDelayed(Z, temp)

    expect_identical(as.vector(rhdf5::h5read(temp, "delayed/base")), 3)
    out <- loadDelayed(temp)
    expect_equal(Z, out)
})

test_that("DelayedUnaryIsoOpStack works for logical", {
    Z <- X > 0.2 & TRUE
    temp <- tempfile(fileext=".h5")
    saveDelayed(Z, temp)

    expect_identical(rhdf5::h5readAttributes(temp, "delayed")$delayed_operation, "unary logic")
    expect_identical(as.vector(rhdf5::h5read(temp, "delayed/method")), "&&")
    out <- loadDelayed(temp)
    expect_equal(Z, out)

    # Same for the ||.
    Z <- X < 0.2 | FALSE
    temp <- tempfile(fileext=".h5")
    saveDelayed(Z, temp)

    expect_identical(as.vector(rhdf5::h5read(temp, "delayed/method")), "||") 
    out <- loadDelayed(temp)
    expect_equal(Z, out)

    # Same for !
    Z <- !X
    temp <- tempfile(fileext=".h5")
    saveDelayed(Z, temp)

    expect_identical(as.vector(rhdf5::h5read(temp, "delayed/method")), "!") 
    out <- loadDelayed(temp)
    expect_equal(Z, out)
})

test_that("DelayedUnaryIsoOpStack works for unary arithmetic", {
    Z <- -X
    temp <- tempfile(fileext=".h5")
    saveDelayed(Z, temp)

    expect_identical(as.vector(rhdf5::h5read(temp, "delayed/method")), "-")
    expect_identical(as.vector(rhdf5::h5read(temp, "delayed/side")), "none")

    out <- loadDelayed(temp)
    expect_equal(out, Z)

    Z <- +X
    temp <- tempfile(fileext=".h5")
    saveDelayed(Z, temp)

    expect_identical(as.vector(rhdf5::h5read(temp, "delayed/method")), "+")
    expect_identical(as.vector(rhdf5::h5read(temp, "delayed/side")), "none")

    out <- loadDelayed(temp)
    expect_equal(out, Z)
})

test_that("DelayedUnaryIsoOpStack works for other unary operations", {
    Z <- !X
    temp <- tempfile(fileext=".h5")
    saveDelayed(Z, temp)

    expect_identical(as.vector(rhdf5::h5read(temp, "delayed/method")), "!")

    out <- loadDelayed(temp)
    expect_equal(out, Z)

    # Trying with something that gets renamed.
    Z <- is.nan(X)
    temp <- tempfile(fileext=".h5")
    saveDelayed(Z, temp)
    expect_identical(as.vector(rhdf5::h5read(temp, "delayed/method")), "is_nan")

    out <- loadDelayed(temp)
    expect_equal(out, Z)
})

test_that("DelayedUnaryIsoOpStack works for Math2", {
    Z <- round(X)
    temp <- tempfile(fileext=".h5")
    saveDelayed(Z, temp)

    manifest <- rhdf5::h5ls(temp)
    all.paths <- file.path(manifest$group, manifest$name)
    expect_identical(as.vector(rhdf5::h5read(temp, "delayed/method")), "round")
    expect_identical(as.vector(rhdf5::h5read(temp, "delayed/digits")), 0L)

    roundtrip <- loadDelayed(temp)
    expect_identical(as.matrix(Z), as.matrix(roundtrip))
    expect_s4_class(roundtrip@seed, "DelayedUnaryIsoOpStack")
})
