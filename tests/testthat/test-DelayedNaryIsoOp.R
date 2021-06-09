# This tests the DelayedNaryIsoOp saving/loading functionality.
# library(testthat); library(DelayedArraySaver); source("test-DelayedNaryIsoOp.R")

library(DelayedArray)
X <- DelayedArray(matrix(runif(100), ncol=5))
Y <- DelayedArray(matrix(runif(100), ncol=5))

test_that("DelayedNaryIsoOp works as expected", {
    Z <- X * Y
    temp <- tempfile(fileext=".h5")
    saveDelayed(Z, temp)

    expect_identical(rhdf5::h5readAttributes(temp, "delayed")$delayed_type[2], "binary isometric")

    manifest <- rhdf5::h5ls(temp)
    all.paths <- file.path(manifest$group, manifest$name)
    expect_true(any(grepl("delayed/seeds/1", all.paths)))
    expect_true(any(grepl("delayed/seeds/2", all.paths)))
    expect_identical(as.vector(rhdf5::h5read(temp, "delayed/operation")), "*")

    roundtrip <- loadDelayed(temp)
    expect_identical(as.matrix(Z), as.matrix(roundtrip))
    expect_s4_class(roundtrip@seed, "DelayedNaryIsoOp")
})

test_that("DelayedAbind works for 3D arrays", {
    A <- DelayedArray(array(runif(100), c(10, 5, 4)))
    B <- DelayedArray(array(runif(100), c(10, 5, 4)))
    Z <- A + B
    temp <- tempfile(file=".h5")
    saveDelayed(Z, temp)

    manifest <- rhdf5::h5ls(temp)
    all.paths <- file.path(manifest$group, manifest$name)
    expect_true(any(grepl("delayed/seeds/1", all.paths)))
    expect_true(any(grepl("delayed/seeds/2", all.paths)))
    expect_identical(as.vector(rhdf5::h5read(temp, "delayed/operation")), "+")

    roundtrip <- loadDelayed(temp)
    expect_identical(as.array(Z), as.array(roundtrip))
    expect_s4_class(roundtrip@seed, "DelayedNaryIsoOp")
})

test_that("DelayedNaryIsoOp works with multiple seeds", {
    AA <- X
    Z <- (X * Y) * AA
    temp <- tempfile(fileext=".h5")
    saveDelayed(Z, temp)

    roundtrip <- loadDelayed(temp)
    expect_identical(as.matrix(Z), as.matrix(roundtrip))
    expect_s4_class(roundtrip@seed, "DelayedNaryIsoOp")
})
