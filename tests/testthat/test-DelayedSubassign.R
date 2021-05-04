# This tests the DelayedSubassign saving/loading functionality.
# library(testthat); library(DelayedArraySaver); source("test-DelayedSubassign.R")

library(DelayedArray)
X <- DelayedArray(matrix(runif(100), ncol=20))

test_that("DelayedSubassign works when all indices are supplied", {
    X[1:2,3:5] <- matrix(-runif(6), ncol=3)
    temp <- tempfile(fileext=".h5")
    saveDelayed(X, temp)

    manifest <- rhdf5::h5ls(temp)
    all.paths <- file.path(manifest$group, manifest$name)
    expect_true(any(grepl("delayed/index/1", all.paths)))
    expect_true(any(grepl("delayed/index/2", all.paths)))

    roundtrip <- loadDelayed(temp)
    expect_identical(as.matrix(X), as.matrix(roundtrip))
    expect_s4_class(roundtrip@seed, "DelayedSubassign")
})

test_that("DelayedSubassign works when only one index is supplied", {
    X[1:2,] <- matrix(-runif(2*ncol(X)), ncol=ncol(X))
    temp <- tempfile(fileext=".h5")
    saveDelayed(X, temp)

    manifest <- rhdf5::h5ls(temp)
    all.paths <- file.path(manifest$group, manifest$name)
    expect_true(any(grepl("delayed/index/1", all.paths)))
    expect_false(any(grepl("delayed/index/2", all.paths)))

    roundtrip <- loadDelayed(temp)
    expect_identical(as.matrix(X), as.matrix(roundtrip))
    expect_s4_class(roundtrip@seed, "DelayedSubassign")
})

test_that("DelayedSubassign works when the replacement is a DelayedArray", {
    X[1:2,3:5] <- DelayedArray(matrix(-runif(6), ncol=3)) + 1
    temp <- tempfile(fileext=".h5")
    saveDelayed(X, temp)

    manifest <- rhdf5::h5ls(temp)
    all.paths <- file.path(manifest$group, manifest$name)
    expect_true(any(grepl("delayed/index/1", all.paths)))
    expect_true(any(grepl("delayed/index/2", all.paths)))

    roundtrip <- loadDelayed(temp)
    expect_identical(as.matrix(X), as.matrix(roundtrip))
    expect_s4_class(roundtrip@seed, "DelayedSubassign")
})


