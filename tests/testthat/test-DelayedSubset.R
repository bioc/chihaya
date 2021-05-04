# This tests the DelayedSubset saving/loading functionality.
# library(testthat); library(DelayedArraySaver); source("test-DelayedSubset.R")

library(DelayedArray)
X <- DelayedArray(matrix(runif(100), ncol=20))

test_that("DelayedSubset works when all indices are supplied", {
    Y <- X[1:2,3:5]
    temp <- tempfile(fileext=".h5")
    saveDelayed(Y, temp)

    manifest <- rhdf5::h5ls(temp)
    expect_identical(as.vector(rhdf5::h5read(temp, "delayed/index/1")), 1:2)
    expect_identical(as.vector(rhdf5::h5read(temp, "delayed/index/2")), 3:5)

    roundtrip <- loadDelayed(temp)
    expect_identical(as.matrix(Y), as.matrix(roundtrip))
    expect_s4_class(roundtrip@seed, "DelayedSubset")

    # Trying with out-of-order indices.
    Y <- X[c(1,3,5),c(2,8,6,4)]
    temp <- tempfile(fileext=".h5")
    saveDelayed(Y, temp)
    roundtrip <- loadDelayed(temp)
    expect_identical(as.matrix(Y), as.matrix(roundtrip))
})

test_that("DelayedSubset works when only one index is supplied", {
    Y <- X[1:2,] 
    temp <- tempfile(fileext=".h5")
    saveDelayed(Y, temp)

    manifest <- rhdf5::h5ls(temp)
    all.paths <- file.path(manifest$group, manifest$name)
    expect_true(any(grepl("delayed/index/1", all.paths)))
    expect_false(any(grepl("delayed/index/2", all.paths)))

    roundtrip <- loadDelayed(temp)
    expect_identical(as.matrix(Y), as.matrix(roundtrip))
    expect_s4_class(roundtrip@seed, "DelayedSubset")
})