# This tests the DelayedSetDimnames saving/loading functionality.
# library(testthat); library(DelayedArraySaver); source("test-DelayedSetDimnames.R")

library(DelayedArray)
X <- DelayedArray(matrix(runif(100), ncol=20))

test_that("DelayedSetDimnames works as expected (colnames only)", {
    Z <- X
    colnames(Z) <- LETTERS[1:20]
    temp <- tempfile(file=".h5")
    saveDelayed(Z, temp)

    expect_identical(rhdf5::h5readAttributes(temp, "delayed")$delayed_type[2], "dimnames")

    manifest <- rhdf5::h5ls(temp)
    all.paths <- file.path(manifest$group, manifest$name)
    expect_false(any(grepl("delayed/dimnames/1", all.paths)))
    expect_identical(as.vector(rhdf5::h5read(temp, "delayed/dimnames/2")), LETTERS[1:20])

    roundtrip <- loadDelayed(temp)
    expect_identical(as.matrix(Z), as.matrix(roundtrip))
    expect_s4_class(roundtrip@seed, "DelayedSetDimnames")
})

test_that("DelayedSetDimnames works as expected (rownames only)", {
    Z <- X
    rownames(Z) <- letters[1:5]
    temp <- tempfile(file=".h5")
    saveDelayed(Z, temp)

    manifest <- rhdf5::h5ls(temp)
    all.paths <- file.path(manifest$group, manifest$name)
    expect_false(any(grepl("delayed/dimnames/2", all.paths)))
    expect_identical(as.vector(rhdf5::h5read(temp, "delayed/dimnames/1")), letters[1:5])

    roundtrip <- loadDelayed(temp)
    expect_identical(as.matrix(Z), as.matrix(roundtrip))
    expect_s4_class(roundtrip@seed, "DelayedSetDimnames")
})

test_that("DelayedSetDimnames works as expected (both sets of names)", {
    Z <- X
    dimnames(Z) <- list(letters[1:5], LETTERS[1:20])
    temp <- tempfile(file=".h5")
    saveDelayed(Z, temp)

    expect_identical(as.vector(rhdf5::h5read(temp, "delayed/dimnames/1")), letters[1:5])
    expect_identical(as.vector(rhdf5::h5read(temp, "delayed/dimnames/2")), LETTERS[1:20])

    roundtrip <- loadDelayed(temp)
    expect_identical(as.matrix(Z), as.matrix(roundtrip))
    expect_s4_class(roundtrip@seed, "DelayedSetDimnames")
})
