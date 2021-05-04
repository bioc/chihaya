# This tests the DelayedAbind saving/loading functionality.
# library(testthat); library(DelayedArraySaver); source("test-DelayedAbind.R")

library(DelayedArray)
X <- DelayedArray(matrix(runif(100), ncol=20))
Y <- DelayedArray(matrix(runif(100), ncol=20))

test_that("DelayedAbind works along rows", {
    Z <- rbind(X, Y)
    temp <- tempfile(file=".h5")
    saveDelayed(Z, temp)

    manifest <- rhdf5::h5ls(temp)
    all.paths <- file.path(manifest$group, manifest$name)
    expect_true(any(grepl("delayed/seeds/1", all.paths)))
    expect_true(any(grepl("delayed/seeds/2", all.paths)))
    expect_identical(as.vector(rhdf5::h5read(temp, "delayed/along")), 1L)

    roundtrip <- loadDelayed(temp)
    expect_identical(as.matrix(Z), as.matrix(roundtrip))
    expect_s4_class(roundtrip@seed, "DelayedAbind")
})

test_that("DelayedAbind works along columns", {
    YY <- DelayedArray(matrix(runif(100), ncol=20)) # throwing in another dataset for some variety.
    Z <- BiocGenerics::cbind(X, Y, YY)
    temp <- tempfile(file=".h5")
    saveDelayed(Z, temp)

    manifest <- rhdf5::h5ls(temp)
    all.paths <- file.path(manifest$group, manifest$name)
    expect_true(any(grepl("delayed/seeds/1", all.paths)))
    expect_true(any(grepl("delayed/seeds/2", all.paths)))
    expect_true(any(grepl("delayed/seeds/3", all.paths)))
    expect_identical(as.vector(rhdf5::h5read(temp, "delayed/along")), 2L)

    roundtrip <- loadDelayed(temp)
    expect_identical(as.matrix(Z), as.matrix(roundtrip))
    expect_s4_class(roundtrip@seed, "DelayedAbind")
})

test_that("DelayedAbind works for 3D arrays", {
    A <- DelayedArray(array(runif(100), c(10, 5, 4)))
    B <- DelayedArray(array(runif(100), c(10, 5, 4)))
    Z <- arbind(A, B)
    temp <- tempfile(file=".h5")
    saveDelayed(Z, temp)

    manifest <- rhdf5::h5ls(temp)
    all.paths <- file.path(manifest$group, manifest$name)
    expect_true(any(grepl("delayed/seeds/1", all.paths)))
    expect_true(any(grepl("delayed/seeds/2", all.paths)))
    expect_identical(as.vector(rhdf5::h5read(temp, "delayed/along")), 1L)

    roundtrip <- loadDelayed(temp)
    expect_identical(as.array(Z), as.array(roundtrip))
    expect_s4_class(roundtrip@seed, "DelayedAbind")
})
