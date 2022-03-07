# This tests the DelayedAperm saving/loading functionality.
# library(testthat); library(chihaya); source("test-DelayedAperm.R")

library(DelayedArray)
Y <- DelayedArray(matrix(runif(100), ncol=20))

test_that("DelayedAperm works along rows", {
    X <- DelayedArray(matrix(runif(100), ncol=20))
    Z <- t(X)
    temp <- tempfile(file=".h5")
    saveDelayed(Z, temp)

    expect_identical(rhdf5::h5readAttributes(temp, "delayed")$delayed_operation, "transpose")

    manifest <- rhdf5::h5ls(temp)
    all.paths <- file.path(manifest$group, manifest$name)
    expect_true(any(grepl("delayed/seed", all.paths)))
    expect_identical(as.vector(rhdf5::h5read(temp, "delayed/permutation")), 1:0)

    roundtrip <- loadDelayed(temp)
    expect_identical(as.matrix(Z), as.matrix(roundtrip))
    expect_s4_class(roundtrip@seed, "DelayedAperm")
})

test_that("DelayedAperm works for 3D arrays", {
    A <- DelayedArray(array(runif(100), c(10, 5, 4)))
    perm <- c(3L, 1L, 2L)
    Z <- aperm(A, perm)
    temp <- tempfile(file=".h5")
    saveDelayed(Z, temp)

    manifest <- rhdf5::h5ls(temp)
    all.paths <- file.path(manifest$group, manifest$name)
    expect_true(any(grepl("delayed/seed", all.paths)))
    expect_identical(as.vector(rhdf5::h5read(temp, "delayed/permutation")), perm - 1L)

    roundtrip <- loadDelayed(temp)
    expect_identical(as.array(Z), as.array(roundtrip))
    expect_s4_class(roundtrip@seed, "DelayedAperm")
})
